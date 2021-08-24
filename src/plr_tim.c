#include "angband.h"

#include <assert.h>

/* XXX move to plr_bonus.c */
void plr_bonus_ac(int amt)
{
    int add = amt - plr->bonus_to_a;
    /* magical bonuses to ac do not stack ... take the highest */
    if (add > 0)
    {
        plr->to_a += add;
        plr->dis_to_a += add;
        plr->bonus_to_a += add;
    }
}
void plr_bonus_speed(int amt)
{
    int add = amt - plr->bonus_speed;
    if (add > 0)
    {
        plr->pspeed += add;
        plr->bonus_speed += add;
    }
}

/************************************************************************
 * Default Timers
 ************************************************************************/
static plr_tim_info_ptr _aura_cold(void);
static plr_tim_info_ptr _aura_elec(void);
static plr_tim_info_ptr _aura_fire(void);
static plr_tim_info_ptr _aura_holy(void);
static plr_tim_info_ptr _aura_shards(void);
static plr_tim_info_ptr _berserk(void);
static plr_tim_info_ptr _blessed(void);
static plr_tim_info_ptr _blind(void);
static plr_tim_info_ptr _brand_acid(void);
static plr_tim_info_ptr _brand_elec(void);
static plr_tim_info_ptr _brand_fire(void);
static plr_tim_info_ptr _brand_cold(void);
static plr_tim_info_ptr _brand_pois(void);
static plr_tim_info_ptr _brand_mana(void);
static plr_tim_info_ptr _confused(void);
static plr_tim_info_ptr _cut(void);
static plr_tim_info_ptr _device_power(void);
static plr_tim_info_ptr _enlarge_weapon(void);
static plr_tim_info_ptr _ego_whip(void);
static plr_tim_info_ptr _fast(void);
static plr_tim_info_ptr _giant_strength(void);
static plr_tim_info_ptr _hallucinate(void);
static plr_tim_info_ptr _hero(void);
static plr_tim_info_ptr _im_acid(void);
static plr_tim_info_ptr _im_elec(void);
static plr_tim_info_ptr _im_fire(void);
static plr_tim_info_ptr _im_cold(void);
static plr_tim_info_ptr _infravision(void);
static plr_tim_info_ptr _inv_prot(void);
static plr_tim_info_ptr _invuln(void);
static plr_tim_info_ptr _kutar_expand(void);
static plr_tim_info_ptr _levitation(void);
static plr_tim_info_ptr _light_speed(void);
static plr_tim_info_ptr _magical_armor(void);
static plr_tim_info_ptr _multishadow(void);
static plr_tim_info_ptr _no_spells(void);
static plr_tim_info_ptr _paralyzed(void);
static plr_tim_info_ptr _passwall(void);
static plr_tim_info_ptr _poison(void);
static plr_tim_info_ptr _prot_evil(void);
static plr_tim_info_ptr _prot_good(void);
static plr_tim_info_ptr _regen(void);
static plr_tim_info_ptr _reflect(void);
static plr_tim_info_ptr _res_acid(void);
static plr_tim_info_ptr _res_elec(void);
static plr_tim_info_ptr _res_fire(void);
static plr_tim_info_ptr _res_cold(void);
static plr_tim_info_ptr _res_pois(void);
static plr_tim_info_ptr _res_conf(void);
static plr_tim_info_ptr _res_nether(void);
static plr_tim_info_ptr _res_disen(void);
static plr_tim_info_ptr _res_time(void);
static plr_tim_info_ptr _res_magic(void);
static plr_tim_info_ptr _revenge(void);
static plr_tim_info_ptr _see_invis(void);
static plr_tim_info_ptr _slow(void);
static plr_tim_info_ptr _star_regen(void);
static plr_tim_info_ptr _stealth(void);
static plr_tim_info_ptr _stone_skin(void);
static plr_tim_info_ptr _stun(void);
static plr_tim_info_ptr _superstealth(void);
static plr_tim_info_ptr _sustain(void);
static plr_tim_info_ptr _telepathy(void);
static plr_tim_info_ptr _ult_res(void);
static plr_tim_info_ptr _weaponmastery(void);
static plr_tim_info_ptr _wraith(void);

status_display_t status_display_create(cptr name, cptr abbrev, byte color)
{
    status_display_t d;
    assert(name);
    assert(abbrev);
    d.name = name;
    d.abbrev = abbrev;
    d.color = color;
    return d;
}
plr_tim_info_ptr plr_tim_info_alloc(int id, cptr name)
{
    plr_tim_info_ptr info = malloc(sizeof(plr_tim_info_t));
    memset(info, 0, sizeof(plr_tim_info_t));
    info->id = id;
    info->name = name;
    return info;
}

/************************************************************************
 * Registered Timer Info
 ************************************************************************/
static void _register(int_map_ptr map, plr_tim_info_ptr info)
{
    int_map_add(map, info->id, info);
}
static int_map_ptr _info(void)
{
    static int_map_ptr map = NULL;
    if (!map)
    {
        map = int_map_alloc(free);
        _register(map, _aura_cold());
        _register(map, _aura_elec());
        _register(map, _aura_fire());
        _register(map, _aura_holy());
        _register(map, _aura_shards());
        _register(map, _berserk());
        _register(map, _blessed());
        _register(map, _blind());
        _register(map, _brand_acid());
        _register(map, _brand_elec());
        _register(map, _brand_fire());
        _register(map, _brand_cold());
        _register(map, _brand_pois());
        _register(map, _brand_mana());
        _register(map, _confused());
        _register(map, _cut());
        _register(map, _device_power());
        _register(map, _enlarge_weapon());
        _register(map, _ego_whip());
        _register(map, _fast());
        _register(map, _giant_strength());
        _register(map, _hallucinate());
        _register(map, _hero());
        _register(map, _im_acid());
        _register(map, _im_elec());
        _register(map, _im_fire());
        _register(map, _im_cold());
        _register(map, _infravision());
        _register(map, _inv_prot());
        _register(map, _invuln());
        _register(map, _kutar_expand());
        _register(map, _levitation());
        _register(map, _light_speed());
        _register(map, _magical_armor());
        _register(map, _multishadow());
        _register(map, _no_spells());
        _register(map, _paralyzed());
        _register(map, _passwall());
        _register(map, _poison());
        _register(map, _prot_evil());
        _register(map, _prot_good());
        _register(map, _reflect());
        _register(map, _regen());
        _register(map, _res_acid());
        _register(map, _res_elec());
        _register(map, _res_fire());
        _register(map, _res_cold());
        _register(map, _res_pois());
        _register(map, _res_conf());
        _register(map, _res_nether());
        _register(map, _res_disen());
        _register(map, _res_time());
        _register(map, _res_magic());
        _register(map, _revenge());
        _register(map, _see_invis());
        _register(map, _slow());
        _register(map, _star_regen());
        _register(map, _stealth());
        _register(map, _stone_skin());
        _register(map, _stun());
        _register(map, _superstealth());
        _register(map, _sustain());
        _register(map, _telepathy());
        _register(map, _ult_res());
        _register(map, _weaponmastery());
        _register(map, _wraith());
        /* this file is getting too big ... */
        illusion_register_timers();
        music_register_timers();
        hex_register_timers();
        bless_register_timers();
    }
    return map;
}

static plr_tim_info_ptr _find_info(int id)
{
    plr_tim_info_ptr info = int_map_find(_info(), id);
    assert(info);
    assert(info->id == id);
    return info;
}

plr_tim_info_ptr plr_tim_info_find(int id)
{
    return _find_info(id);
}

void plr_tim_register(plr_tim_info_ptr info)
{
    assert(info);
    assert(info->id);
    assert(info->name);
    assert(info->on_f);
    assert(info->off_f);
    _register(_info(), info);
}

/************************************************************************
 * Hooks
 ************************************************************************/
static void _plr_hook_on(plr_tim_ptr timer)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.timer_on) r->hooks.timer_on(timer);
    if (c->hooks.timer_on) c->hooks.timer_on(timer);
}
static void _plr_hook_off(plr_tim_ptr timer)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.timer_off) r->hooks.timer_off(timer);
    if (c->hooks.timer_off) c->hooks.timer_off(timer);
}

/************************************************************************
 * Timer Queue
 ************************************************************************/
static bool        _tick = FALSE;

static plr_tim_ptr _find(int id)
{
    plr_tim_ptr t;
    for (t = plr->timers; t; t = t->next)
    {
        if (t->id == id) return t;
    }
    return NULL;
}
static int _count(void)
{
    int ct = 0;
    plr_tim_ptr t;
    for (t = plr->timers; t; t = t->next) ct++;
    return ct;
}
static void _off(plr_tim_ptr t)
{
    plr_tim_info_ptr info = _find_info(t->id);
    _plr_hook_off(t);
    info->off_f(t);
    if (disturb_state) disturb(0, 0);
}
static void _add_tail(plr_tim_ptr tim)
{
    plr_tim_ptr t = plr->timers;
    while (t && t->next) t = t->next;
    if (!t) plr->timers = tim;
    else t->next = tim;
}
static void _add_head(plr_tim_ptr tim)
{
    tim->next = plr->timers;
    plr->timers = tim;
}
static bool _add(plr_tim_ptr t)
{
    plr_tim_info_ptr info = _find_info(t->id);
    bool notice;
    _add_head(t); /* before on_f ... cf T_BRAND_FIRE et. al. */
    t->flags |= info->flags;
    plr->redraw |= PR_STATUS;
    notice = info->on_f(t);
    _plr_hook_on(t);
    if (disturb_state) disturb(0, 0);
    return notice;
}
static void _remove(plr_tim_ptr tim)
{
    plr_tim_ptr t;
    plr_tim_ptr p = NULL;
    if (_tick)
    {   /* wait for _cleanup() */
        tim->count = 0;
        return;
    }
    for (t = plr->timers; t; p = t, t = t->next)
    {
        if (t == tim)
        {
            if (!p) plr->timers = t->next;
            else p->next = t->next;
            _off(t);
            free(t);
            plr->redraw |= PR_STATUS;
            break;
        }
    }
}
static plr_tim_ptr _alloc(int id, int count)
{
    plr_tim_ptr t = malloc(sizeof(plr_tim_t));
    memset(t, 0, sizeof(plr_tim_t));
    t->id = id;
    t->count = count;
    return t;
}

/************************************************************************
 * Public
 ************************************************************************/
bool plr_tim_add(int id, int count)
{
    return plr_tim_add_aux(id, count, 0);
}
bool plr_tim_add_aux(int id, int count, int parm)
{
    plr_tim_ptr t;
    assert(count > 0);
    if (count <= 0) return FALSE;
    if (plr->is_dead) return FALSE;
    t = _find(id);
    if (t)
    {
        plr_tim_info_ptr info = _find_info(id);
        if (info->add_f)
            info->add_f(t, count);
        else if (t->flags & TF_IGNORE)
            return FALSE;
        else if (t->flags & TF_AUGMENT)
            t->count += count;
        else
            t->count = MAX(t->count, count);
        t->parm = parm;
        return FALSE;
    }
    t = _alloc(id, count);
    t->parm = parm;
    return _add(t);
}
/* XXX For example, Killing Spree (Mauler) and Spell Reaction (Rage-Mage)
 * should increase T_FAST, which is not a TF_AUGMENT timer (If it were, then
 * repeatedly casting Haste Self would be absurd). */
bool plr_tim_augment(int id, int count)
{
    plr_tim_ptr t;
    assert(count > 0);
    if (count <= 0) return FALSE;
    if (plr->is_dead) return FALSE;
    t = _find(id);
    if (t)
    {
        t->count += count;
        return FALSE;
    }
    t = _alloc(id, count);
    return _add(t);
}
void plr_tim_subtract(int id, int count)
{
    plr_tim_ptr t = _find(id);
    plr_tim_info_ptr info;
    assert(count > 0);
    if (!t) return;
    info = _find_info(id);
    if (info->add_f)
        info->add_f(t, -count);
    else
        t->count = MAX(0, t->count - count);
    if (t->count <= 0 && !(t->flags & TF_LOCKED))
        _remove(t);
}
bool plr_tim_recover(int id, int pct, int min)
{
    plr_tim_ptr t = _find(id);
    int         x, y;
    plr_tim_info_ptr info;
    assert(pct >= 0);
    assert(min >= 0);
    if (!t) return FALSE;
    /* use the best recovery method */
    x = MAX(0, t->count - min);
    y = t->count * pct / 100;
    info = _find_info(id);
    if (info->add_f)
    {
        int c = MIN(x, y);
        int d = c - t->count;
        info->add_f(t, d);
    }
    else
        t->count = MIN(x, y);
    if (t->count <= 0 && !(t->flags & TF_LOCKED))
        _remove(t);
    return TRUE;
}
bool plr_tim_remove(int id)
{
    plr_tim_ptr t = _find(id);
    plr_tim_info_ptr info;
    if (!t) return FALSE;
    info = _find_info(id);
    if (info->add_f)
        info->add_f(t, -t->count); /* T_STUN and !Curing */
    _remove(t);
    return TRUE;
}
void plr_tim_lock(int id)
{
    plr_tim_ptr t = _find(id);
    if (t) t->flags |= TF_LOCKED;
    else
    {
        t = _alloc(id, 0);
        t->flags |= TF_LOCKED;
        _add(t);
    }
}
void plr_tim_unlock(int id)
{
    plr_tim_ptr t = _find(id);
    if (!t) return;
    t->flags &= ~TF_LOCKED;
    if (t->count <= 0)
        _remove(t);
}
static void _cleanup(void)
{
    plr_tim_ptr t = plr->timers, p = NULL;
    while (t)
    {
        if (t->count <= 0 && !(t->flags & TF_LOCKED))
        {
            plr_tim_ptr x = t;
            if (!p) plr->timers = t->next;
            else p->next = t->next;
            _off(t);
            t = t->next;
            free(x);
            plr->redraw |= PR_STATUS;
        }
        else
        {
            p = t;
            t = t->next;
        }
    }
}
void plr_tim_tick(void)
{
    plr_tim_ptr t;
    if (plr->is_dead) return;
    _tick = TRUE;
    for (t = plr->timers; t; t = t->next)
    {
        if (t->count > 0 && !(t->flags & TF_FAST_TICK))
        {
            plr_tim_info_ptr info = _find_info(t->id);
            if (info->tick_f)
            {
                info->tick_f(t);
                if (plr->is_dead) return;
            }
            else
                t->count--;
        }
    }
    _cleanup();
    if (plr->wizard) plr->redraw |= PR_STATUS;
    _tick = FALSE;
}
void plr_tim_fast_tick(void)
{
    plr_tim_ptr t;
    if (plr->is_dead) return;
    _tick = TRUE;
    for (t = plr->timers; t; t = t->next)
    {
        if (t->count > 0 && (t->flags & TF_FAST_TICK))
        {
            plr_tim_info_ptr info = _find_info(t->id);
            if (info->tick_f)
            {
                info->tick_f(t);
                if (plr->is_dead) return;
            }
            else
                t->count--;
        }
    }
    _cleanup();
    if (plr->wizard) plr->redraw |= PR_STATUS;
    _tick = FALSE;
}
void plr_tim_clear(void)
{
    while (plr->timers)
    {
        plr_tim_ptr x = plr->timers;
        plr->timers = plr->timers->next;
        free(x);
    }
    plr->redraw |= PR_STATUS;
}
bool plr_tim_dispel(void)
{
    bool notice = FALSE;
    plr_tim_ptr t = plr->timers, p = NULL;
    while (t)
    {
        if (!(t->flags & (TF_NO_DISPEL | TF_LOCKED)))
        {
            plr_tim_ptr x = t;
            if (!p) plr->timers = t->next;
            else p->next = t->next;
            _off(t);
            t = t->next;
            free(x);
            notice = TRUE;
            plr->redraw |= PR_STATUS;
        }
        else
        {
            plr_tim_info_ptr info = _find_info(t->id);
            if (info->dispel_f)
                info->dispel_f(t);
            p = t;
            t = t->next;
        }
    }
    return notice;
}
bool plr_tim_disenchant(void)
{
    plr_tim_ptr t;
    int tot = 0, n;
    for (t = plr->timers; t; t = t->next)
    {
        if (t->flags & TF_NO_DISPEL) continue;
        if (t->flags & TF_LOCKED) continue;
        tot++;
    }
    if (!tot) return FALSE;
    n = randint0(tot);
    for (t = plr->timers; t; t = t->next)
    {
        if (t->flags & TF_NO_DISPEL) continue;
        if (t->flags & TF_LOCKED) continue;
        n--;
        if (n < 0)
        {
             _remove(t);
             return TRUE;
        }
    }
    return FALSE;
}
bool plr_tim_find(int id)
{
    plr_tim_ptr t = _find(id);
    return t && !(t->flags & TF_INTERRUPTED);
}
int plr_tim_parm(int id)
{
    plr_tim_ptr t = _find(id);
    if (t) return t->parm;
    return 0;
}
int plr_tim_amount(int id)
{
    plr_tim_ptr t = _find(id);
    if (t) return t->count;
    return 0;
}
int plr_tim_count(void)
{
    return _count();
}

/************************************************************************
 * Calc Bonuses et.al.
 ************************************************************************/
void plr_tim_calc_bonuses(void)
{
    plr_tim_ptr t;
    for (t = plr->timers; t; t = t->next)
    {
        plr_tim_info_ptr info = _find_info(t->id);
        if (info->calc_bonuses_f)
            info->calc_bonuses_f(t);
    }
}
void plr_tim_calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr attack_info)
{
    plr_tim_ptr t;
    for (t = plr->timers; t; t = t->next)
    {
        plr_tim_info_ptr info = _find_info(t->id);
        if (info->calc_weapon_bonuses_f)
            info->calc_weapon_bonuses_f(t, obj, attack_info);
    }
}
void plr_tim_calc_shooter_bonuses(obj_ptr obj, plr_shoot_info_ptr shooter_info)
{
    plr_tim_ptr t;
    for (t = plr->timers; t; t = t->next)
    {
        plr_tim_info_ptr info = _find_info(t->id);
        if (info->calc_shooter_bonuses_f)
            info->calc_shooter_bonuses_f(t, obj, shooter_info);
    }
}
void plr_tim_flags(u32b flgs[OF_ARRAY_SIZE])
{
    plr_tim_ptr t;
    for (t = plr->timers; t; t = t->next)
    {
        plr_tim_info_ptr info = _find_info(t->id);
        if (info->flags_f)
            info->flags_f(t, flgs);
    }
}
void plr_tim_stats(s16b stats[MAX_STATS])
{
    plr_tim_ptr t;
    for (t = plr->timers; t; t = t->next)
    {
        plr_tim_info_ptr info = _find_info(t->id);
        if (info->stats_f)
            info->stats_f(t, stats);
    }
}
static status_display_t _custom_display(void)
{
    status_display_t custom = {0};
    plr_class_ptr cls = plr_class();
    plr_race_ptr race = plr_race();

    if (cls->hooks.status_display)
        custom = cls->hooks.status_display();
    else if (race->hooks.status_display)
        custom = race->hooks.status_display();

    return custom;
}
void plr_tim_status_bar(void)
{
    rect_t r = ui_status_bar_rect();
    status_display_t custom = _custom_display();
    doc_ptr doc = doc_alloc(r.cx);
    plr_tim_ptr t;
    int long_len = 0, short_len = 0, ct = 0, len = 0;

    Term_erase(r.x, r.y, r.cx);
    doc_insert(doc, "<style:table>");
    for (t = plr->timers; t; t = t->next)
    {
        plr_tim_info_ptr info = _find_info(t->id);
        status_display_t sd;
        if (!info->status_display_f) continue;
        sd = info->status_display_f(t);
        if (plr->wizard)
            doc_printf(doc, "<color:%c>%s(%d)</color> ", attr_to_attr_char(sd.color), sd.name, t->count);
        else
            doc_printf(doc, "<color:%c>%s</color> ", attr_to_attr_char(sd.color), sd.name);
        ct++;
        long_len += strlen(sd.name) + 1;
        if (plr->wizard) long_len += 4;  /* XXX not quite right */
        short_len += strlen(sd.abbrev);
    }
    if (custom.name)
    {
        doc_printf(doc, "<color:%c>%s</color> ", attr_to_attr_char(custom.color), custom.name);
        ct++;
        long_len += strlen(custom.name) + 1;
        short_len += strlen(custom.abbrev);
    }
    len = long_len;

    if (len >= r.cx)
    {
        bool spaces = FALSE;
        doc_rollback(doc, doc_pos_create(0, 0));
        if (short_len + ct < r.cx) spaces = TRUE;
        for (t = plr->timers; t; t = t->next)
        {
            plr_tim_info_ptr info = _find_info(t->id);
            status_display_t sd;
            if (!info->status_display_f) continue;
            sd = info->status_display_f(t);
            doc_printf(doc, "<color:%c>%s</color>", attr_to_attr_char(sd.color), sd.abbrev);
            if (spaces) doc_insert_char(doc, TERM_WHITE, ' ');
        }
        if (custom.name)
        {
            doc_printf(doc, "<color:%c>%s</color> ",attr_to_attr_char(custom.color), custom.abbrev);
            if (spaces) doc_insert_char(doc, TERM_WHITE, ' ');
        }
        len = short_len;
        if (spaces)
            len += ct;
    }
    len = MIN(len, r.cx);

    /* XXX make it easier to center text */
    if (ct)
        doc_sync_term(doc, doc_range_top_lines(doc, 1), doc_pos_create(r.x + (r.cx - len)/2, r.y));
    doc_free(doc);
}
void plr_tim_prt_effects(doc_ptr doc)
{
    plr_tim_ptr t;
    for (t = plr->timers; t; t = t->next)
    {
        plr_tim_info_ptr info = _find_info(t->id);
        if (info->prt_effects_f)
            info->prt_effects_f(t, doc);
    }
}
bool plr_tim_dispel_check(mon_ptr mon)
{
    plr_tim_ptr t;
    for (t = plr->timers; t; t = t->next)
    {
        plr_tim_info_ptr info = _find_info(t->id);
        if (info->dispel_prob && randint0(100) < info->dispel_prob)
            return TRUE;
        if (info->dispel_check_f && info->dispel_check_f(t, mon))
            return TRUE;
    }
    return FALSE;
}
void plr_tim_self_knowledge(doc_ptr doc)
{
    plr_tim_ptr t;
    for (t = plr->timers; t; t = t->next)
    {
        plr_tim_info_ptr info = _find_info(t->id);
        if (!info->desc) continue;
        if (t->flags & TF_INTERRUPTED)
            doc_printf(doc, "<color:D>%s</color> (<color:r>Interrupted</color>)", info->desc);
        else
            doc_insert(doc, info->desc);
        doc_newline(doc);
    }
}
void plr_tim_weigh_magic(doc_ptr doc)
{
    plr_tim_ptr t;
    for (t = plr->timers; t; t = t->next)
    {
        plr_tim_info_ptr info = _find_info(t->id);
        doc_printf(doc, "<color:U>%-18.18s</color> %3d <indent>%s</indent>\n", info->name, t->count, info->desc ? info->desc : "");
    }
}

/************************************************************************
 * Savefiles
 ************************************************************************/
void plr_tim_load(savefile_ptr file)
{
    int ct = savefile_read_s16b(file), i;
    for (i = 0; i < ct; i++)
    {
        s16b id = savefile_read_s16b(file);
        s16b count = savefile_read_s16b(file);
        plr_tim_ptr t = _alloc(id, count);
        t->flags = savefile_read_u16b(file);
        t->parm = savefile_read_s16b(file);
        _add_tail(t);
    }
    if (music_current())
        plr->action = ACTION_SING;
}

void plr_tim_save(savefile_ptr file)
{
    plr_tim_ptr t;
    savefile_write_s16b(file, _count());
    for (t = plr->timers; t; t = t->next)
    {
        savefile_write_s16b(file, t->id);
        savefile_write_s16b(file, t->count);
        savefile_write_u16b(file, t->flags);
        savefile_write_s16b(file, t->parm);
    }
}

/************************************************************************
 * Helpers for Timers
 ************************************************************************/
static int _plr_save_odds(int rlev, int boost)
{
    int roll = 100 + rlev/2 + boost;
    int sav = plr->skills.sav;
    int odds = sav * 100 / roll;
    return odds;
}
static bool _plr_save(int rlev, int boost)
{
    int odds = _plr_save_odds(rlev, boost);
    return randint0(100) < odds;
}
/************************************************************************
 * T_AURA_COLD
 ************************************************************************/
static bool _aura_cold_on(plr_tim_ptr timer)
{
    msg_print("You are enveloped by a freezing aura!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _aura_cold_off(plr_tim_ptr timer)
{
    msg_print("Your freezing aura disappears.");
    plr->update |= PU_BONUS;
}
static void _aura_cold_bonus(plr_tim_ptr timer)
{
    plr->sh_cold = TRUE;
}
static void _aura_cold_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_AURA_COLD);
}
static status_display_t _aura_cold_display(plr_tim_ptr timer)
{
    return status_display_create("SCold", "[Co", TERM_WHITE);
}
static plr_tim_info_ptr _aura_cold(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_AURA_COLD, "Aura of Cold");
    info->desc = "You are enveloped by a freezing aura.";
    info->on_f = _aura_cold_on;
    info->off_f = _aura_cold_off;
    info->calc_bonuses_f = _aura_cold_bonus;
    info->flags_f = _aura_cold_flags;
    info->status_display_f = _aura_cold_display;
    return info;
}

/************************************************************************
 * T_AURA_ELEC
 ************************************************************************/
static bool _aura_elec_on(plr_tim_ptr timer)
{
    msg_print("You are enveloped in sparks!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _aura_elec_off(plr_tim_ptr timer)
{
    msg_print("Your aura of electricity disappears.");
    plr->update |= PU_BONUS;
}
static void _aura_elec_bonus(plr_tim_ptr timer)
{
    plr->sh_elec = TRUE;
}
static void _aura_elec_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_AURA_ELEC);
}
static status_display_t _aura_elec_display(plr_tim_ptr timer)
{
    return status_display_create("SElec", "[El", TERM_BLUE);
}
static plr_tim_info_ptr _aura_elec(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_AURA_ELEC, "Aura of Electricity");
    info->desc = "You are enveloped by an aura of electricity.";
    info->on_f = _aura_elec_on;
    info->off_f = _aura_elec_off;
    info->calc_bonuses_f = _aura_elec_bonus;
    info->flags_f = _aura_elec_flags;
    info->status_display_f = _aura_elec_display;
    return info;
}

/************************************************************************
 * T_AURA_FIRE
 ************************************************************************/
static bool _aura_fire_on(plr_tim_ptr timer)
{
    msg_print("You are enveloped by a fiery aura!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _aura_fire_off(plr_tim_ptr timer)
{
    msg_print("Your fiery aura disappears.");
    plr->update |= PU_BONUS;
}
static void _aura_fire_bonus(plr_tim_ptr timer)
{
    plr->sh_fire = TRUE;
}
static void _aura_fire_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_AURA_FIRE);
}
static status_display_t _aura_fire_display(plr_tim_ptr timer)
{
    return status_display_create("SFire", "[F", TERM_L_RED);
}
static plr_tim_info_ptr _aura_fire(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_AURA_FIRE, "Aura of Fire");
    info->desc = "You are enveloped by an aura of fire.";
    info->on_f = _aura_fire_on;
    info->off_f = _aura_fire_off;
    info->calc_bonuses_f = _aura_fire_bonus;
    info->flags_f = _aura_fire_flags;
    info->status_display_f = _aura_fire_display;
    return info;
}

/************************************************************************
 * T_AURA_HOLY
 ************************************************************************/
static bool _aura_holy_on(plr_tim_ptr timer)
{
    msg_print("You are cloaked in righteousness!");
    return TRUE;
}
static void _aura_holy_off(plr_tim_ptr timer)
{
    msg_print("Your aura of righteousness fades away.");
}
static void _aura_holy_bonus(plr_tim_ptr timer)
{
    plr->sh_holy = TRUE;
}
static status_display_t _aura_holy_display(plr_tim_ptr timer)
{
    return status_display_create("Holy", "Ho", TERM_WHITE);
}
static plr_tim_info_ptr _aura_holy(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_AURA_HOLY, "Aura of Holiness");
    info->desc = "You are protected by a holy aura that damages evil monsters.";
    info->on_f = _aura_holy_on;
    info->off_f = _aura_holy_off;
    info->calc_bonuses_f = _aura_holy_bonus;
    info->status_display_f = _aura_holy_display;
    return info;
}

/************************************************************************
 * T_AURA_SHARDS
 ************************************************************************/
static bool _aura_shards_on(plr_tim_ptr timer)
{
    if (plr->pclass == CLASS_MIRROR_MASTER)
        msg_print("You were enveloped by mirror shards.");
    else
        msg_print("You are enveloped in shards!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _aura_shards_off(plr_tim_ptr timer)
{
    if (plr->pclass == CLASS_MIRROR_MASTER)
        msg_print("The mirror shards disappear.");
    else
        msg_print("You are no longer enveloped in shards.");
    plr->update |= PU_BONUS;
}
static void _aura_shards_bonus(plr_tim_ptr timer)
{
    plr->sh_shards = TRUE;
}
static void _aura_shards_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_AURA_SHARDS);
}
static status_display_t _aura_shards_display(plr_tim_ptr timer)
{
    return status_display_create("SShards", "[Sh", TERM_UMBER);
}
static plr_tim_info_ptr _aura_shards(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_AURA_SHARDS, "Aura of Shards");
    info->desc = "You are enveloped in an aura of shards.";
    info->on_f = _aura_shards_on;
    info->off_f = _aura_shards_off;
    info->calc_bonuses_f = _aura_shards_bonus;
    info->flags_f = _aura_shards_flags;
    info->status_display_f = _aura_shards_display;
    return info;
}

/************************************************************************
 * T_BERSERK
 ************************************************************************/
static bool _berserk_on(plr_tim_ptr timer)
{
    msg_print("You feel like a killing machine!");
    fear_clear_p();
    plr->update |= PU_BONUS | PU_HP;
    return TRUE;
}
static void _berserk_off(plr_tim_ptr timer)
{
    msg_print("You feel less Berserk.");
    plr->update |= PU_BONUS | PU_HP;
}
static void _berserk_bonus(plr_tim_ptr timer)
{
    int pct = plr->pclass == CLASS_RAGE_MAGE ? 50 : 100; /* XXX tweak me */
    res_add_immune(GF_FEAR);
    plr->pspeed += 3;
    /* Note: The Rage Mage is no longer skill smashed by Berserk */
    plr->to_a -= 10 * pct / 100;
    plr->dis_to_a -= 10 * pct / 100;
    plr->skills.stl -= 7 * pct / 100;
    plr->skills.dev -= 20 * pct / 100;
    plr->skills.sav -= 30 * pct / 100;
    plr->skills.srh -= 15 * pct / 100;
    plr->skills.fos -= 15 * pct / 100;
    plr->skill_tht -= 20;
    plr->skill_dig += 30;
    plr->to_h_m += 12;
    plr->innate_attack_info.to_h += 12;
    plr->innate_attack_info.dis_to_h += 12;
    if (plr->prace != RACE_MON_BEHOLDER)
    {
        int to_d = 3 + plr->lev/5;
        plr->to_d_m += to_d;
        plr->innate_attack_info.to_d += to_d;
        plr->innate_attack_info.dis_to_d += to_d;
    }
    plr->xtra_hp += 30;
}
static void _berserk_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_IM_(GF_FEAR));
    add_flag(flgs, OF_SPEED);
}
static void _berserk_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    int to_d = 3 + plr->lev/5;
    info->to_h += 12;
    info->dis_to_h += 12;
    /* Dual wielding prorates damage across weaponry (unless Genji) ... */
    if (have_flag(info->paf_flags, PAF_DUAL_WIELDING) && !have_flag(info->paf_flags, PAF_GENJI))
    {
        info->to_d += (to_d + info->which%2)/2;
        info->dis_to_d += (to_d + info->which%2)/2;
    }
    /* but multiple arms each get the full bonus. In other words, we biff a 
     * berserking ninja, but not a Xorn single wielding in each set of arms. */
    else
    {
        info->to_d += to_d;
        info->dis_to_d += to_d;
    }
}
static void _berserk_shooter_bonus(plr_tim_ptr timer, obj_ptr obj, plr_shoot_info_ptr info)
{
    info->to_h -= 12;
    info->dis_to_h -= 12;
}
static status_display_t _berserk_display(plr_tim_ptr timer)
{
    return status_display_create("Berserk", "Br", TERM_RED);
}
static plr_tim_info_ptr _berserk(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_BERSERK, "Berserk");
    info->desc = "You are in a berserk rage.";
    info->on_f = _berserk_on;
    info->off_f = _berserk_off;
    info->calc_bonuses_f = _berserk_bonus;
    info->flags_f = _berserk_flags;
    info->calc_weapon_bonuses_f = _berserk_weapon_bonus;
    info->calc_shooter_bonuses_f = _berserk_shooter_bonus;
    info->status_display_f = _berserk_display;
    info->dispel_prob = 50;
    return info;
}

/************************************************************************
 * T_BLESSED
 ************************************************************************/
static bool _blessed_on(plr_tim_ptr timer)
{
    msg_print("You feel righteous!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _blessed_off(plr_tim_ptr timer)
{
    msg_print("The prayer has expired.");
    plr->update |= PU_BONUS;
}
void blessed_bonus(plr_tim_ptr timer)
{
    plr_bonus_ac(5);
    plr->to_h_m += 10;
    plr->innate_attack_info.to_h += 10;
    plr->innate_attack_info.dis_to_h += 10;
}
void blessed_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    info->to_h += 10;
    info->dis_to_h += 10;
}
void blessed_shooter_bonus(plr_tim_ptr timer, obj_ptr obj, plr_shoot_info_ptr info)
{
    info->to_h += 10;
    info->dis_to_h += 10;
}
static status_display_t _blessed_display(plr_tim_ptr timer)
{
    return status_display_create("Bless", "Bs", TERM_WHITE);
}
static plr_tim_info_ptr _blessed(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_BLESSED, "Blessed");
    info->desc = "You are blessed.";
    info->on_f = _blessed_on;
    info->off_f = _blessed_off;
    info->calc_bonuses_f = blessed_bonus;
    info->calc_weapon_bonuses_f = blessed_weapon_bonus;
    info->calc_shooter_bonuses_f = blessed_shooter_bonus;
    info->status_display_f = _blessed_display;
    return info;
}

/************************************************************************
 * T_BLIND
 ************************************************************************/
static void _blind_status(void)
{
    plr->update |= PU_UN_VIEW | PU_UN_LIGHT | PU_VIEW | PU_LIGHT | PU_MONSTERS | PU_MON_LIGHT;
    if (prace_is_(RACE_MON_BEHOLDER))
        plr->update |= PU_BONUS;

    plr->redraw |= PR_MAP | PR_EFFECTS;
    plr->window |= PW_OVERHEAD | PW_DUNGEON;
}
static bool _blind_on(plr_tim_ptr timer)
{
    msg_print("You are blind.");
    _blind_status();
    virtue_add(VIRTUE_ENLIGHTENMENT, -1);
    return TRUE;
}
static void _blind_off(plr_tim_ptr timer)
{
    msg_print("You can see again.");
    _blind_status();
}
static void _blind_tick(plr_tim_ptr timer)
{
    do { timer->count--; }
    while (timer->count > 0 && res_save_default(GF_BLIND));
}
static void _blind_display(plr_tim_ptr timer, doc_ptr doc)
{
    doc_insert(doc, "<color:D>Blind</color>\n");
}
static plr_tim_info_ptr _blind(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_BLIND, "Blindness");
    info->desc = "You are blind.";
    info->on_f = _blind_on;
    info->off_f = _blind_off;
    info->tick_f = _blind_tick;
    info->prt_effects_f = _blind_display;
    info->flags = TF_BIFF;
    return info;
}

/************************************************************************
 * T_BRAND_ACID
 ************************************************************************/
static bool _brand_acid_on(plr_tim_ptr timer)
{
    plr_tim_remove(T_BRAND_ELEC);
    plr_tim_remove(T_BRAND_FIRE);
    plr_tim_remove(T_BRAND_COLD);
    plr_tim_remove(T_BRAND_POIS);
    msg_print("For a while, the blows you deal will melt with acid!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _brand_acid_off(plr_tim_ptr timer)
{
    msg_print("Your temporary acidic brand fades away.");
    plr->update |= PU_BONUS;
}
static void _brand_acid_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    add_flag(info->obj_flags, OF_BRAND_ACID);
    add_flag(info->obj_known_flags, OF_BRAND_ACID);
}
static void _brand_acid_shooter_bonus(plr_tim_ptr timer, obj_ptr obj, plr_shoot_info_ptr info)
{
    add_flag(info->flags, OF_BRAND_ACID);
    add_flag(info->known_flags, OF_BRAND_ACID);
}
static void _brand_acid_flags(plr_tim_ptr timer, u32b flags[OF_ARRAY_SIZE])
{
    add_flag(flags, OF_BRAND_ACID);
}
static bool _brand_acid_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    if (mon_res_pct(mon, GF_ACID) > 0) return FALSE;
    return TRUE;
}
static status_display_t _brand_acid_display(plr_tim_ptr timer)
{
    return status_display_create("BAcid", "BAc", TERM_SLATE);
}
static plr_tim_info_ptr _brand_acid(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_BRAND_ACID, "Acid Brand");
    info->desc = "Your weapon melts your enemies with acid.";
    info->on_f = _brand_acid_on;
    info->off_f = _brand_acid_off;
    info->calc_weapon_bonuses_f = _brand_acid_weapon_bonus;
    info->calc_shooter_bonuses_f = _brand_acid_shooter_bonus;
    info->flags_f = _brand_acid_flags;
    info->dispel_check_f = _brand_acid_dispel;
    info->status_display_f = _brand_acid_display;
    return info;
}

/************************************************************************
 * T_BRAND_ELEC
 ************************************************************************/
static bool _brand_elec_on(plr_tim_ptr timer)
{
    plr_tim_remove(T_BRAND_ACID);
    plr_tim_remove(T_BRAND_FIRE);
    plr_tim_remove(T_BRAND_COLD);
    plr_tim_remove(T_BRAND_POIS);
    msg_print("For a while, the blows you deal will shock your foes!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _brand_elec_off(plr_tim_ptr timer)
{
    msg_print("Your temporary electrical brand fades away.");
    plr->update |= PU_BONUS;
}
static void _brand_elec_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    add_flag(info->obj_flags, OF_BRAND_ELEC);
    add_flag(info->obj_known_flags, OF_BRAND_ELEC);
}
static void _brand_elec_shooter_bonus(plr_tim_ptr timer, obj_ptr obj, plr_shoot_info_ptr info)
{
    add_flag(info->flags, OF_BRAND_ELEC);
    add_flag(info->known_flags, OF_BRAND_ELEC);
}
static void _brand_elec_flags(plr_tim_ptr timer, u32b flags[OF_ARRAY_SIZE])
{
    add_flag(flags, OF_BRAND_ELEC);
}
static bool _brand_elec_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    if (mon_res_pct(mon, GF_ELEC) > 0) return FALSE;
    return TRUE;
}
static status_display_t _brand_elec_display(plr_tim_ptr timer)
{
    return status_display_create("BElec", "BEl", TERM_L_BLUE);
}
static plr_tim_info_ptr _brand_elec(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_BRAND_ELEC, "Elec Brand");
    info->desc = "Your weapon shocks your foes.";
    info->on_f = _brand_elec_on;
    info->off_f = _brand_elec_off;
    info->calc_weapon_bonuses_f = _brand_elec_weapon_bonus;
    info->calc_shooter_bonuses_f = _brand_elec_shooter_bonus;
    info->flags_f = _brand_elec_flags;
    info->dispel_check_f = _brand_elec_dispel;
    info->status_display_f = _brand_elec_display;
    return info;
}

/************************************************************************
 * T_BRAND_FIRE
 ************************************************************************/
static bool _brand_fire_on(plr_tim_ptr timer)
{
    plr_tim_remove(T_BRAND_ACID);
    plr_tim_remove(T_BRAND_ELEC);
    plr_tim_remove(T_BRAND_COLD);
    plr_tim_remove(T_BRAND_POIS);
    msg_print("For a while, the blows you deal will burn with fire!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _brand_fire_off(plr_tim_ptr timer)
{
    msg_print("Your temporary fiery brand fades away.");
    plr->update |= PU_BONUS;
}
static void _brand_fire_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    add_flag(info->obj_flags, OF_BRAND_FIRE);
    add_flag(info->obj_known_flags, OF_BRAND_FIRE);
}
static void _brand_fire_shooter_bonus(plr_tim_ptr timer, obj_ptr obj, plr_shoot_info_ptr info)
{
    add_flag(info->flags, OF_BRAND_FIRE);
    add_flag(info->known_flags, OF_BRAND_FIRE);
}
static void _brand_fire_flags(plr_tim_ptr timer, u32b flags[OF_ARRAY_SIZE])
{
    add_flag(flags, OF_BRAND_FIRE);
}
static bool _brand_fire_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    if (mon_res_pct(mon, GF_FIRE) > 0) return FALSE;
    return TRUE;
}
static status_display_t _brand_fire_display(plr_tim_ptr timer)
{
    return status_display_create("BFire", "BFi", TERM_L_RED);
}
static plr_tim_info_ptr _brand_fire(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_BRAND_FIRE, "Flame Tongue");
    info->desc = "Your weapon burns your foes.";
    info->on_f = _brand_fire_on;
    info->off_f = _brand_fire_off;
    info->calc_weapon_bonuses_f = _brand_fire_weapon_bonus;
    info->calc_shooter_bonuses_f = _brand_fire_shooter_bonus;
    info->flags_f = _brand_fire_flags;
    info->dispel_check_f = _brand_fire_dispel;
    info->status_display_f = _brand_fire_display;
    return info;
}

/************************************************************************
 * T_BRAND_COLD
 ************************************************************************/
static bool _brand_cold_on(plr_tim_ptr timer)
{
    plr_tim_remove(T_BRAND_ACID);
    plr_tim_remove(T_BRAND_ELEC);
    plr_tim_remove(T_BRAND_FIRE);
    plr_tim_remove(T_BRAND_POIS);
    msg_print("For a while, the blows you deal will chill to the bone!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _brand_cold_off(plr_tim_ptr timer)
{
    msg_print("Your temporary frost brand fades away.");
    plr->update |= PU_BONUS;
}
static void _brand_cold_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    add_flag(info->obj_flags, OF_BRAND_COLD);
    add_flag(info->obj_known_flags, OF_BRAND_COLD);
}
static void _brand_cold_shooter_bonus(plr_tim_ptr timer, obj_ptr obj, plr_shoot_info_ptr info)
{
    add_flag(info->flags, OF_BRAND_COLD);
    add_flag(info->known_flags, OF_BRAND_COLD);
}
static void _brand_cold_flags(plr_tim_ptr timer, u32b flags[OF_ARRAY_SIZE])
{
    add_flag(flags, OF_BRAND_COLD);
}
static bool _brand_cold_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    if (mon_res_pct(mon, GF_COLD) > 0) return FALSE;
    return TRUE;
}
static status_display_t _brand_cold_display(plr_tim_ptr timer)
{
    return status_display_create("BCold", "BCo", TERM_WHITE);
}
static plr_tim_info_ptr _brand_cold(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_BRAND_COLD, "Frost Brand");
    info->desc = "Your weapon freezes your enemies.";
    info->on_f = _brand_cold_on;
    info->off_f = _brand_cold_off;
    info->calc_weapon_bonuses_f = _brand_cold_weapon_bonus;
    info->calc_shooter_bonuses_f = _brand_cold_shooter_bonus;
    info->flags_f = _brand_cold_flags;
    info->dispel_check_f = _brand_cold_dispel;
    info->status_display_f = _brand_cold_display;
    return info;
}

/************************************************************************
 * T_BRAND_POIS
 ************************************************************************/
static bool _brand_pois_on(plr_tim_ptr timer)
{
    plr_tim_remove(T_BRAND_ACID);
    plr_tim_remove(T_BRAND_ELEC);
    plr_tim_remove(T_BRAND_FIRE);
    plr_tim_remove(T_BRAND_COLD);
    msg_print("For a while, the blows you deal will poison your enemies!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _brand_pois_off(plr_tim_ptr timer)
{
    msg_print("Your temporary poison brand fades away.");
    plr->update |= PU_BONUS;
}
static void _brand_pois_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    add_flag(info->obj_flags, OF_BRAND_POIS);
    add_flag(info->obj_known_flags, OF_BRAND_POIS);
}
static void _brand_pois_shooter_bonus(plr_tim_ptr timer, obj_ptr obj, plr_shoot_info_ptr info)
{
    add_flag(info->flags, OF_BRAND_POIS);
    add_flag(info->known_flags, OF_BRAND_POIS);
}
static void _brand_pois_flags(plr_tim_ptr timer, u32b flags[OF_ARRAY_SIZE])
{
    add_flag(flags, OF_BRAND_POIS);
}
static bool _brand_pois_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    if (mon_res_pct(mon, GF_POIS) > 0) return FALSE;
    return TRUE;
}
static status_display_t _brand_pois_display(plr_tim_ptr timer)
{
    return status_display_create("BPois", "BPo", TERM_L_GREEN);
}
static plr_tim_info_ptr _brand_pois(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_BRAND_POIS, "Viper's Fang");
    info->desc = "Your weapon poisons your enemies.";
    info->on_f = _brand_pois_on;
    info->off_f = _brand_pois_off;
    info->calc_weapon_bonuses_f = _brand_pois_weapon_bonus;
    info->calc_shooter_bonuses_f = _brand_pois_shooter_bonus;
    info->flags_f = _brand_pois_flags;
    info->dispel_check_f = _brand_pois_dispel;
    info->status_display_f = _brand_pois_display;
    return info;
}

/************************************************************************
 * T_BRAND_MANA
 ************************************************************************/
static bool _brand_mana_on(plr_tim_ptr timer)
{
    if (plr->attack_info[0].type == PAT_MONK || plr->attack_info[1].type == PAT_MONK)
        msg_print("Your fists begin to thrum with power!");
    else
        msg_print("Your weapon begins to thrum with power!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _brand_mana_off(plr_tim_ptr timer)
{
    if (plr->attack_info[0].type == PAT_MONK || plr->attack_info[1].type == PAT_MONK)
        msg_print("Your fists no longer thrum with power.");
    else
        msg_print("Your weapon no longer thrums with power.");
    plr->update |= PU_BONUS;
}
static void _brand_mana_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    add_flag(info->obj_flags, OF_BRAND_MANA);
    add_flag(info->obj_known_flags, OF_BRAND_MANA);
}
static void _brand_mana_shooter_bonus(plr_tim_ptr timer, obj_ptr obj, plr_shoot_info_ptr info)
{
    add_flag(info->flags, OF_BRAND_MANA);
    add_flag(info->known_flags, OF_BRAND_MANA);
}
static void _brand_mana_flags(plr_tim_ptr timer, u32b flags[OF_ARRAY_SIZE])
{
    add_flag(flags, OF_BRAND_MANA);
}
static status_display_t _brand_mana_display(plr_tim_ptr timer)
{
    return status_display_create("Force", "Fc", TERM_L_BLUE);
}
static plr_tim_info_ptr _brand_mana(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_BRAND_MANA, "Mana Brand");
    info->desc = "Your weapon strikes powerfully at your enemies using your mana.";
    info->on_f = _brand_mana_on;
    info->off_f = _brand_mana_off;
    info->calc_weapon_bonuses_f = _brand_mana_weapon_bonus;
    info->calc_shooter_bonuses_f = _brand_mana_shooter_bonus;
    info->flags_f = _brand_mana_flags;
    info->status_display_f = _brand_mana_display;
    info->dispel_prob = 100;
    return info;
}

/************************************************************************
 * T_CONFUSED
 ************************************************************************/
static bool _confused_on(plr_tim_ptr timer)
{
    msg_print("You are confused!");
    #if 0
    if (plr->action == ACTION_LEARN) XXX Blue Mage
    {
        msg_print("You cannot continue Learning!");
        new_mane = FALSE;

        plr->redraw |= PR_STATE;
        plr->action = ACTION_NONE;
    }
    #endif
    virtue_add(VIRTUE_HARMONY, -1);
    plr->redraw |= PR_EFFECTS;
    return TRUE;
}
static void _confused_off(plr_tim_ptr timer)
{
    msg_print("You feel less confused now.");
    plr->redraw |= PR_EFFECTS;
}
static void _confused_tick(plr_tim_ptr timer)
{
    do { timer->count--; }
    while (timer->count > 0 && res_save_default(GF_CONF));
}
static void _confused_display(plr_tim_ptr timer, doc_ptr doc)
{
    doc_insert(doc, "<color:v>Confused</color>\n");
}
static plr_tim_info_ptr _confused(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_CONFUSED, "Confused");
    info->desc = "You are confused.";
    info->on_f = _confused_on;
    info->off_f = _confused_off;
    info->tick_f = _confused_tick;
    info->prt_effects_f = _confused_display;
    info->flags = TF_BIFF;
    return info;
}

/************************************************************************
 * T_CUT:
 * This is a difficult timer, since it needs to emit messages whenever
 * the cut level changes. We use a custom add_f hook to achieve this.
 ************************************************************************/
typedef struct { int level; int dam; cptr desc; byte attr; } _cut_info_t;
static _cut_info_t _cut_info(int cut)
{
    point_t tbl[7] = { {CUT_GRAZE, 1}, {CUT_LIGHT, 3}, {CUT_BAD, 7}, {CUT_NASTY, 16},
                       {CUT_SEVERE, 32}, {CUT_DEEP_GASH, 80}, {CUT_MORTAL_WOUND, 200} };
    _cut_info_t result = {0};
    if (cut) result.dam = interpolate(cut, tbl, 7);
    if (cut >= CUT_MORTAL_WOUND)
    {
        result.level = CUT_MORTAL_WOUND;
        result.desc = "Mortal Wound";
        result.attr = TERM_L_RED;
    }
    else if (cut >= CUT_DEEP_GASH)
    {
        result.level = CUT_DEEP_GASH;
        result.desc = "Deep Gash";
        result.attr = TERM_RED;
    }
    else if (cut >= CUT_SEVERE)
    {
        result.level = CUT_SEVERE;
        result.desc = "Severe Cut";
        result.attr = TERM_RED;
    }
    else if (cut >= CUT_NASTY)
    {
        result.level = CUT_NASTY;
        result.desc = "Nasty Cut";
        result.attr = TERM_ORANGE;
    }
    else if (cut >= CUT_BAD)
    {
        result.level = CUT_BAD;
        result.desc = "Bad Cut";
        result.attr = TERM_ORANGE;
    }
    else if (cut >= CUT_LIGHT)
    {
        result.level = CUT_LIGHT;
        result.desc = "Light Cut";
        result.attr = TERM_YELLOW;
    }
    else if (cut >= CUT_GRAZE)
    {
        result.level = CUT_GRAZE;
        result.desc = "Graze";
        result.attr = TERM_YELLOW;
    }
    else
    {
        assert(result.level == CUT_NONE);
    }
    return result;
}
static void _cut_change(int old, int new)
{
    _cut_info_t old_cut = _cut_info(old);
    _cut_info_t new_cut = _cut_info(new);
    if (new_cut.level > old_cut.level)
    {
        msg_format("You have been given a <color:%c>%s</color>.", attr_to_attr_char(new_cut.attr), new_cut.desc);
        plr->redraw |= PR_EFFECTS;
        if (plr->pclass == CLASS_BLOOD_KNIGHT) plr->update = PU_BONUS;
    }
    else if (new_cut.level < old_cut.level)
    {
        if (new_cut.level == CUT_NONE)
            msg_print("You are no longer bleeding.");
        plr->redraw |= PR_EFFECTS;
        if (plr->pclass == CLASS_BLOOD_KNIGHT) plr->update = PU_BONUS;
    }
}
static bool _cut_on(plr_tim_ptr timer)
{
    _cut_change(0, timer->count);
    return TRUE;
}
static void _cut_add(plr_tim_ptr timer, int amt)
{
    _cut_change(timer->count, timer->count + amt);
    timer->count += amt;
}
static void _cut_off(plr_tim_ptr timer)
{
    /* no need ... the final tick_f will handle this (but this hook is assumed to exist) */
}
static void _cut_tick(plr_tim_ptr timer)
{
    int amt = MIN(timer->count, adj_con_fix[plr->stat_ind[A_CON]] + 1);

    if (!plr_tim_find(T_INVULN))
    {
        _cut_info_t cut = _cut_info(timer->count);
        if (cut.dam)
            take_hit(DAMAGE_NOESCAPE, cut.dam, "a fatal wound");
    }

    if (timer->count > CUT_MORTAL_WOUND) return; /* mortal wounds do not heal ... you need to see a doctor! */

    if (plr->pclass == CLASS_BLOOD_KNIGHT) /* Blood-Knights *want* to be cut (cf _cauterize_wounds_spell) */
        amt = MAX(1, amt/3);

    _cut_add(timer, -amt);
}
static void _cut_display(plr_tim_ptr timer, doc_ptr doc)
{
    _cut_info_t cut = _cut_info(timer->count);
    if (cut.level == CUT_NONE) return; /* paranoia */
    doc_printf(doc, "<color:%c>%s</color>\n", attr_to_attr_char(cut.attr), cut.desc);
}
static plr_tim_info_ptr _cut(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_CUT, "Cut");
    info->desc = "You are bleeding.";
    info->on_f = _cut_on;
    info->add_f = _cut_add;
    info->off_f = _cut_off;
    info->tick_f = _cut_tick;
    info->prt_effects_f = _cut_display;
    info->flags = TF_BIFF;
    return info;
}

/************************************************************************
 * T_DEVICE_POWER
 ************************************************************************/
static bool _device_power_on(plr_tim_ptr timer)
{
    msg_print("Your magical devices feel more powerful.");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _device_power_off(plr_tim_ptr timer)
{
    msg_print("Your magical devices return to normal.");
    plr->update |= PU_BONUS;
}
static void _device_power_bonus(plr_tim_ptr timer)
{
    int bonus = 1 + (plr->lev + 10)/15;
    plr->device_power += bonus;
}
static void _device_power_flags(plr_tim_ptr timer, u32b flags[OF_ARRAY_SIZE])
{
    add_flag(flags, OF_DEVICE_POWER);
}
static status_display_t _device_power_display(plr_tim_ptr timer)
{
    return status_display_create("Device", "Dv", TERM_VIOLET);
}
static plr_tim_info_ptr _device_power(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_DEVICE_POWER, "Device Power");
    info->desc = "Your magical devices are more powerful.";
    info->on_f = _device_power_on;
    info->off_f = _device_power_off;
    info->calc_bonuses_f = _device_power_bonus;
    info->flags_f = _device_power_flags;
    info->status_display_f = _device_power_display;
    return info;
}

/************************************************************************
 * T_EGO_WHIP
 ************************************************************************/
static bool _ego_whip_on(plr_tim_ptr timer)
{
    msg_print("Your mind is lashed by an ego whip!");
    plr->redraw |= PR_EFFECTS;
    return TRUE;
}
static void _ego_whip_off(plr_tim_ptr timer)
{
    msg_print("You shake off the ego whip!");
    plr->redraw |= PR_EFFECTS;
}
static void _ego_whip_tick(plr_tim_ptr timer)
{
    int rlev = timer->parm;
    int dam = 120*rlev/100;
    msg_print("You are lashed!");
    take_hit(DAMAGE_NOESCAPE, dam, "an ego whip");
    do { timer->count--; }
    while (timer->count > 0 && _plr_save(rlev, 0));
}
static void _ego_whip_display(plr_tim_ptr timer, doc_ptr doc)
{
    doc_insert(doc, "<color:B>Ego Whip</color>\n");
}
static plr_tim_info_ptr _ego_whip(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_EGO_WHIP, "Ego Whip");
    info->desc = "You are being lashed by an ego whip.";
    info->on_f = _ego_whip_on;
    info->off_f = _ego_whip_off;
    info->tick_f = _ego_whip_tick;
    info->prt_effects_f = _ego_whip_display;
    info->flags = TF_BIFF | TF_FAST_TICK;
    return info;
}

/************************************************************************
 * T_ENLARGE_WEAPON
 ************************************************************************/
static bool _enlarge_weapon_on(plr_tim_ptr timer)
{
    msg_print("You feel your weapon is much bigger.");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _enlarge_weapon_off(plr_tim_ptr timer)
{
    msg_print("Your weapon returns to normal.");
    plr->update |= PU_BONUS;
}
static void _enlarge_weapon_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    if (!obj) return;
    if (!obj_is_specified_art(obj, "\\.Ruyi")) return;
    info->to_dd += 2;
    info->to_ds += 2;

    info->dis_to_h -= 20;
    info->to_h -= 20;
}
static status_display_t _enlarge_weapon_display(plr_tim_ptr timer)
{
    return status_display_create("Enlarge", "Ew", TERM_RED);
}
static plr_tim_info_ptr _enlarge_weapon(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_ENLARGE_WEAPON, "Enlarge Weapon");
    info->desc = "Your weapon has grown much bigger.";
    info->on_f = _enlarge_weapon_on;
    info->off_f = _enlarge_weapon_off;
    info->calc_weapon_bonuses_f = _enlarge_weapon_weapon_bonus;
    info->status_display_f = _enlarge_weapon_display;
    info->dispel_prob = 100;
    return info;
}

/************************************************************************
 * T_FAST
 ************************************************************************/
static bool _fast_on(plr_tim_ptr timer)
{
    if (plr_tim_find(T_LIGHT_SPEED)) return FALSE;
    msg_print("You feel yourself moving much faster!");
    virtue_add(VIRTUE_PATIENCE, -1);
    virtue_add(VIRTUE_DILIGENCE, 1);
    plr->update |= PU_BONUS;
    plr->redraw |= PR_EFFECTS;
    return TRUE;
}
static void _fast_off(plr_tim_ptr timer)
{
    if (plr_tim_find(T_LIGHT_SPEED)) return;
    msg_print("You feel yourself slow down.");
    plr->update |= PU_BONUS;
    plr->redraw |= PR_EFFECTS;
}
static void _fast_bonus(plr_tim_ptr timer)
{
    if (plr_tim_find(T_LIGHT_SPEED)) return;
    if (!plr->riding)
        plr_bonus_speed(10);
}
static void _fast_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SPEED);
}
static bool _fast_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    return plr->pspeed < 35;
}
static plr_tim_info_ptr _fast(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_FAST, "Fast");
    info->desc = "You are moving much faster.";
    info->on_f = _fast_on;
    info->off_f = _fast_off;
    info->calc_bonuses_f = _fast_bonus;
    info->flags_f = _fast_flags;
    info->dispel_check_f = _fast_dispel;
    return info;
}

/************************************************************************
 * T_GIANT_STRENGTH
 ************************************************************************/
static bool _giant_strength_on(plr_tim_ptr timer)
{
    msg_print("You become gigantic!");
    plr->update |= PU_BONUS | PU_HP;
    return TRUE;
}
static void _giant_strength_off(plr_tim_ptr timer)
{
    msg_print("Your body reverts to normal size.");
    plr->update |= PU_BONUS | PU_HP;
}
static void _giant_strength_bonus(plr_tim_ptr timer)
{
    plr->skills.thn += 60*plr->lev/50;
    plr->xtra_hp += 10 + plr->lev/2;
}
static void _giant_strength_stats(plr_tim_ptr timer, s16b stats[MAX_STATS])
{
    int amt = 4 * plr->lev / 50; /* 13, 25, 38, 50 */
    stats[A_STR] += amt;
    stats[A_DEX] += amt;
    stats[A_CON] += amt;
}
static void _giant_strength_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_STR);
    add_flag(flgs, OF_DEX);
    add_flag(flgs, OF_CON);
}
static void _giant_strength_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    info->blows_calc.wgt /= 2;
    info->blows_calc.mul += 20;
}
static status_display_t _giant_strength_display(plr_tim_ptr timer)
{
    return status_display_create("Giant", "Gi", TERM_L_UMBER);
}
static plr_tim_info_ptr _giant_strength(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_GIANT_STRENGTH, "Giant Strength");
    info->desc = "You have grown much bigger.";
    info->on_f = _giant_strength_on;
    info->off_f = _giant_strength_off;
    info->calc_bonuses_f = _giant_strength_bonus;
    info->calc_weapon_bonuses_f = _giant_strength_weapon_bonus;
    info->stats_f = _giant_strength_stats;
    info->flags_f = _giant_strength_flags;
    info->status_display_f = _giant_strength_display;
    return info;
}

/************************************************************************
 * T_HALLUCINATE
 ************************************************************************/
static void _hallucinate_status(void)
{
    plr->redraw |= PR_EFFECTS | PR_MAP | PR_HEALTH_BARS;
    plr->update |= PU_MONSTERS;
    plr->window |= PW_OVERHEAD | PW_DUNGEON;
}
static bool _hallucinate_on(plr_tim_ptr timer)
{
    msg_print("Oh, wow! Everything looks so cosmic now!");
    _hallucinate_status();
    return TRUE;
}
static void _hallucinate_off(plr_tim_ptr timer)
{
    msg_print("You can see clearly again.");
    _hallucinate_status();
}
static void _hallucinate_tick(plr_tim_ptr timer)
{
    do { timer->count--; }
    while (timer->count > 0 && res_save_default(GF_CHAOS));
}
static void _hallucinate_display(plr_tim_ptr timer, doc_ptr doc)
{
    doc_insert(doc, "<color:v>Hallucinate</color>\n");
}
static plr_tim_info_ptr _hallucinate(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_HALLUCINATE, "Hallucination");
    info->desc = "You are hallucinating.";
    info->on_f = _hallucinate_on;
    info->off_f = _hallucinate_off;
    info->tick_f = _hallucinate_tick;
    info->prt_effects_f = _hallucinate_display;
    info->flags = TF_BIFF;
    return info;
}

/************************************************************************
 * T_HERO
 ************************************************************************/
static bool _hero_on(plr_tim_ptr timer)
{
    msg_print("You feel like a hero!");
    fear_clear_p();
    plr->update |= PU_BONUS | PU_HP;
    return TRUE;
}
static void _hero_off(plr_tim_ptr timer)
{
    msg_print("The heroism wears off.");
    plr->update |= PU_BONUS | PU_HP;
}
void hero_bonus(plr_tim_ptr timer)
{
    plr->xtra_hp += 10;
    plr->to_h_m += 12;
    plr->innate_attack_info.to_h += 12;
    plr->innate_attack_info.dis_to_h += 12;
    res_add(GF_FEAR);
}
void hero_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_FEAR));
}
void hero_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    info->to_h += 12;
    info->dis_to_h += 12;
}
void hero_shooter_bonus(plr_tim_ptr timer, obj_ptr obj, plr_shoot_info_ptr info)
{
    info->to_h += 12;
    info->dis_to_h += 12;
}
static status_display_t _hero_display(plr_tim_ptr timer)
{
    return status_display_create("Hero", "He", TERM_WHITE);
}
static plr_tim_info_ptr _hero(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_HERO, "Heroism");
    info->desc = "You are the stuff of legends.";
    info->on_f = _hero_on;
    info->off_f = _hero_off;
    info->calc_bonuses_f = hero_bonus;
    info->flags_f = hero_flags;
    info->calc_weapon_bonuses_f = hero_weapon_bonus;
    info->calc_shooter_bonuses_f = hero_shooter_bonus;
    info->status_display_f = _hero_display;
    return info;
}

/************************************************************************
 * T_IM_ACID
 ************************************************************************/
static bool _im_acid_on(plr_tim_ptr timer)
{
    plr_tim_remove(T_IM_ELEC);
    plr_tim_remove(T_IM_FIRE);
    plr_tim_remove(T_IM_COLD);
    msg_print("You feel immune to acid!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _im_acid_off(plr_tim_ptr timer)
{
    msg_print("You are no longer immune to acid.");
    plr->update |= PU_BONUS;
}
static void _im_acid_bonus(plr_tim_ptr timer)
{
    res_add_immune(GF_ACID);
}
static void _im_acid_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_IM_(GF_ACID));
}
static bool _im_acid_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    return mon_has_breath(mon, GF_ACID);
}
static status_display_t _im_acid_display(plr_tim_ptr timer)
{
    return status_display_create("ImmAcid", "IAc", TERM_L_GREEN);
}
static plr_tim_info_ptr _im_acid(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_IM_ACID, "Immunity to Acid");
    info->desc = "You are immune to acid.";
    info->on_f = _im_acid_on;
    info->off_f = _im_acid_off;
    info->calc_bonuses_f = _im_acid_bonus;
    info->flags_f = _im_acid_flags;
    info->dispel_check_f = _im_acid_dispel;
    info->status_display_f = _im_acid_display;
    return info;
}

/************************************************************************
 * T_IM_ELEC
 ************************************************************************/
static bool _im_elec_on(plr_tim_ptr timer)
{
    plr_tim_remove(T_IM_ACID);
    plr_tim_remove(T_IM_FIRE);
    plr_tim_remove(T_IM_COLD);
    msg_print("You feel immune to electricity!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _im_elec_off(plr_tim_ptr timer)
{
    msg_print("You are no longer immune to electricity.");
    plr->update |= PU_BONUS;
}
static void _im_elec_bonus(plr_tim_ptr timer)
{
    res_add_immune(GF_ELEC);
}
static void _im_elec_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_IM_(GF_ELEC));
}
static bool _im_elec_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    return mon_has_breath(mon, GF_ELEC);
}
static status_display_t _im_elec_display(plr_tim_ptr timer)
{
    return status_display_create("ImmElec", "IEl", TERM_L_BLUE);
}
static plr_tim_info_ptr _im_elec(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_IM_ELEC, "Immunity to Electricity");
    info->desc = "You are immune to electricity.";
    info->on_f = _im_elec_on;
    info->off_f = _im_elec_off;
    info->calc_bonuses_f = _im_elec_bonus;
    info->flags_f = _im_elec_flags;
    info->dispel_check_f = _im_elec_dispel;
    info->status_display_f = _im_elec_display;
    return info;
}

/************************************************************************
 * T_IM_FIRE
 ************************************************************************/
static bool _im_fire_on(plr_tim_ptr timer)
{
    plr_tim_remove(T_IM_ACID);
    plr_tim_remove(T_IM_ELEC);
    plr_tim_remove(T_IM_COLD);
    msg_print("You feel immune to fire!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _im_fire_off(plr_tim_ptr timer)
{
    msg_print("You are no longer immune to fire.");
    plr->update |= PU_BONUS;
}
static void _im_fire_bonus(plr_tim_ptr timer)
{
    res_add_immune(GF_FIRE);
}
static void _im_fire_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_IM_(GF_FIRE));
}
static bool _im_fire_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    return mon_has_breath(mon, GF_FIRE);
}
static status_display_t _im_fire_display(plr_tim_ptr timer)
{
    return status_display_create("ImmFire", "IFi", TERM_L_RED);
}
static plr_tim_info_ptr _im_fire(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_IM_FIRE, "Immunity to Fire");
    info->desc = "You are immune to fire.";
    info->on_f = _im_fire_on;
    info->off_f = _im_fire_off;
    info->calc_bonuses_f = _im_fire_bonus;
    info->flags_f = _im_fire_flags;
    info->dispel_check_f = _im_fire_dispel;
    info->status_display_f = _im_fire_display;
    return info;
}

/************************************************************************
 * T_IM_COLD
 ************************************************************************/
static bool _im_cold_on(plr_tim_ptr timer)
{
    plr_tim_remove(T_IM_ACID);
    plr_tim_remove(T_IM_ELEC);
    plr_tim_remove(T_IM_FIRE);
    msg_print("You feel immune to cold!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _im_cold_off(plr_tim_ptr timer)
{
    msg_print("You are no longer immune to cold.");
    plr->update |= PU_BONUS;
}
static void _im_cold_bonus(plr_tim_ptr timer)
{
    res_add_immune(GF_COLD);
}
static void _im_cold_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_IM_(GF_COLD));
}
static bool _im_cold_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    return mon_has_breath(mon, GF_COLD);
}
static status_display_t _im_cold_display(plr_tim_ptr timer)
{
    return status_display_create("ImmCold", "ICo", TERM_WHITE);
}
static plr_tim_info_ptr _im_cold(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_IM_COLD, "Immunity to Cold");
    info->desc = "You are immune to cold.";
    info->on_f = _im_cold_on;
    info->off_f = _im_cold_off;
    info->calc_bonuses_f = _im_cold_bonus;
    info->flags_f = _im_cold_flags;
    info->dispel_check_f = _im_cold_dispel;
    info->status_display_f = _im_cold_display;
    return info;
}

/************************************************************************
 * T_INFRAVISION
 ************************************************************************/
static bool _infravision_on(plr_tim_ptr timer)
{
    msg_print("Your eyes begin to tingle!");
    plr->update |= PU_BONUS | PU_MONSTERS;
    return TRUE;
}
static void _infravision_off(plr_tim_ptr timer)
{
    msg_print("Your eyes stop tingling.");
    plr->update |= PU_BONUS | PU_MONSTERS;
}
static void _infravision_bonus(plr_tim_ptr timer)
{
    plr->see_infra += 3;
}
static void _infravision_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_INFRA);
}
static status_display_t _infravision_display(plr_tim_ptr timer)
{
    return status_display_create("Infr", "If", TERM_L_RED);
}
static plr_tim_info_ptr _infravision(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_INFRAVISION, "Infravision");
    info->desc = "You have enhanced infravision.";
    info->on_f = _infravision_on;
    info->off_f = _infravision_off;
    info->calc_bonuses_f = _infravision_bonus;
    info->flags_f = _infravision_flags;
    info->status_display_f = _infravision_display;
    return info;
}

/************************************************************************
 * T_INV_PROT
 ************************************************************************/
static bool _inv_prot_on(plr_tim_ptr timer)
{
    if (plr->pclass == CLASS_ROGUE)
        msg_print("You feel your loot is safe.");
    else
        msg_print("Your inventory seems safer now.");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _inv_prot_off(plr_tim_ptr timer)
{
    if (plr->pclass == CLASS_ROGUE)
        msg_print("Your loot feels exposed once again.");
    else
        msg_print("Your inventory is no longer protected.");
    plr->update |= PU_BONUS;
}
static void _inv_prot_bonus(plr_tim_ptr timer)
{
    plr->inven_prot = TRUE;
}
static status_display_t _inv_prot_display(plr_tim_ptr timer)
{
    return status_display_create("InvProt", "Ip", TERM_L_BLUE);
}
static plr_tim_info_ptr _inv_prot(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_INV_PROT, "Inventory Protection");
    info->desc = "Your pack is protected from the elements.";
    info->on_f = _inv_prot_on;
    info->off_f = _inv_prot_off;
    info->calc_bonuses_f = _inv_prot_bonus;
    info->status_display_f = _inv_prot_display;
    return info;
}

/************************************************************************
 * T_INVULN
 ************************************************************************/
static bool _invuln_on(plr_tim_ptr timer)
{
    msg_print("Invulnerability!");
    virtue_add(VIRTUE_UNLIFE, -2);
    virtue_add(VIRTUE_HONOUR, -2);
    virtue_add(VIRTUE_SACRIFICE, -3);
    virtue_add(VIRTUE_VALOUR, -5);
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _invuln_off(plr_tim_ptr timer)
{
    msg_print("Your shield of Invulnerability collapses!");
    plr->energy_need += ENERGY_NEED();
    plr->update |= PU_BONUS;
}
static void _invuln_tick(plr_tim_ptr timer)
{
    /* invulnerability is a fixed damage shield. timer->count is the
     * amount of damage that can still be absorbed. cf take_hit */
    if (timer->count > 0) /* paranoia */
    {
        int amt = (timer->count + 9)/10; /* fast tick timer */
        int min = plr->mhp/20;

        if (amt < min) amt = min;
        if (amt > timer->count) amt = timer->count;

        if (energy_use) /* e.g. fast walking ninjas */
            amt = amt * energy_use / 100;

        timer->count -= amt;
    }
}
static void _invuln_bonus(plr_tim_ptr timer)
{
    res_add_immune(GF_FEAR);
}
static void _invuln_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_IM_(GF_FEAR));
}
static status_display_t _invuln_display(plr_tim_ptr timer)
{
    return status_display_create("Invuln", "Iv", TERM_YELLOW);
}
static plr_tim_info_ptr _invuln(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_INVULN, "Invulnerability");
    info->desc = "You are invulnerable.";
    info->on_f = _invuln_on;
    info->off_f = _invuln_off;
    info->tick_f = _invuln_tick;
    info->calc_bonuses_f = _invuln_bonus;
    info->flags_f = _invuln_flags;
    info->status_display_f = _invuln_display;
    info->flags = TF_FAST_TICK;
    info->dispel_prob = 100;
    return info;
}

/************************************************************************
 * T_KUTAR_EXPAND
 ************************************************************************/
static bool _kutar_expand_on(plr_tim_ptr timer)
{
    msg_print("Your body expands horizontally.");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _kutar_expand_off(plr_tim_ptr timer)
{
    msg_print("Your body returns to normal.");
    plr->update |= PU_BONUS;
}
static void _kutar_expand_bonus(plr_tim_ptr timer)
{
    plr_bonus_ac(10 + 40*plr->lev/50);
    plr->vuln_magic = TRUE;
}
static status_display_t _kutar_expand_display(plr_tim_ptr timer)
{
    return status_display_create("Expand", "Eh", TERM_L_UMBER);
}
static plr_tim_info_ptr _kutar_expand(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_KUTAR_EXPAND, "Expand Horizontally");
    info->desc = "You have expanded horizontally.";
    info->on_f = _kutar_expand_on;
    info->off_f = _kutar_expand_off;
    info->calc_bonuses_f = _kutar_expand_bonus;
    info->status_display_f = _kutar_expand_display;
    return info;
}

/************************************************************************
 * T_LEVITATION
 ************************************************************************/
static bool _levitation_on(plr_tim_ptr timer)
{
    msg_print("You begin to fly!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _levitation_off(plr_tim_ptr timer)
{
    msg_print("You stop flying.");
    plr->update |= PU_BONUS;
}
static void _levitation_bonus(plr_tim_ptr timer)
{
    plr->levitation = TRUE;
}
static void _levitation_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_LEVITATION);
}
static status_display_t _levitation_display(plr_tim_ptr timer)
{
    return status_display_create("Levit", "Lv", TERM_L_BLUE);
}
static plr_tim_info_ptr _levitation(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_LEVITATION, "Levitation");
    info->desc = "You are flying.";
    info->on_f = _levitation_on;
    info->off_f = _levitation_off;
    info->calc_bonuses_f = _levitation_bonus;
    info->flags_f = _levitation_flags;
    info->status_display_f = _levitation_display;
    return info;
}

/************************************************************************
 * T_LIGHT_SPEED
 ************************************************************************/
static bool _light_speed_on(plr_tim_ptr timer)
{
    msg_print("You feel yourself moving <color:y>unbelievably fast</color>!");
    virtue_add(VIRTUE_PATIENCE, -1);
    virtue_add(VIRTUE_DILIGENCE, 1);
    plr->update |= PU_BONUS;
    plr->redraw |= PR_EFFECTS;
    return TRUE;
}
static void _light_speed_off(plr_tim_ptr timer)
{
    msg_print("You feel yourself <color:U>slow down</color>.");
    plr->update |= PU_BONUS;
    plr->redraw |= PR_EFFECTS;
}
static void _light_speed_bonus(plr_tim_ptr timer)
{
    if (!plr->riding)
        plr->pspeed += 500; /* calc_bonuses will trim it later */
}
static void _light_speed_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SPEED);
}
static bool _light_speed_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    return mon->mspeed < 25;
}
static status_display_t _light_speed_display(plr_tim_ptr timer)
{
    return status_display_create("*FAST*", "Fs", TERM_YELLOW);
}
static plr_tim_info_ptr _light_speed(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_LIGHT_SPEED, "Light Speed");
    info->desc = "You are moving unbelievably fast.";
    info->on_f = _light_speed_on;
    info->off_f = _light_speed_off;
    info->calc_bonuses_f = _light_speed_bonus;
    info->flags_f = _light_speed_flags;
    info->dispel_check_f = _light_speed_dispel;
    info->status_display_f = _light_speed_display;
    info->flags = TF_FAST_TICK;
    return info;
}

/************************************************************************
 * T_MAGICAL_ARMOR
 ************************************************************************/
static bool _magical_armor_on(plr_tim_ptr timer)
{
    msg_print("You feel more resistant to magic.");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _magical_armor_off(plr_tim_ptr timer)
{
    msg_print("You feel less resistant to magic.");
    plr->update |= PU_BONUS;
}
static void _magical_armor_bonus(plr_tim_ptr timer)
{
    plr->res_magic = TRUE;
    plr_bonus_ac(10 + 40*plr->lev/50);
    res_add(GF_BLIND);
    res_add(GF_CONF);
    plr->reflect = TRUE;
    plr->free_act++;
    plr->levitation = TRUE;
}
static void _magical_armor_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_BLIND));
    add_flag(flgs, OF_RES_(GF_CONF));
    add_flag(flgs, OF_REFLECT);
    add_flag(flgs, OF_FREE_ACT);
    add_flag(flgs, OF_LEVITATION);
}
static status_display_t _magical_armor_display(plr_tim_ptr timer)
{
    return status_display_create("MgcArm", "Md", TERM_YELLOW);
}
static plr_tim_info_ptr _magical_armor(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_MAGICAL_ARMOR, "Magical Armor");
    info->desc = "Your defenses are magically enhanced.";
    info->on_f = _magical_armor_on;
    info->off_f = _magical_armor_off;
    info->calc_bonuses_f = _magical_armor_bonus;
    info->flags_f = _magical_armor_flags;
    info->status_display_f = _magical_armor_display;
    info->dispel_prob = 100;
    return info;
}

/************************************************************************
 * T_MULTISHADOW
 ************************************************************************/
static bool _multishadow_on(plr_tim_ptr timer)
{
    msg_print("A shadow of yourself appears nearby.");
    return TRUE;
}
static void _multishadow_off(plr_tim_ptr timer)
{
    msg_print("Your shadow disappears.");
}
static status_display_t _multishadow_display(plr_tim_ptr timer)
{
    return status_display_create("MltShdw", "Ms", TERM_L_BLUE);
}
static plr_tim_info_ptr _multishadow(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_MULTISHADOW, "Multi-Shadow");
    info->desc = "There is a shadow image of yourself nearby occasionally drawing monster attacks.";
    info->on_f = _multishadow_on;
    info->off_f = _multishadow_off;
    info->status_display_f = _multishadow_display;
    info->dispel_prob = 100;
    return info;
}

/************************************************************************
 * T_NO_SPELLS
 ************************************************************************/
static bool _no_spells_on(plr_tim_ptr timer)
{
    msg_print("You feel your magic blocked.");
    return TRUE;
}
static void _no_spells_off(plr_tim_ptr timer)
{
    msg_print("You feel your magic return.");
}
static status_display_t _no_spells_display(plr_tim_ptr timer)
{
    return status_display_create("NoSpells", "Ns", TERM_VIOLET);
}
static plr_tim_info_ptr _no_spells(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_NO_SPELLS, "No Spells");
    info->desc = "Your magic is blocked.";
    info->on_f = _no_spells_on;
    info->off_f = _no_spells_off;
    info->status_display_f = _no_spells_display;
    info->flags = TF_IGNORE | TF_NO_DISPEL | TF_FAST_TICK;
    return info;
}

/************************************************************************
 * T_PARALYZED
 ************************************************************************/
static bool _paralyzed_on(plr_tim_ptr timer)
{
    if (!(timer->flags & TF_NO_MSG))
        msg_print("<color:v>You are paralyzed!</color>");
    plr->redraw |= PR_EFFECTS;
    return TRUE;
}
static void _paralyzed_off(plr_tim_ptr timer)
{
    if (!(timer->flags & TF_NO_MSG))
        msg_print("<color:B>You can move again.</color>");
    plr->redraw |= PR_EFFECTS;
}
static void _paralyzed_tick(plr_tim_ptr timer)
{
    do { timer->count--; }
    while (timer->count > 0 && !(timer->flags & TF_NO_MSG) && free_act_save_p(cave->dun_lvl/2));
    /* XXX TF_NO_MSG implies Repose of The Dead, which should not gain quick recovery */
}
static void _paralyzed_display(plr_tim_ptr timer, doc_ptr doc)
{
    doc_insert(doc, "<color:r>Paralyzed</color>\n");
}
static plr_tim_info_ptr _paralyzed(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_PARALYZED, "Paralysis");
    info->desc = "You are paralyzed.";
    info->on_f = _paralyzed_on;
    info->off_f = _paralyzed_off;
    info->tick_f = _paralyzed_tick;
    info->prt_effects_f = _paralyzed_display;
    info->flags = TF_IGNORE | TF_NO_DISPEL | TF_FAST_TICK;
    return info;
}

/************************************************************************
 * T_PASSWALL
 ************************************************************************/
static bool _passwall_on(plr_tim_ptr timer)
{
    msg_print("You become ethereal.");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _passwall_off(plr_tim_ptr timer)
{
    msg_print("You are no longer ethereal.");
    plr->update |= PU_BONUS;
}
static void _passwall_bonus(plr_tim_ptr timer)
{
    plr->pass_wall = TRUE;
    plr->no_passwall_dam = TRUE;
}
static status_display_t _passwall_display(plr_tim_ptr timer)
{
    return status_display_create("PassWall", "Pw", TERM_SLATE);
}
static plr_tim_info_ptr _passwall(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_PASSWALL, "Pass Wall");
    info->desc = "You can move through solid rock.";
    info->on_f = _passwall_on;
    info->off_f = _passwall_off;
    info->calc_bonuses_f = _passwall_bonus;
    info->status_display_f = _passwall_display;
    info->dispel_prob = 100;
    return info;
}

/************************************************************************
 * T_POISON
 ************************************************************************/
static bool _poison_on(plr_tim_ptr timer)
{
    plr->redraw |= PR_EFFECTS;
    return TRUE;
}
static void _poison_add(plr_tim_ptr timer, int amt)
{
    timer->count += amt;
    plr->redraw |= PR_EFFECTS;
}
static void _poison_off(plr_tim_ptr timer)
{
    msg_print("You are no longer poisoned.");
    plr->redraw |= PR_EFFECTS;
}
static void _poison_tick(plr_tim_ptr timer)
{
    if (timer->count > 0) /* paranoia */
    {
        int amt = (timer->count + 6)/7; /* XXX 700 counters are high-end; perhaps 1000 max */
        int min = plr->mhp/60;

        if (amt < min)
        {
            amt = min;
            if (amt > timer->count) amt = timer->count;
        }

        if (energy_use) /* e.g. fast walking ninjas */
            amt = amt * energy_use / 100;

        if (!plr_tim_find(T_INVULN))
        {
            msg_print("You are <color:G>poisoned</color>!");
            take_hit(DAMAGE_NOESCAPE, amt, "poison");
        }
        timer->count -= amt;
        plr->redraw |= PR_EFFECTS;
    }
}
static void _poison_display(plr_tim_ptr timer, doc_ptr doc)
{
    doc_printf(doc, "<color:g>Poison:</color><color:G>%d</color>\n", timer->count);
}
static plr_tim_info_ptr _poison(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_POISON, "Poison");
    info->desc = "You are poisoned.";
    info->on_f = _poison_on;
    info->add_f = _poison_add;
    info->off_f = _poison_off;
    info->tick_f = _poison_tick;
    info->prt_effects_f = _poison_display;
    info->flags = TF_BIFF | TF_FAST_TICK;
    return info;
}

/************************************************************************
 * T_PROT_EVIL
 ************************************************************************/
static bool _prot_evil_on(plr_tim_ptr timer)
{
    msg_print("You feel safe from evil!");
    return TRUE;
}
static void _prot_evil_off(plr_tim_ptr timer)
{
    msg_print("You no longer feel safe from evil.");
}
static void _prot_evil_bonus(plr_tim_ptr timer)
{
    plr->repel_evil = TRUE;
}
static status_display_t _prot_evil_display(plr_tim_ptr timer)
{
    return status_display_create("PrtEvl", "Ev", TERM_YELLOW);
}
static plr_tim_info_ptr _prot_evil(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_PROT_EVIL, "Protection from Evil");
    info->desc = "You are protected from the melee attacks of evil monsters.";
    info->on_f = _prot_evil_on;
    info->off_f = _prot_evil_off;
    info->calc_bonuses_f = _prot_evil_bonus;
    info->status_display_f = _prot_evil_display;
    return info;
}

/************************************************************************
 * T_PROT_GOOD
 ************************************************************************/
static bool _prot_good_on(plr_tim_ptr timer)
{
    msg_print("You feel safe from good!");
    return TRUE;
}
static void _prot_good_off(plr_tim_ptr timer)
{
    msg_print("You no longer feel safe from good.");
}
static void _prot_good_bonus(plr_tim_ptr timer)
{
    plr->repel_good = TRUE;
}
static status_display_t _prot_good_display(plr_tim_ptr timer)
{
    return status_display_create("PrtGood", "Gd", TERM_L_DARK);
}
static plr_tim_info_ptr _prot_good(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_PROT_GOOD, "Protection from Good");
    info->desc = "You are protected from the melee attacks of good monsters.";
    info->on_f = _prot_good_on;
    info->off_f = _prot_good_off;
    info->calc_bonuses_f = _prot_good_bonus;
    info->status_display_f = _prot_good_display;
    return info;
}

/************************************************************************
 * T_REFLECT
 ************************************************************************/
static bool _reflect_on(plr_tim_ptr timer)
{
    msg_print("Your body becomes smooth.");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _reflect_off(plr_tim_ptr timer)
{
    msg_print("Your body is no longer smooth.");
    plr->update |= PU_BONUS;
}
static void _reflect_bonus(plr_tim_ptr timer)
{
    plr->reflect = TRUE;
}
static void _reflect_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_REFLECT);
}
static status_display_t _reflect_display(plr_tim_ptr timer)
{
    return status_display_create("Reflect", "Rf", TERM_SLATE);
}
static plr_tim_info_ptr _reflect(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_REFLECT, "Reflection");
    info->desc = "You are reflecting bolt attacks.";
    info->on_f = _reflect_on;
    info->off_f = _reflect_off;
    info->calc_bonuses_f = _reflect_bonus;
    info->flags_f = _reflect_flags;
    info->status_display_f = _reflect_display;
    return info;
}

/************************************************************************
 * T_REGEN
 ************************************************************************/
static bool _regen_on(plr_tim_ptr timer)
{
    msg_print("You feel yourself regenerating quickly!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _regen_off(plr_tim_ptr timer)
{
    msg_print("You feel yourself regenerating slowly.");
    plr->update |= PU_BONUS;
}
static void _regen_bonus(plr_tim_ptr timer)
{
    int amt = timer->parm;
    if (!amt) amt = 100;
    plr->regen += amt;
}
static void _regen_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_REGEN);
}
static status_display_t _regen_display(plr_tim_ptr timer)
{
    return status_display_create("Regen", "Rg", TERM_L_BLUE);
}
static plr_tim_info_ptr _regen(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_REGEN, "Regeneration");
    info->desc = "You are regenerating more quickly.";
    info->on_f = _regen_on;
    info->off_f = _regen_off;
    info->calc_bonuses_f = _regen_bonus;
    info->flags_f = _regen_flags;
    info->status_display_f = _regen_display;
    return info;
}

/************************************************************************
 * T_RES_ACID
 ************************************************************************/
static bool _res_acid_on(plr_tim_ptr timer)
{
    msg_print("You feel resistant to acid!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _res_acid_off(plr_tim_ptr timer)
{
    msg_print("You feel less resistant to acid.");
    plr->update |= PU_BONUS;
}
static void _res_acid_bonus(plr_tim_ptr timer)
{
    res_add(GF_ACID);
}
static void _res_acid_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_ACID));
}
static bool _res_acid_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    return mon_has_breath(mon, GF_ACID) && res_pct(GF_ACID) < 75;
}
static status_display_t _res_acid_display(plr_tim_ptr timer)
{
    return status_display_create("Acid", "Ac", TERM_GREEN);
}
static plr_tim_info_ptr _res_acid(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_RES_ACID, "Resist Acid");
    info->desc = "You are resistant to acid.";
    info->on_f = _res_acid_on;
    info->off_f = _res_acid_off;
    info->calc_bonuses_f = _res_acid_bonus;
    info->flags_f = _res_acid_flags;
    info->dispel_check_f = _res_acid_dispel;
    info->status_display_f = _res_acid_display;
    return info;
}

/************************************************************************
 * T_RES_ELEC
 ************************************************************************/
static bool _res_elec_on(plr_tim_ptr timer)
{
    msg_print("You feel resistant to electricity!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _res_elec_off(plr_tim_ptr timer)
{
    msg_print("You feel less resistant to electricity.");
    plr->update |= PU_BONUS;
}
static void _res_elec_bonus(plr_tim_ptr timer)
{
    res_add(GF_ELEC);
}
static void _res_elec_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_ELEC));
}
static bool _res_elec_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    return mon_has_breath(mon, GF_ELEC) && res_pct(GF_ELEC) < 75;
}
static status_display_t _res_elec_display(plr_tim_ptr timer)
{
    return status_display_create("Elec", "El", TERM_BLUE);
}
static plr_tim_info_ptr _res_elec(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_RES_ELEC, "Resist Elec");
    info->desc = "You are resistant to electricity.";
    info->on_f = _res_elec_on;
    info->off_f = _res_elec_off;
    info->calc_bonuses_f = _res_elec_bonus;
    info->flags_f = _res_elec_flags;
    info->dispel_check_f = _res_elec_dispel;
    info->status_display_f = _res_elec_display;
    return info;
}

/************************************************************************
 * T_RES_FIRE
 ************************************************************************/
static bool _res_fire_on(plr_tim_ptr timer)
{
    msg_print("You feel resistant to fire!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _res_fire_off(plr_tim_ptr timer)
{
    msg_print("You feel less resistant to fire.");
    plr->update |= PU_BONUS;
}
static void _res_fire_bonus(plr_tim_ptr timer)
{
    res_add(GF_FIRE);
}
static void _res_fire_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_FIRE));
}
static bool _res_fire_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    return mon_has_breath(mon, GF_FIRE) && res_pct(GF_FIRE) < 75;
}
static status_display_t _res_fire_display(plr_tim_ptr timer)
{
    return status_display_create("Fire", "Fi", TERM_RED);
}
static plr_tim_info_ptr _res_fire(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_RES_FIRE, "Resist Fire");
    info->desc = "You are resistant to fire.";
    info->on_f = _res_fire_on;
    info->off_f = _res_fire_off;
    info->calc_bonuses_f = _res_fire_bonus;
    info->flags_f = _res_fire_flags;
    info->dispel_check_f = _res_fire_dispel;
    info->status_display_f = _res_fire_display;
    return info;
}

/************************************************************************
 * T_RES_COLD
 ************************************************************************/
static bool _res_cold_on(plr_tim_ptr timer)
{
    msg_print("You feel resistant to cold!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _res_cold_off(plr_tim_ptr timer)
{
    msg_print("You feel less resistant to cold.");
    plr->update |= PU_BONUS;
}
static void _res_cold_bonus(plr_tim_ptr timer)
{
    res_add(GF_COLD);
}
static void _res_cold_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_COLD));
}
static bool _res_cold_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    return mon_has_breath(mon, GF_COLD) && res_pct(GF_COLD) < 75;
}
static status_display_t _res_cold_display(plr_tim_ptr timer)
{
    return status_display_create("Cold", "Co", TERM_SLATE);
}
static plr_tim_info_ptr _res_cold(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_RES_COLD, "Resist Cold");
    info->desc = "You are resistant to cold.";
    info->on_f = _res_cold_on;
    info->off_f = _res_cold_off;
    info->calc_bonuses_f = _res_cold_bonus;
    info->flags_f = _res_cold_flags;
    info->dispel_check_f = _res_cold_dispel;
    info->status_display_f = _res_cold_display;
    return info;
}

/************************************************************************
 * T_RES_POIS
 ************************************************************************/
static bool _res_pois_on(plr_tim_ptr timer)
{
    msg_print("You feel resistant to poison!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _res_pois_off(plr_tim_ptr timer)
{
    msg_print("You feel less resistant to poison.");
    plr->update |= PU_BONUS;
}
static void _res_pois_bonus(plr_tim_ptr timer)
{
    res_add(GF_POIS);
}
static void _res_pois_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_POIS));
}
static bool _res_pois_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    return (mon_has_breath(mon, GF_POIS) || mon_has_breath(mon, GF_NUKE)) && res_pct(GF_POIS) < 75;
}
static status_display_t _res_pois_display(plr_tim_ptr timer)
{
    return status_display_create("Pois", "Po", TERM_GREEN);
}
static plr_tim_info_ptr _res_pois(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_RES_POIS, "Resist Poison");
    info->desc = "You are resistant to poison.";
    info->on_f = _res_pois_on;
    info->off_f = _res_pois_off;
    info->calc_bonuses_f = _res_pois_bonus;
    info->flags_f = _res_pois_flags;
    info->dispel_check_f = _res_pois_dispel;
    info->status_display_f = _res_pois_display;
    return info;
}

/************************************************************************
 * T_RES_CONF
 ************************************************************************/
static bool _res_conf_on(plr_tim_ptr timer)
{
    msg_print("You feel resistant to confusion!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _res_conf_off(plr_tim_ptr timer)
{
    msg_print("You feel less resistant to confusion.");
    plr->update |= PU_BONUS;
}
static void _res_conf_bonus(plr_tim_ptr timer)
{
    res_add(GF_CONF);
}
static void _res_conf_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_CONF));
}
static status_display_t _res_conf_display(plr_tim_ptr timer)
{
    return status_display_create("Conf", "Cf", TERM_L_UMBER);
}
static plr_tim_info_ptr _res_conf(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_RES_CONF, "Resist Confusion");
    info->desc = "You are resistant to conf.";
    info->on_f = _res_conf_on;
    info->off_f = _res_conf_off;
    info->calc_bonuses_f = _res_conf_bonus;
    info->flags_f = _res_conf_flags;
    info->status_display_f = _res_conf_display;
    return info;
}

/************************************************************************
 * T_RES_NETHER
 ************************************************************************/
static bool _res_nether_on(plr_tim_ptr timer)
{
    msg_print("You feel resistant to nether!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _res_nether_off(plr_tim_ptr timer)
{
    msg_print("You feel less resistant to nether.");
    plr->update |= PU_BONUS;
}
static void _res_nether_bonus(plr_tim_ptr timer)
{
    res_add(GF_NETHER);
}
static void _res_nether_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_NETHER));
}
static status_display_t _res_nether_display(plr_tim_ptr timer)
{
    return status_display_create("Neth", "Nt", TERM_L_DARK);
}
static plr_tim_info_ptr _res_nether(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_RES_NETHER, "Resist Nether");
    info->desc = "You are resistant to nether.";
    info->on_f = _res_nether_on;
    info->off_f = _res_nether_off;
    info->calc_bonuses_f = _res_nether_bonus;
    info->flags_f = _res_nether_flags;
    info->status_display_f = _res_nether_display;
    return info;
}

/************************************************************************
 * T_RES_DISEN
 ************************************************************************/
static bool _res_disen_on(plr_tim_ptr timer)
{
    msg_print("You feel resistant to disenchantment!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _res_disen_off(plr_tim_ptr timer)
{
    msg_print("You feel less resistant to disenchantment.");
    plr->update |= PU_BONUS;
}
static void _res_disen_bonus(plr_tim_ptr timer)
{
    res_add(GF_DISEN);
}
static void _res_disen_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_DISEN));
}
static status_display_t _res_disen_display(plr_tim_ptr timer)
{
    return status_display_create("Disen", "Ds", TERM_VIOLET);
}
static plr_tim_info_ptr _res_disen(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_RES_DISEN, "Resist Disenchantment");
    info->desc = "You are resistant to disenchantment.";
    info->on_f = _res_disen_on;
    info->off_f = _res_disen_off;
    info->calc_bonuses_f = _res_disen_bonus;
    info->flags_f = _res_disen_flags;
    info->status_display_f = _res_disen_display;
    return info;
}

/************************************************************************
 * T_RES_TIME
 ************************************************************************/
static bool _res_time_on(plr_tim_ptr timer)
{
    msg_print("You feel resistant to time!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _res_time_off(plr_tim_ptr timer)
{
    msg_print("You feel less resistant to time.");
    plr->update |= PU_BONUS;
}
static void _res_time_bonus(plr_tim_ptr timer)
{
    res_add(GF_TIME);
}
static void _res_time_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_TIME));
}
static status_display_t _res_time_display(plr_tim_ptr timer)
{
    return status_display_create("Time", "Tm", TERM_L_BLUE);
}
static plr_tim_info_ptr _res_time(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_RES_TIME, "Resist Time");
    info->desc = "You are resistant to time.";
    info->on_f = _res_time_on;
    info->off_f = _res_time_off;
    info->calc_bonuses_f = _res_time_bonus;
    info->flags_f = _res_time_flags;
    info->status_display_f = _res_time_display;
    return info;
}

/************************************************************************
 * T_RES_MAGIC
 ************************************************************************/
static bool _res_magic_on(plr_tim_ptr timer)
{
    msg_print("You have been protected from magic!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _res_magic_off(plr_tim_ptr timer)
{
    msg_print("You are no longer protected from magic.");
    plr->update |= PU_BONUS;
}
static void _res_magic_bonus(plr_tim_ptr timer)
{
    plr->res_magic = TRUE;
}
static status_display_t _res_magic_display(plr_tim_ptr timer)
{
    return status_display_create("ResMag", "Rm", TERM_SLATE);
}
static plr_tim_info_ptr _res_magic(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_RES_MAGIC, "Resist Magic");
    info->desc = "You are resistant to magic.";
    info->on_f = _res_magic_on;
    info->off_f = _res_magic_off;
    info->calc_bonuses_f = _res_magic_bonus;
    info->status_display_f = _res_magic_display;
    return info;
}

/************************************************************************
 * T_REVENGE
 ************************************************************************/
static bool _revenge_on(plr_tim_ptr timer)
{
    if (plr->pclass == CLASS_BLOOD_KNIGHT)
        msg_print("You feel like bloody revenge!");
    else 
        msg_print("You feel like a keeper of commandments!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _revenge_off(plr_tim_ptr timer)
{
    if (plr->pclass == CLASS_BLOOD_KNIGHT)
        msg_print("You no longer feel like bloody revenge.");
    else 
        msg_print("You no longer feel like a keeper.");
    plr->update |= PU_BONUS;
}
static void _revenge_bonus(plr_tim_ptr timer)
{
    plr->revenge = TRUE;
}
static status_display_t _revenge_display(plr_tim_ptr timer)
{
    return status_display_create("Revenge", "Rv", TERM_VIOLET);
}
static plr_tim_info_ptr _revenge(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_REVENGE, "Revenge");
    info->desc = "Monsters that damage you are damaged in kind.";
    info->on_f = _revenge_on;
    info->off_f = _revenge_off;
    info->calc_bonuses_f = _revenge_bonus;
    info->status_display_f = _revenge_display;
    return info;
}

/************************************************************************
 * T_SEE_INVIS
 ************************************************************************/
static bool _see_invis_on(plr_tim_ptr timer)
{
    msg_print("Your eyes feel very sensitive!");
    plr->update |= PU_BONUS | PU_MONSTERS;
    return TRUE;
}
static void _see_invis_off(plr_tim_ptr timer)
{
    msg_print("Your eyes feel less sensitive.");
    plr->update |= PU_BONUS | PU_MONSTERS;
}
static void _see_invis_bonus(plr_tim_ptr timer)
{
    plr->see_inv++;
}
static void _see_invis_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SEE_INVIS);
}
static status_display_t _see_invis_display(plr_tim_ptr timer)
{
    return status_display_create("SInv", "Si", TERM_L_BLUE);
}
static plr_tim_info_ptr _see_invis(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_SEE_INVIS, "See Invisible");
    info->desc = "You can see invisible monsters.";
    info->on_f = _see_invis_on;
    info->off_f = _see_invis_off;
    info->calc_bonuses_f = _see_invis_bonus;
    info->flags_f = _see_invis_flags;
    info->status_display_f = _see_invis_display;
    return info;
}

/************************************************************************
 * T_SLOW
 ************************************************************************/
static bool _slow_on(plr_tim_ptr timer)
{
    msg_print("You feel yourself moving slower!");
    plr->update |= PU_BONUS;
    plr->redraw |= PR_EFFECTS;
    return TRUE;
}
static void _slow_off(plr_tim_ptr timer)
{
    msg_print("You feel yourself speed up.");
    plr->update |= PU_BONUS;
    plr->redraw |= PR_EFFECTS;
}
static void _slow_tick(plr_tim_ptr timer)
{
    do { timer->count--; }
    while (timer->count > 0 && free_act_save_p(cave->dun_lvl*2));
}
static void _slow_bonus(plr_tim_ptr timer)
{
    if (!plr->riding)
        plr->pspeed -= 10;
}
static void _slow_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_DEC_SPEED);
}
static plr_tim_info_ptr _slow(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_SLOW, "Slow");
    info->desc = "You are moving slower.";
    info->on_f = _slow_on;
    info->off_f = _slow_off;
    info->tick_f = _slow_tick;
    info->calc_bonuses_f = _slow_bonus;
    info->flags_f = _slow_flags;
    info->flags = TF_BIFF;
    return info;
}

/************************************************************************
 * T_STAR_REGEN
 ************************************************************************/
static bool _star_regen_on(plr_tim_ptr timer)
{
    msg_print("You feel life flowing through you!");
    return TRUE;
}
static void _star_regen_off(plr_tim_ptr timer)
{
    msg_print("You no longer feel life flowing through you.");
}
static void _star_regen_tick(plr_tim_ptr timer)
{
    int amt = timer->parm;
    if (!amt) amt = 25;
    hp_player(amt);
    timer->count--;
}
static void _star_regen_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_REGEN);
}
static status_display_t _star_regen_display(plr_tim_ptr timer)
{
    return status_display_create("*Regen*", "Rg", TERM_L_BLUE);
}
static plr_tim_info_ptr _star_regen(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_STAR_REGEN, "*Regeneration*");
    info->desc = "You are regenerating incredibly fast.";
    info->on_f = _star_regen_on;
    info->off_f = _star_regen_off;
    info->tick_f = _star_regen_tick;
    info->flags_f = _star_regen_flags;
    info->status_display_f = _star_regen_display;
    info->flags = TF_FAST_TICK;
    return info;
}

/************************************************************************
 * T_STEALTH
 ************************************************************************/
static bool _stealth_on(plr_tim_ptr timer)
{
    if (plr->pclass == CLASS_ROGUE || plr->pclass == CLASS_SKILLMASTER)
        msg_print("You begin to tread softly.");
    else if (plr->pclass == CLASS_NECROMANCER || plr->pclass == CLASS_RUNE_KNIGHT)
        msg_print("You are cloaked in darkness.");
    else
        msg_print("You begin to stalk your prey.");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _stealth_off(plr_tim_ptr timer)
{
    if (plr->pclass == CLASS_ROGUE || plr->pclass == CLASS_SKILLMASTER)
        msg_print("You no longer tread softly.");
    else if (plr->pclass == CLASS_NECROMANCER || plr->pclass == CLASS_RUNE_KNIGHT)
        msg_print("You are no longer cloaked in darkness.");
    else
        msg_print("You no longer stalk your prey.");
    plr->update |= PU_BONUS;
}
static void _stealth_bonus(plr_tim_ptr timer)
{
    plr->skills.stl += 3 + plr->lev/5;
}
static void _stealth_flags(plr_tim_ptr timer, u32b flags[OF_ARRAY_SIZE])
{
    add_flag(flags, OF_STEALTH);
}
static status_display_t _stealth_display(plr_tim_ptr timer)
{
    return status_display_create("Stealth", "Stl", TERM_L_DARK);
}
static plr_tim_info_ptr _stealth(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_STEALTH, "Stealth");
    info->desc = "You are moving with enhanced stealth.";
    info->on_f = _stealth_on;
    info->off_f = _stealth_off;
    info->calc_bonuses_f = _stealth_bonus;
    info->flags_f = _stealth_flags;
    info->status_display_f = _stealth_display;
    return info;
}

/************************************************************************
 * T_STONE_SKIN
 ************************************************************************/
static bool _stone_skin_on(plr_tim_ptr timer)
{
    if (plr_tim_find(T_ULT_RES)) return FALSE;
    msg_print("Your skin turns to stone.");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _stone_skin_off(plr_tim_ptr timer)
{
    if (plr_tim_find(T_ULT_RES)) return;
    msg_print("Your skin returns to normal.");
    plr->update |= PU_BONUS;
}
static void _stone_skin_bonus(plr_tim_ptr timer)
{
    plr_bonus_ac(10 + plr_prorata_level(40));
}
static status_display_t _stone_skin_display(plr_tim_ptr timer)
{
    return status_display_create("StnSkn", "Ss", TERM_WHITE);
}
static plr_tim_info_ptr _stone_skin(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_STONE_SKIN, "Stone Skin");
    info->desc = "Your stin has turned to stone.";
    info->on_f = _stone_skin_on;
    info->off_f = _stone_skin_off;
    info->calc_bonuses_f = _stone_skin_bonus;
    info->status_display_f = _stone_skin_display;
    info->dispel_prob = 50;
    return info;
}

/************************************************************************
 * T_STUN:
 * This is a difficult timer, since it needs to emit messages whenever
 * the stun level changes. We use a custom add_f hook to achieve this.
 ************************************************************************/
typedef struct { int level; cptr name; cptr msg; byte attr; } _stun_info_t;
static _stun_info_t _stun_info(int stun)
{
    _stun_info_t result = {0};
    if (stun >= STUN_KNOCKED_OUT)
    {
        result.level = STUN_KNOCKED_OUT;
        result.name = "Knocked Out";  /* <== PR_EFFECTS */
        result.msg = "knocked out";   /* <== You have been %s */
        result.attr = TERM_VIOLET;
    }
    else if (stun >= STUN_MASSIVE)
    {
        result.level = STUN_MASSIVE;
        result.name = "Massive Stun";
        result.msg = "massively stunned";
        result.attr = TERM_RED;
    }
    else if (stun >= STUN_HEAVY)
    {
        result.level = STUN_HEAVY;
        result.name = "Heavy Stun";
        result.msg = "heavily stunned";
        result.attr = TERM_L_RED;
    }
    else if (stun >= STUN_MODERATE)
    {
        result.level = STUN_MODERATE;
        result.name = "Stun";
        result.msg = "stunned";
        result.attr = TERM_ORANGE;
    }
    else if (stun >= STUN_LIGHT)
    {
        result.level = STUN_LIGHT;
        result.name = "Light Stun";
        result.msg = "lightly stunned";
        result.attr = TERM_YELLOW;
    }
    else if (stun >= STUN_DAZE)
    {
        result.level = STUN_DAZE;
        result.name = "Dazed";
        result.msg = "dazed";
        result.attr = TERM_L_UMBER;
    }
    else
    {
        assert(result.level == STUN_NONE);
    }
    return result;
}
static void _stun_change(int old, int new)
{
    _stun_info_t old_stun = _stun_info(old);
    _stun_info_t new_stun = _stun_info(new);
    if (new_stun.level > old_stun.level)
    {
        msg_format("You have been <color:%c>%s</color>.", attr_to_attr_char(new_stun.attr), new_stun.msg);
        if (randint1(1000) < new || one_in_(16))
        {
            msg_print("A vicious blow hits your head.");
            if (one_in_(3))
            {
                if (!plr->sustain_int) do_dec_stat(A_INT);
                if (!plr->sustain_wis) do_dec_stat(A_WIS);
            }
            else if (one_in_(2))
            {
                if (!plr->sustain_int) do_dec_stat(A_INT);
            }
            else
            {
                if (!plr->sustain_wis) do_dec_stat(A_WIS);
            }
        }
        plr->redraw |= PR_EFFECTS | PR_HEALTH_BARS;
    }
    else if (new_stun.level < old_stun.level)
    {
        if (old_stun.level == STUN_KNOCKED_OUT && new_stun.level > STUN_NONE)
            msg_format("You are no longer <color:%c>%s</color>.", attr_to_attr_char(old_stun.attr), old_stun.msg); 

        if  (new_stun.level == STUN_NONE)
        {
            msg_format("You are no longer <color:%c>%s</color>.", attr_to_attr_char(old_stun.attr), old_stun.msg);
            if (disturb_state) disturb(0, 0);
        }
        plr->redraw |= PR_EFFECTS | PR_HEALTH_BARS;
    }
}
static bool _stun_on(plr_tim_ptr timer)
{
    _stun_change(0, timer->count);
    return TRUE;
}
static void _stun_add(plr_tim_ptr timer, int amt)
{
    if (amt > 0 && timer->count >= STUN_KNOCKED_OUT) return;
    _stun_change(timer->count, timer->count + amt);
    timer->count += amt;
    /* XXX stunning is unusual ... cf _high_priest_timer_on for chanting realms
     * that wish to make a STUN save every time the plr stun level increases.
     * Note that light stuns are *extremely* common, so having that function
     * as an effective dispel magic is just too harsh ... XXX */
    if (amt > 0)
        _plr_hook_on(timer);
}
static void _stun_off(plr_tim_ptr timer)
{
    /* no need ... the final tick_f will handle this (but this hook is assumed to exist) */
}
static void _stun_tick(plr_tim_ptr timer)
{
    int amt = adj_con_fix[plr->stat_ind[A_CON]] + 1;
    if (amt > timer->count) amt = timer->count;
    _stun_add(timer, -amt);
}
static void _stun_display(plr_tim_ptr timer, doc_ptr doc)
{
    _stun_info_t s = _stun_info(timer->count);
    if (s.level == STUN_NONE) return; /* paranoia */
    doc_printf(doc, "<color:%c>%s</color>\n", attr_to_attr_char(s.attr), s.name);
}
static plr_tim_info_ptr _stun(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_STUN, "Stun");
    info->desc = "You are stunned.";
    info->on_f = _stun_on;
    info->add_f = _stun_add;
    info->off_f = _stun_off;
    info->tick_f = _stun_tick;
    info->prt_effects_f = _stun_display;
    info->flags = TF_BIFF;
    return info;
}

/************************************************************************
 * T_SUPERSTEALTH
 ************************************************************************/
static void _superstealth_status(void)
{
    plr->update |= PU_BONUS | PU_TORCH; /* Note: Forcing PU_TORCH is the key!!! */
    plr->update |= PU_UN_VIEW | PU_UN_LIGHT | PU_VIEW | PU_LIGHT;
    plr->redraw |= PR_EFFECTS;
}
static bool _superstealth_on(plr_tim_ptr timer)
{
    if (plr->cur_light && plr->pclass == CLASS_NECROMANCER)
        msg_print("You can hide in shadows once you remove that nasty light source!");
    else
        msg_print("You can hide in shadows!");

    _superstealth_status();
    return TRUE;
}
static void _superstealth_off(plr_tim_ptr timer)
{
    msg_print("You can no longer hide in shadows.");
    if (plr->pclass != CLASS_NINJA)
        set_superstealth(FALSE);
    _superstealth_status();
}
static void _superstealth_bonus(plr_tim_ptr timer)
{
    plr->see_nocto = DUN_VIEW_MAX;
}
static void _superstealth_flags(plr_tim_ptr timer, u32b flags[OF_ARRAY_SIZE])
{
    add_flag(flags, OF_STEALTH);
}
static status_display_t _superstealth_display(plr_tim_ptr timer)
{
    return status_display_create("Stealth", "Stl", TERM_L_BLUE);
}
static plr_tim_info_ptr _superstealth(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_SUPERSTEALTH, "Hide in Shadows");
    info->desc = "You can hide in shadows.";
    info->on_f = _superstealth_on;
    info->off_f = _superstealth_off;
    info->calc_bonuses_f = _superstealth_bonus;
    info->flags_f = _superstealth_flags;
    info->status_display_f = _superstealth_display;
    info->flags = TF_IGNORE;
    return info;
}

/************************************************************************
 * T_SUSTAIN
 ************************************************************************/
static bool _sustain_on(plr_tim_ptr timer)
{
    msg_print("You feel sustained by your supreme righteousness!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _sustain_off(plr_tim_ptr timer)
{
    msg_print("You no longer feel sustained.");
    plr->update |= PU_BONUS;
}
static void _sustain_bonus(plr_tim_ptr timer)
{
    plr->sustain_str = TRUE;
    plr->sustain_int = TRUE;
    plr->sustain_wis = TRUE;
    plr->sustain_dex = TRUE;
    plr->sustain_con = TRUE;
    plr->sustain_chr = TRUE;
    plr->hold_life++;
}
static void _sustain_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SUST_STR);
    add_flag(flgs, OF_SUST_INT);
    add_flag(flgs, OF_SUST_WIS);
    add_flag(flgs, OF_SUST_DEX);
    add_flag(flgs, OF_SUST_CON);
    add_flag(flgs, OF_SUST_CHR);
    add_flag(flgs, OF_HOLD_LIFE);
}
static status_display_t _sustain_display(plr_tim_ptr timer)
{
    return status_display_create("Sustain", "(*", TERM_YELLOW);
}
static plr_tim_info_ptr _sustain(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_SUSTAIN, "Sustaining");
    info->desc = "Your stats and life force are sustained.";
    info->on_f = _sustain_on;
    info->off_f = _sustain_off;
    info->calc_bonuses_f = _sustain_bonus;
    info->flags_f = _sustain_flags;
    info->status_display_f = _sustain_display;
    return info;
}

/************************************************************************
 * T_TELEPATHY
 ************************************************************************/
static bool _telepathy_on(plr_tim_ptr timer)
{
    msg_print("You feel your consciousness expand!");
    plr->update |= PU_BONUS | PU_MONSTERS;
    return TRUE;
}
static void _telepathy_off(plr_tim_ptr timer)
{
    msg_print("Your consciousness contracts again.");
    plr->update |= PU_BONUS | PU_MONSTERS;
}
static void _telepathy_bonus(plr_tim_ptr timer)
{
    plr->telepathy = TRUE;
}
static void _telepathy_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_TELEPATHY);
}
static status_display_t _telepathy_display(plr_tim_ptr timer)
{
    return status_display_create("Telepa", "Te", TERM_ORANGE);
}
static plr_tim_info_ptr _telepathy(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_TELEPATHY, "Telepathy");
    info->desc = "You have a telepathic awareness of nearby monsters.";
    info->on_f = _telepathy_on;
    info->off_f = _telepathy_off;
    info->calc_bonuses_f = _telepathy_bonus;
    info->flags_f = _telepathy_flags;
    info->status_display_f = _telepathy_display;
    return info;
}

/************************************************************************
 * T_ULT_RES
 ************************************************************************/
static bool _ult_res_on(plr_tim_ptr timer)
{
    msg_print("You feel resistant!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _ult_res_off(plr_tim_ptr timer)
{
    msg_print("You feel less resistant");
    plr->update |= PU_BONUS;
}
void ult_res_bonus(plr_tim_ptr timer)
{
    res_add_ultimate();
    plr->reflect = TRUE;
    plr->see_inv++;
    plr->free_act++;
    plr->hold_life++;
    plr->res_magic = TRUE;
    plr->sustain_str = TRUE;
    plr->sustain_int = TRUE;
    plr->sustain_wis = TRUE;
    plr->sustain_con = TRUE;
    plr->sustain_dex = TRUE;
    plr->sustain_chr = TRUE;
    plr_bonus_ac(75);
}
void ult_res_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_ACID));
    add_flag(flgs, OF_RES_(GF_ELEC));
    add_flag(flgs, OF_RES_(GF_FIRE));
    add_flag(flgs, OF_RES_(GF_COLD));
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_RES_(GF_LIGHT));
    add_flag(flgs, OF_RES_(GF_DARK));
    add_flag(flgs, OF_RES_(GF_CONF));
    add_flag(flgs, OF_RES_(GF_NETHER));
    add_flag(flgs, OF_RES_(GF_NEXUS));
    add_flag(flgs, OF_RES_(GF_SOUND));
    add_flag(flgs, OF_RES_(GF_SHARDS));
    add_flag(flgs, OF_RES_(GF_CHAOS));
    add_flag(flgs, OF_RES_(GF_DISEN));
    add_flag(flgs, OF_RES_(GF_FEAR));
    add_flag(flgs, OF_REFLECT);
    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_FREE_ACT);
    add_flag(flgs, OF_SEE_INVIS);
    add_flag(flgs, OF_SUST_STR);
    add_flag(flgs, OF_SUST_INT);
    add_flag(flgs, OF_SUST_WIS);
    add_flag(flgs, OF_SUST_DEX);
    add_flag(flgs, OF_SUST_CON);
    add_flag(flgs, OF_SUST_CHR);
}
static status_display_t _ult_res_display(plr_tim_ptr timer)
{
    return status_display_create("Ulitma", "Ul", TERM_YELLOW);
}
static plr_tim_info_ptr _ult_res(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_ULT_RES, "Ultimate Resistance");
    info->desc = "You are resistant to almost everything.";
    info->on_f = _ult_res_on;
    info->off_f = _ult_res_off;
    info->calc_bonuses_f = ult_res_bonus;
    info->flags_f = ult_res_flags;
    info->status_display_f = _ult_res_display;
    info->dispel_prob = 100;
    return info;
}

/************************************************************************
 * T_WEAPONMASTERY
 ************************************************************************/
static bool _weaponmastery_on(plr_tim_ptr timer)
{
    if (plr->attack_info[0].type == PAT_MONK || plr->attack_info[1].type == PAT_MONK)
        msg_print("Your fists seem more powerful!");
    else
        msg_print("Your weapon seems more powerful!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _weaponmastery_off(plr_tim_ptr timer)
{
    if (plr->attack_info[0].type == PAT_MONK || plr->attack_info[1].type == PAT_MONK)
        msg_print("Your fists return to normal.");
    else
        msg_print("Your weapon returns to normal.");
    plr->update |= PU_BONUS;
}
static void _weaponmastery_bonus(plr_tim_ptr timer)
{
    equip_xtra_might(plr->lev/23);
}
static void _weaponmastery_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    info->to_dd += plr->lev/23;
}
static status_display_t _weaponmastery_display(plr_tim_ptr timer)
{
    return status_display_create("Weapon", "Wp", TERM_L_BLUE);
}
static plr_tim_info_ptr _weaponmastery(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_WEAPONMASTERY, "Weaponmaster");
    info->desc = "The damage dice of your melee attacks are magically enhanced.";
    info->on_f = _weaponmastery_on;
    info->off_f = _weaponmastery_off;
    info->calc_bonuses_f = _weaponmastery_bonus;
    info->calc_weapon_bonuses_f = _weaponmastery_weapon_bonus;
    info->status_display_f = _weaponmastery_display;
    return info;
}

/************************************************************************
 * T_WRAITH
 ************************************************************************/
static bool _wraith_on(plr_tim_ptr timer)
{
    msg_print("You leave the physical world and turn into a wraith-being!");
    virtue_add(VIRTUE_UNLIFE, 3);
    virtue_add(VIRTUE_HONOUR, -2);
    virtue_add(VIRTUE_SACRIFICE, -2);
    virtue_add(VIRTUE_VALOUR, -5);
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _wraith_off(plr_tim_ptr timer)
{
    msg_print("You feel opaque.");
    plr->update |= PU_BONUS;
}
static void _wraith_bonus(plr_tim_ptr timer)
{
    res_add_immune(GF_DARK);
    plr->reflect = TRUE;
    plr->pass_wall = TRUE;
    plr->no_passwall_dam = TRUE;
}
static void _wraith_flags(plr_tim_ptr timer, u32b flags[OF_ARRAY_SIZE])
{
    add_flag(flags, OF_IM_(GF_DARK));
    add_flag(flags, OF_REFLECT);
}
static status_display_t _wraith_display(plr_tim_ptr timer)
{
    return status_display_create("Wraith", "Wr", TERM_L_DARK);
}
static plr_tim_info_ptr _wraith(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_WRAITH, "Wraithform");
    info->desc = "You have turned into a wraith.";
    info->on_f = _wraith_on;
    info->off_f = _wraith_off;
    info->calc_bonuses_f = _wraith_bonus;
    info->flags_f = _wraith_flags;
    info->status_display_f = _wraith_display;
    info->dispel_prob = 100;
    return info;
}

