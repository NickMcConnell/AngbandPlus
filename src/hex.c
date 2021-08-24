#include "angband.h"

#include <assert.h>

bool hex_inhale = FALSE;

/************************************************************************
 * Malediction (formerly Hex)
 *
 * We'll be using the player timer system in a creative way to track
 * and maintain current chants. Players may weave multiple chants at
 * once depending upon level. Hex is shared with the Skillmaster.
 ************************************************************************/

/************************************************************************
 * Revenge
 *
 * A dish best served cold ... we'll keep score of any wrongs we receive, 
 * of course, and then dish out appropriate punishment when the time is
 * right!
 ************************************************************************/
#define _PATIENCE 1
#define _REVENGE 2
static struct { int type, turn, amt; } _vengeance;

void hex_on_dam(int dam)
{
    if (!_vengeance.type) return;
    _vengeance.amt += dam;
}

void hex_load(savefile_ptr file)
{
    if (savefile_is_older_than(file, 7, 3, 4, 1)) return;
    _vengeance.type = savefile_read_byte(file);
    if (_vengeance.type)
    {
        _vengeance.turn = savefile_read_byte(file);
        _vengeance.amt = savefile_read_s16b(file);
    }
    else
    {
        _vengeance.turn = 0;
        _vengeance.amt = 0;
    }
}
void hex_save(savefile_ptr file)
{
    savefile_write_byte(file, _vengeance.type);
    if (_vengeance.type)
    {
        savefile_write_byte(file, _vengeance.turn);
        savefile_write_s16b(file, _vengeance.amt);
    }
}

/************************************************************************
 * Timer Helpers
 ************************************************************************/
static int _count(void)
{
    plr_tim_ptr t;
    int ct = 0;
    for (t = plr->timers; t; t = t->next)
    {
        if (t->id >= T_HEX_BEGIN && t->id < T_HEX_END)
            ct++;
    }
    return ct;
}
int hex_count(void) 
{
    return _count();
}
int hex_max(void)
{
    return 1 + plr->lev/15;
}
static vec_ptr _current(void)
{
    vec_ptr v = vec_alloc(NULL);
    plr_tim_ptr t;
    for (t = plr->timers; t; t = t->next)
    {
        if (t->id >= T_HEX_BEGIN && t->id < T_HEX_END)
            vec_add(v, t);
    }
    return v;
}
static void _begin(int chant)
{
    plr_tim_add_aux(chant, 1, current_spell_cost);

    set_action(ACTION_SPELL);
    plr->update |= PU_BONUS;
    plr->redraw |= PR_STATUS;
}
static void _end(int chant)
{
    plr_tim_remove(chant);
}
static void _end_all(void)
{
    vec_ptr v = _current();
    int i;
    for (i = 0; i < vec_length(v); i++)
    {
        plr_tim_ptr t = vec_get(v, i);
        _end(t->id);
    }
    vec_free(v);
}
void hex_stop(void)
{
    _end_all();
}
static void _off(plr_tim_ptr timer)
{
    assert(!plr_tim_find(timer->id)); /* assert timer has already been removed from q as promised */

    if (!_count() && plr->action == ACTION_SPELL)
        set_action(ACTION_NONE);

    plr->update |= PU_BONUS;
    plr->redraw |= PR_STATUS;
}
static int _spell_index(int chant) /* maintaining a chant grants proficiency (plr->spell_exp[i]) */
{ /* XXX cf spell_stats_gain_skill for the new spell system. There, we use spell names, not
     integer indices. We could keep skill with just the timer info name (_find_info(chant)->name)
     and not need this ugly mapping ... but book based casting is still on the old system XXX */
    switch (chant)
    {
    case T_HEX_BLESS: return 0;
    case T_HEX_CURE_LIGHT: return 1;
    case T_HEX_DEMON_AURA: return 2;
    case T_HEX_STINKING_MIST: return 3;
    case T_HEX_XTRA_MIGHT: return 4;
    case T_HEX_DETECT_EVIL: return 6;
    case T_HEX_PATIENCE: return 7;
    case T_HEX_ICE_ARMOR: return 8;
    case T_HEX_CURE_SERIOUS: return 9;
    case T_HEX_VAMP_MIST: return 11;
    case T_HEX_RUNESWORD: return 12;
    case T_HEX_CONFUSION: return 13;
    case T_HEX_BUILDING: return 14;
    case T_HEX_ANTI_TELE: return 15;
    case T_HEX_SHOCK_CLOAK: return 16;
    case T_HEX_CURE_CRITICAL: return 17;
    case T_HEX_ANIMATE_DEAD: return 19;
    case T_HEX_SHADOW_CLOAK: return 21;
    case T_HEX_PAIN_TO_MANA: return 22;
    case T_HEX_EYE_FOR_EYE: return 23;
    case T_HEX_ANTI_MULTIPLY: return 24;
    case T_HEX_RESTORE: return 25;
    case T_HEX_VAMP_BLADE: return 27;
    case T_HEX_STUN: return 28;
    case T_HEX_ANTI_MAGIC: return 30;
    case T_HEX_REVENGE: return 31;
    }
    return -1;
}
static void _gain_skill(plr_tim_ptr timer)
{
    int i = _spell_index(timer->id);
    int l = cave->difficulty;
    magic_type *s_ptr;

    if (i < 0) return;
    if (plr->pclass == CLASS_SKILLMASTER) return;

    s_ptr = &technic_info[REALM_HEX - MIN_TECHNIC][i];

    if (plr->spell_exp[i] < SPELL_EXP_BEGINNER)
        plr->spell_exp[i] += 5;
    else if(plr->spell_exp[i] < SPELL_EXP_SKILLED)
    {
        if (one_in_(2) && l > 4 && l + 10 > plr->lev)
            plr->spell_exp[i] += 1;
    }
    else if(plr->spell_exp[i] < SPELL_EXP_EXPERT)
    { 
        if (one_in_(5) && l + 5 > plr->lev && l + 5 > s_ptr->slevel)
            plr->spell_exp[i] += 1;
    }
    else if(plr->spell_exp[i] < SPELL_EXP_MASTER)
    {
        if (one_in_(5) && l + 5 > plr->lev && l > s_ptr->slevel)
            plr->spell_exp[i] += 1;
    }
}
static bool _upkeep(plr_tim_ptr timer)
{
    s32b need_mana = timer->parm;
    u32b need_mana_frac = 0;

    /* no upkeep on the first tick ... take a look at plr_tim_fast_tick processing.
     * we now tick *after* the initial cast, which paid full cost. so while we now get
     * the effect right away, I don't think we should pay another half cost this turn. */
    if (timer->count == 1)
        return TRUE;

    if (plr->anti_magic)
        return FALSE;

    s64b_RSHIFT(need_mana, need_mana_frac, 1); /* /= 2 */
    if (s64b_cmp(plr->csp, plr->csp_frac, need_mana, need_mana_frac) < 0)
        return FALSE;

    s64b_sub(&plr->csp, &plr->csp_frac, need_mana, need_mana_frac);
    _gain_skill(timer);

    plr->redraw |= PR_MANA;
    return TRUE;
}
static void _dispel_stuff(void)
{
    plr->update |= PU_BONUS | PU_HP | PU_MONSTERS;
    plr->redraw |= PR_MAP | PR_STATUS | PR_STATE;
    plr->window |= PW_OVERHEAD | PW_DUNGEON;
}
static void _dispel(plr_tim_ptr timer)
{
    /* dispel magic interrupts the chant, giving a short window
     * of vulnerability for some effects. the chant is automatically
     * re-started next _tick, but the counter is reset. Some chants
     * improve with duration of singing. */
    timer->flags |= TF_INTERRUPTED;
    timer->count = 1;

    if (plr->action == ACTION_SPELL) /* do this once only on the first active chant */
    {
        msg_print("Your chanting is interrupted.");
        plr->action = ACTION_NONE;
        _dispel_stuff();
        plr->energy_need += ENERGY_NEED(); /* <= Especially wrt *this* !!! */
    }
}
static bool _tick(plr_tim_ptr timer)
{
    if (_upkeep(timer))
    {
        timer->count++;
        if (timer->flags & TF_INTERRUPTED)
        {
            timer->flags &= ~TF_INTERRUPTED;
            if (plr->action != ACTION_SPELL) /* do this once only on the first interrupted chant */
            {
                msg_print("You restart chanting.");
                plr->action = ACTION_SPELL;
                _dispel_stuff();
            }
            return FALSE; /* XXX no effect this tick */
        }
        return TRUE;
    }
    /* end this chant if upkeep failed (out of mana) */
    timer->count = 0;
    return FALSE;
}
static plr_tim_info_ptr _alloc(int id, cptr name)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(id, name);
    info->off_f = _off;
    info->dispel_f = _dispel;
    info->flags = TF_FAST_TICK | TF_NO_DISPEL | TF_IGNORE;
    return info;
}
static byte _color(plr_tim_ptr timer, byte desired)
{
    if (timer->flags & TF_INTERRUPTED)
        return TERM_L_DARK;
    return desired;
}
static void _bonus_to_h(plr_attack_info_ptr info, int amt)
{
    info->to_h += amt;
    info->dis_to_h += amt;
}
static void _bonus_to_d(plr_attack_info_ptr info, int amt)
{
    info->to_d += amt;
    info->dis_to_d += amt;
}

/************************************************************************
 * Evil Blessing
 ************************************************************************/
static bool _blessing_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of unrighteousness ...");
    return TRUE;
}
static void _blessing_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _blessing_bonus(plr_tim_ptr timer)
{
    if (plr_tim_find(T_BLESSED)) return; /* XXX no double dipping (quibble) */
    if (timer->flags & TF_INTERRUPTED) return;
    blessed_bonus(timer);
}
static void _blessing_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    if (plr_tim_find(T_BLESSED)) return;
    if (timer->flags & TF_INTERRUPTED) return;
    blessed_weapon_bonus(timer, obj, info);
}
static void _blessing_shooter_bonus(plr_tim_ptr timer, obj_ptr obj, plr_shoot_info_ptr info)
{
    if (plr_tim_find(T_BLESSED)) return;
    if (timer->flags & TF_INTERRUPTED) return;
    blessed_shooter_bonus(timer, obj, info);
}
static status_display_t _blessing_display(plr_tim_ptr timer)
{
    return status_display_create("Blessing", "Bs", _color(timer, TERM_RED));
}
static plr_tim_info_ptr _blessing(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_BLESS, "Evil Blessing");
    info->desc = "You are chanting an evil blessing.";
    info->on_f = _blessing_on;
    info->tick_f = _blessing_tick;
    info->calc_bonuses_f = _blessing_bonus;
    info->calc_weapon_bonuses_f = _blessing_weapon_bonus;
    info->calc_shooter_bonuses_f = _blessing_shooter_bonus;
    info->status_display_f = _blessing_display;
    return info;
}

/************************************************************************
 * Cure Light Wounds
 ************************************************************************/
static bool _cure_light_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of healing ...");
    return TRUE;
}
static dice_t _cure_light_dice(void) { return spell_dice(1, 10, 0); }
static void _cure_light_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _cure_light_dice();
        hp_player(dice_roll(d));
        plr_tim_subtract(T_CUT, 10);
    }
}
static status_display_t _cure_light_display(plr_tim_ptr timer)
{
    return status_display_create("Cure Light", "Cl", _color(timer, TERM_YELLOW));
}
static plr_tim_info_ptr _cure_light(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_CURE_LIGHT, "Cure Light Wounds");
    info->desc = "Your chant heals you a small amount.";
    info->on_f = _cure_light_on;
    info->tick_f = _cure_light_tick;
    info->status_display_f = _cure_light_display;
    return info;
}

/************************************************************************
 * Demon Aura
 ************************************************************************/
static bool _demon_aura_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of demonic protection ...");
    return TRUE;
}
static void _demon_aura_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _demon_aura_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->sh_fire = TRUE;
    plr->regen += 100;
}
static void _demon_aura_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    if (timer->flags & TF_INTERRUPTED) return;
    add_flag(flgs, OF_AURA_FIRE);
    add_flag(flgs, OF_REGEN);
}
static status_display_t _demon_aura_display(plr_tim_ptr timer)
{
    return status_display_create("Demon", "Dm", _color(timer, TERM_RED));
}
static plr_tim_info_ptr _demon_aura(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_DEMON_AURA, "Demon Aura");
    info->desc = "Your chant is granting demonic protection.";
    info->on_f = _demon_aura_on;
    info->tick_f = _demon_aura_tick;
    info->calc_bonuses_f = _demon_aura_bonus;
    info->flags_f = _demon_aura_flags;
    info->status_display_f = _demon_aura_display;
    return info;
}

/************************************************************************
 * Stinking Mist
 ************************************************************************/
static bool _stink_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of poisonous venom ...");
    return TRUE;
}
static dice_t _stink_dice(void) { return spell_dam_dice(1, 5 + plr->lev/2, 0); }
static void _stink_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _stink_dice();
        plr_project_los(GF_POIS, dice_roll(d));
    }
}
static status_display_t _stink_display(plr_tim_ptr timer)
{
    return status_display_create("Stink", "Pu", _color(timer, TERM_L_GREEN));
}
static plr_tim_info_ptr _stink(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_STINKING_MIST, "Stinking Mist");
    info->desc = "Your chant poisons nearby monsters.";
    info->on_f = _stink_on;
    info->tick_f = _stink_tick;
    info->status_display_f = _stink_display;
    return info;
}

/************************************************************************
 * Extra Might
 ************************************************************************/
static bool _might_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of unholy strength ...");
    plr->update |= PU_HP;
    return TRUE;
}
static void _might_off(plr_tim_ptr timer)
{
    _off(timer);
    plr->update |= PU_HP;
}
/* XXX _dispel_stuff() is doing PU_HP XXX */
static void _might_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _might_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->xtra_hp += 15;
}
static void _might_stats(plr_tim_ptr timer, s16b stats[MAX_STATS])
{
    if (timer->flags & TF_INTERRUPTED) return;
    stats[A_STR] += 4;
}
static void _might_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    if (timer->flags & TF_INTERRUPTED) return;
    info->blows_calc.wgt /= 2;
    info->blows_calc.mul += 20;
}
static status_display_t _might_display(plr_tim_ptr timer)
{
    return status_display_create("Might", "Dm", _color(timer, TERM_L_UMBER));
}
static plr_tim_info_ptr _might(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_XTRA_MIGHT, "Extra Might");
    info->desc = "Your chant is granting unholy strength.";
    info->on_f = _might_on;
    info->off_f = _might_off;
    info->tick_f = _might_tick;
    info->calc_bonuses_f = _might_bonus;
    info->stats_f = _might_stats;
    info->calc_weapon_bonuses_f = _might_weapon_bonus;
    info->status_display_f = _might_display;
    return info;
}

/************************************************************************
 * Evil Detection
 ************************************************************************/
static bool _detect_evil_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant to reveal kindred souls ...");
    return TRUE;
}
static void _detect_evil_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _detect_evil_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->esp_evil = TRUE;
}
static void _detect_evil_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    if (timer->flags & TF_INTERRUPTED) return;
    add_flag(flgs, OF_ESP_EVIL);
}
static status_display_t _detect_evil_display(plr_tim_ptr timer)
{
    return status_display_create("Evil", "Ev", _color(timer, TERM_L_BLUE));
}
static plr_tim_info_ptr _detect_evil(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_DETECT_EVIL, "Evil Detection");
    info->desc = "Your chant is revealing the presence of evil.";
    info->on_f = _detect_evil_on;
    info->tick_f = _detect_evil_tick;
    info->calc_bonuses_f = _detect_evil_bonus;
    info->flags_f = _detect_evil_flags;
    info->status_display_f = _detect_evil_display;
    return info;
}

/************************************************************************
 * Patience
 ************************************************************************/
static bool _patience_on(plr_tim_ptr timer)
{
    int a = 3 - (plr->pspeed + 10) / 10;  /* XXX speed decreases patience? */
    int r = 3 + _1d(3) + MAX(0, MIN(3, a));

    _vengeance.type = _PATIENCE;
    _vengeance.turn = 2 + r; /* This turn: timer->count starts at 1 and then ticks to 2 */
    _vengeance.amt = 0;

    msg_print("You begin a foul chant to patiently wait for revenge ...");
    return TRUE;
}
static void _patience_off(plr_tim_ptr timer)
{
    _off(timer);
    _vengeance.type = 0;
    _vengeance.turn = 0;
    _vengeance.amt = 0;
}
static dice_t _patience_dice(void) { return spell_dam_dice(0, 0, MIN(100, _vengeance.amt)); }
static void _patience_tick(plr_tim_ptr timer)
{
    /* revenge if we have waited long enough, or if we have received too much damage.
     * do not revenge if _tick fails (lack of mana or interrupted chant)
     * XXX _dispel resets timer->count but not _vengeance.amt XXX */
    if (_tick(timer) && (timer->count >= _vengeance.turn || _vengeance.amt >= 200))
    {
        dice_t d = _patience_dice();
        int dam = dice_roll(d);
        int rad = 2 + dam/50;
        msg_print("<color:r>Enough!</color> Now it is time for revenge!!");
        msg_print(NULL);
        plr_burst(rad, GF_HELL_FIRE, dam);
        timer->count = 0;
    }
}
static status_display_t _patience_display(plr_tim_ptr timer)
{
    return status_display_create("Patience", "Pt", _color(timer, TERM_RED));
}
static plr_tim_info_ptr _patience(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_PATIENCE, "Patience");
    info->desc = "You are patiently waiting for revenge.";
    info->on_f = _patience_on;
    info->off_f = _patience_off;
    info->tick_f = _patience_tick;
    info->status_display_f = _patience_display;
    return info;
}

/************************************************************************
 * Ice Armor
 ************************************************************************/
static bool _ice_armor_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of icy protection ...");
    return TRUE;
}
static void _ice_armor_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _ice_armor_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->sh_cold = TRUE;
    plr->to_a += 30;
    plr->dis_to_a += 30;
}
static void _ice_armor_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    if (timer->flags & TF_INTERRUPTED) return;
    add_flag(flgs, OF_AURA_COLD);
}
static status_display_t _ice_armor_display(plr_tim_ptr timer)
{
    return status_display_create("Ice", "Ic", _color(timer, TERM_WHITE));
}
static plr_tim_info_ptr _ice_armor(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_ICE_ARMOR, "Ice Armor");
    info->desc = "Your chant is granting icy protection.";
    info->on_f = _ice_armor_on;
    info->tick_f = _ice_armor_tick;
    info->calc_bonuses_f = _ice_armor_bonus;
    info->flags_f = _ice_armor_flags;
    info->status_display_f = _ice_armor_display;
    return info;
}

/************************************************************************
 * Cure Serious Wounds
 ************************************************************************/
static bool _cure_serious_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of healing ...");
    return TRUE;
}
static dice_t _cure_serious_dice(void) { return spell_dice(2, 10, 0); }
static void _cure_serious_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _cure_serious_dice();
        hp_player(dice_roll(d));
        plr_tim_recover(T_CUT, 50, 10);
    }
}
static status_display_t _cure_serious_display(plr_tim_ptr timer)
{
    return status_display_create("Cure Serious", "Cs", _color(timer, TERM_YELLOW));
}
static plr_tim_info_ptr _cure_serious(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_CURE_SERIOUS, "Cure Serious Wounds");
    info->desc = "Your chant heals you a serious amount.";
    info->on_f = _cure_serious_on;
    info->tick_f = _cure_serious_tick;
    info->status_display_f = _cure_serious_display;
    return info;
}

/************************************************************************
 * Vampiric Mist
 ************************************************************************/
static bool _vamp_mist_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of draining ...");
    return TRUE;
}
static dice_t _vamp_mist_dice(void) { return spell_dam_dice(1, 5 + plr->lev/2, 0); }
static void _vamp_mist_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _vamp_mist_dice();
        plr_project_los(GF_OLD_DRAIN, dice_roll(d));
    }
}
static status_display_t _vamp_mist_display(plr_tim_ptr timer)
{
    return status_display_create("Vamp", "Vm", _color(timer, TERM_L_DARK));
}
static plr_tim_info_ptr _vamp_mist(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_VAMP_MIST, "Vampiric Mist");
    info->desc = "Your chant drains nearby monsters.";
    info->on_f = _vamp_mist_on;
    info->tick_f = _vamp_mist_tick;
    info->status_display_f = _vamp_mist_display;
    return info;
}

/************************************************************************
 * Runesword
 ************************************************************************/
static bool _runesword_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of bloodlust ...");
    if (plr->weapon_ct == 1)
        msg_print("Your weapon glows black.");
    if (plr->weapon_ct > 1)
        msg_print("Your weapons glow black.");
    return TRUE;
}
static void _runesword_off(plr_tim_ptr timer)
{
    _off(timer);
    if (plr->weapon_ct == 1)
        msg_print("Your weapon returns to normal.");
    if (plr->weapon_ct > 1)
        msg_print("Your weapons return to normal.");
}
static void _runesword_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _runesword_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    if (timer->flags & TF_INTERRUPTED) return;

    add_flag(info->obj_flags, OF_SLAY_GOOD);
    add_flag(info->obj_known_flags, OF_SLAY_GOOD);

    if (obj->tval == TV_SWORD)
    {
        add_flag(info->obj_flags, OF_VORPAL);
        add_flag(info->obj_known_flags, OF_VORPAL);
    }

    if (obj && obj_is_cursed(obj))
    {
        if (obj->curse_flags & OFC_CURSED) { _bonus_to_d(info, 5); }
        if (obj->curse_flags & OFC_HEAVY_CURSE) { _bonus_to_d(info, 7); }
        if (obj->curse_flags & OFC_PERMA_CURSE) { _bonus_to_d(info, 13); }
    }
}
static void _runesword_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    if (timer->flags & TF_INTERRUPTED) return;
    add_flag(flgs, OF_SLAY_GOOD);
    add_flag(flgs, OF_VORPAL); /* XXX only for TV_SWORD */
}
static status_display_t _runesword_display(plr_tim_ptr timer)
{
    return status_display_create("Runesword", "Rs", _color(timer, TERM_RED));
}
static plr_tim_info_ptr _runesword(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_RUNESWORD, "Swords to Runeswords");
    info->desc = "Your chant enhances the deadliness of your melee weapon.";
    info->on_f = _runesword_on;
    info->off_f = _runesword_off;
    info->tick_f = _runesword_tick;
    info->flags_f = _runesword_flags;
    info->calc_weapon_bonuses_f = _runesword_weapon_bonus;
    info->status_display_f = _runesword_display;
    return info;
}

/************************************************************************
 * Touch of Confusion (cf _hit_effects2)
 ************************************************************************/
static bool _confusion_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of confusion ...");
    return TRUE;
}
static void _confusion_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static status_display_t _confusion_display(plr_tim_ptr timer)
{
    return status_display_create("Confusion", "Cf", _color(timer, TERM_L_UMBER));
}
static plr_tim_info_ptr _confusion(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_CONFUSION, "Touch of Confusion");
    info->desc = "Your chant causes your melee attacks to confuse your enemies.";
    info->on_f = _confusion_on;
    info->tick_f = _confusion_tick;
    info->status_display_f = _confusion_display;
    return info;
}

/************************************************************************
 * Building Up
 ************************************************************************/
static bool _building_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of demonic strength ...");
    plr->update |= PU_HP;
    return TRUE;
}
static void _building_off(plr_tim_ptr timer)
{
    _off(timer);
    plr->update |= PU_HP;
}
/* XXX _dispel_stuff() is doing PU_HP XXX */
static void _building_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _building_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->xtra_hp += 60;
}
static void _building_stats(plr_tim_ptr timer, s16b stats[MAX_STATS])
{
    if (timer->flags & TF_INTERRUPTED) return;
    stats[A_STR] += 4;
    stats[A_DEX] += 4;
    stats[A_CON] += 4;
}
static void _building_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    if (plr_tim_find(T_HEX_XTRA_MIGHT)) return; /* XXX */
    if (timer->flags & TF_INTERRUPTED) return;
    info->blows_calc.wgt /= 2;
    info->blows_calc.mul += 20;
}
static status_display_t _building_display(plr_tim_ptr timer)
{
    return status_display_create("Building", "Bg", _color(timer, TERM_VIOLET));
}
static plr_tim_info_ptr _building(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_BUILDING, "Building Up");
    info->desc = "Your chant is granting incredible physical prowess.";
    info->on_f = _building_on;
    info->off_f = _building_off;
    info->tick_f = _building_tick;
    info->calc_bonuses_f = _building_bonus;
    info->stats_f = _building_stats;
    info->calc_weapon_bonuses_f = _building_weapon_bonus;
    info->status_display_f = _building_display;
    return info;
}

/************************************************************************
 * Anti-teleport Barrier
 ************************************************************************/
static bool _anti_teleport_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant to disrupt teleportation ...");
    return TRUE;
}
static void _anti_teleport_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _anti_teleport_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->block_teleport = TRUE;
}
static status_display_t _anti_teleport_display(plr_tim_ptr timer)
{
    return status_display_create("No Tele", "!T", _color(timer, TERM_L_BLUE));
}
static plr_tim_info_ptr _anti_teleport(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_ANTI_TELE, "Anti-teleport Barrier");
    info->desc = "Your chant disrupts the teleportation of your enemies.";
    info->on_f = _anti_teleport_on;
    info->tick_f = _anti_teleport_tick;
    info->calc_bonuses_f = _anti_teleport_bonus;
    info->status_display_f = _anti_teleport_display;
    return info;
}

/************************************************************************
 * Shock Cloak
 ************************************************************************/
static bool _shock_cloak_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of shocking protection ...");
    return TRUE;
}
static void _shock_cloak_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _shock_cloak_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->sh_elec = TRUE;
    plr->pspeed += 3;
}
static void _shock_cloak_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    if (timer->flags & TF_INTERRUPTED) return;
    add_flag(flgs, OF_AURA_ELEC);
    add_flag(flgs, OF_SPEED);
}
static status_display_t _shock_cloak_display(plr_tim_ptr timer)
{
    return status_display_create("Shock", "Sk", _color(timer, TERM_BLUE));
}
static plr_tim_info_ptr _shock_cloak(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_SHOCK_CLOAK, "Shock Cloak");
    info->desc = "Your chant is granting shocking protection.";
    info->on_f = _shock_cloak_on;
    info->tick_f = _shock_cloak_tick;
    info->calc_bonuses_f = _shock_cloak_bonus;
    info->flags_f = _shock_cloak_flags;
    info->status_display_f = _shock_cloak_display;
    return info;
}

/************************************************************************
 * Cure Critical Wounds
 ************************************************************************/
static bool _cure_critical_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of healing ...");
    return TRUE;
}
static dice_t _cure_critical_dice(void) { return spell_dice(4, 10, 0); }
static void _cure_critical_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _cure_critical_dice();
        hp_player(dice_roll(d));
        plr_tim_remove(T_STUN);
        plr_tim_remove(T_CUT);
    }
}
static status_display_t _cure_critical_display(plr_tim_ptr timer)
{
    return status_display_create("Cure Crit", "Cc", _color(timer, TERM_YELLOW));
}
static plr_tim_info_ptr _cure_critical(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_CURE_CRITICAL, "Cure Critical Wounds");
    info->desc = "Your chant heals you a critical amount.";
    info->on_f = _cure_critical_on;
    info->tick_f = _cure_critical_tick;
    info->status_display_f = _cure_critical_display;
    return info;
}

/************************************************************************
 * Animate Dead
 ************************************************************************/
static bool _animate_dead_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of revivification ...");
    return TRUE;
}
static void _animate_dead_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
        plr_animate_dead();
}
static status_display_t _animate_dead_display(plr_tim_ptr timer)
{
    return status_display_create("Animate", "An", _color(timer, TERM_ORANGE));
}
static plr_tim_info_ptr _animate_dead(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_ANIMATE_DEAD, "Animate Dead");
    info->desc = "Your chant brings the dead back to life.";
    info->on_f = _animate_dead_on;
    info->tick_f = _animate_dead_tick;
    info->status_display_f = _animate_dead_display;
    return info;
}

/************************************************************************
 * Shadow Cloak (cf mon_on_hit_plr)
 ************************************************************************/
static bool _shadow_check(void)
{
    int slot = equip_find_first(obj_is_cloak);
    if (!slot || !obj_is_cursed(equip_obj(slot)))
        return FALSE;
    return TRUE;
}
static bool _shadow_cloak_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of shadowy protection ...");
    return TRUE;
}
static void _shadow_cloak_tick(plr_tim_ptr timer)
{
    if (!_shadow_check())
        timer->count = 0;
    else
        _tick(timer);
}
static status_display_t _shadow_cloak_display(plr_tim_ptr timer)
{
    return status_display_create("Shadow", "Sh", _color(timer, TERM_L_DARK));
}
static plr_tim_info_ptr _shadow_cloak(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_SHADOW_CLOAK, "Shadow Cloak");
    info->desc = "Your chant grants shadowy protection against melee attacks.";
    info->on_f = _shadow_cloak_on;
    info->tick_f = _shadow_cloak_tick;
    info->status_display_f = _shadow_cloak_display;
    return info;
}

/************************************************************************
 * Pains to Mana
 ************************************************************************/
static bool _psi_drain_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant to feed off the psychic pain of others ...");
    return TRUE;
}
static dice_t _psi_drain_dice(void) { return spell_dam_dice(1, 3*plr->lev/2, 0); }
static void _psi_drain_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _psi_drain_dice();
        plr_project_los(GF_PSI_DRAIN, dice_roll(d));
    }
}
static status_display_t _psi_drain_display(plr_tim_ptr timer)
{
    return status_display_create("Pains", "Pn", _color(timer, TERM_L_RED));
}
static plr_tim_info_ptr _psi_drain(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_PAIN_TO_MANA, "Pains to Mana");
    info->desc = "Your chant inflicts psychic pain on surrounding monsters, draining mana in the process.";
    info->on_f = _psi_drain_on;
    info->tick_f = _psi_drain_tick;
    info->status_display_f = _psi_drain_display;
    return info;
}

/************************************************************************
 * Eye for an Eye
 ************************************************************************/
static bool _eye_for_eye_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant for immediate vengeance ...");
    return TRUE;
}
static void _eye_for_eye_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _eye_for_eye_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->revenge = TRUE;
}
static status_display_t _eye_for_eye_display(plr_tim_ptr timer)
{
    return status_display_create("Eye", "Ey", _color(timer, TERM_VIOLET));
}
static plr_tim_info_ptr _eye_for_eye(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_EYE_FOR_EYE, "Eye for an Eye");
    info->desc = "Your chant is granting immediate retribution.";
    info->on_f = _eye_for_eye_on;
    info->tick_f = _eye_for_eye_tick;
    info->calc_bonuses_f = _eye_for_eye_bonus;
    info->status_display_f = _eye_for_eye_display;
    return info;
}

/************************************************************************
 * Anti-multiply Barrier
 ************************************************************************/
static bool _anti_multiply_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant to disrupt breeding ...");
    return TRUE;
}
static void _anti_multiply_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _anti_multiply_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->block_multiply = TRUE;
}
static status_display_t _anti_multiply_display(plr_tim_ptr timer)
{
    return status_display_create("No Mult", "!B", _color(timer, TERM_L_BLUE));
}
static plr_tim_info_ptr _anti_multiply(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_ANTI_MULTIPLY, "Anti-multiply Barrier");
    info->desc = "Your chant disrupts monster breeding.";
    info->on_f = _anti_multiply_on;
    info->calc_bonuses_f = _anti_multiply_bonus;
    info->tick_f = _anti_multiply_tick;
    info->status_display_f = _anti_multiply_display;
    return info;
}

/************************************************************************
 * Restore Life
 ************************************************************************/
static bool _restore_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of restoration and recovery ...");
    return TRUE;
}
static void _restore_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        bool flag = FALSE;
        int d = plr->max_exp - plr->exp;
        int r = plr->exp / 20;
        int i;

        if (d > 0)
        {
            if (d < r)
                plr->exp = plr->max_exp;
            else
                plr->exp += r;

            plr->notice |= PN_EXP;
            flag = TRUE;
        }

        if (plr->clp < 1000)
        {
            plr_restore_life(50);
            flag = TRUE;
        }

        for (i = A_STR; i < 6; i ++)
        {
            if (plr->stat_cur[i] < plr->stat_max[i])
            {
                if (plr->stat_cur[i] < 18)
                    plr->stat_cur[i]++;
                else
                    plr->stat_cur[i] += 10;

                if (plr->stat_cur[i] > plr->stat_max[i])
                    plr->stat_cur[i] = plr->stat_max[i];

                plr->update |= PU_BONUS;
                flag = TRUE;
            }
        }

        /* stop the chant once fully restored */
        if (!flag)
            timer->count = 0;
    }
}
static status_display_t _restore_display(plr_tim_ptr timer)
{
    return status_display_create("Restore Life", "Rl", _color(timer, TERM_YELLOW));
}
static plr_tim_info_ptr _restore(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_RESTORE, "Restore Life");
    info->desc = "Your chant restores life and stats.";
    info->on_f = _restore_on;
    info->tick_f = _restore_tick;
    info->status_display_f = _restore_display;
    return info;
}

/************************************************************************
 * Swords to Vampires
 ************************************************************************/
static bool _vamp_blade_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of bloodlust ...");
    if (plr->weapon_ct == 1)
        msg_print("Your weapon thirsts for blood.");
    if (plr->weapon_ct > 1)
        msg_print("Your weapons thirst for blood.");
    return TRUE;
}
static void _vamp_blade_off(plr_tim_ptr timer)
{
    _off(timer);
    if (plr->weapon_ct == 1)
        msg_print("Your weapon is satiated.");
    if (plr->weapon_ct > 1)
        msg_print("Your weapons are satiated.");
}
static void _vamp_blade_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _vamp_blade_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    if (timer->flags & TF_INTERRUPTED) return;

    add_flag(info->obj_flags, OF_BRAND_VAMP);
    add_flag(info->obj_known_flags, OF_BRAND_VAMP);
}
static status_display_t _vamp_blade_display(plr_tim_ptr timer)
{
    return status_display_create("Vamp", "Vb", _color(timer, TERM_RED));
}
static plr_tim_info_ptr _vamp_blade(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_VAMP_BLADE, "Swords to Vampires");
    info->desc = "Your chant enables you melee weapons to drain life from your enemies.";
    info->on_f = _vamp_blade_on;
    info->off_f = _vamp_blade_off;
    info->tick_f = _vamp_blade_tick;
    info->calc_weapon_bonuses_f = _vamp_blade_weapon_bonus;
    info->status_display_f = _vamp_blade_display;
    return info;
}

/************************************************************************
 * Word of Stun
 ************************************************************************/
static bool _stun_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant of stunning power ...");
    return TRUE;
}
static dice_t _stun_dice(void) { return spell_dice(0, 0, 5 + plr->lev/5); }
static void _stun_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _stun_dice();
        plr_project_los(GF_STUN, dice_roll(d));
    }
}
static status_display_t _stun_display(plr_tim_ptr timer)
{
    return status_display_create("Stun", "Sn", _color(timer, TERM_L_BLUE));
}
static plr_tim_info_ptr _stun(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_STUN, "Word of Stun");
    info->desc = "Your chant stuns nearby monsters.";
    info->on_f = _stun_on;
    info->tick_f = _stun_tick;
    info->status_display_f = _stun_display;
    return info;
}

/************************************************************************
 * Anti-magic Barrier
 ************************************************************************/
static bool _anti_magic_on(plr_tim_ptr timer)
{
    msg_print("You begin a foul chant to disrupt magic ...");
    return TRUE;
}
static void _anti_magic_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _anti_magic_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->block_magic = TRUE;
}
static status_display_t _anti_magic_display(plr_tim_ptr timer)
{
    return status_display_create("No Spell", "!S", _color(timer, TERM_L_BLUE));
}
static plr_tim_info_ptr _anti_magic(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_ANTI_MAGIC, "Anti-magic Barrier");
    info->desc = "Your chant disrupts everyone's magic but your own.";
    info->on_f = _anti_magic_on;
    info->calc_bonuses_f = _anti_magic_bonus;
    info->tick_f = _anti_magic_tick;
    info->status_display_f = _anti_magic_display;
    return info;
}

/************************************************************************
 * Revenge Sentence
 ************************************************************************/
static bool _revenge_on(plr_tim_ptr timer)
{
    int a = 3 - (plr->pspeed + 10) / 10;  /* XXX speed decreases revenge? */
    int r = 3 + _1d(3) + MAX(0, MIN(3, a));

    _vengeance.type = _REVENGE;
    _vengeance.turn = 2 + r; /* This turn: timer->count starts at 1 and then ticks to 2 */
    _vengeance.amt = 0;

    msg_format("You begin a foul chant to sentence revenge ... %d turns left.", r);
    return TRUE;
}
static void _revenge_off(plr_tim_ptr timer)
{
    _off(timer);
    _vengeance.type = 0;
    _vengeance.turn = 0;
    _vengeance.amt = 0;
}
static dice_t _revenge_dice(void) { return spell_dam_dice(0, 0, _vengeance.amt); }
static void _revenge_tick(plr_tim_ptr timer)
{
    if (_tick(timer) && timer->count >= _vengeance.turn) /* dispel magic might delay the revenge a turn */
    {
        dice_t d = _revenge_dice();
        int dam = dice_roll(d);
        if (dam)
        {
            point_t pos;
            for (;;)
            {
                cmsg_print(TERM_VIOLET, "Time to revenge!");
                pos = plr_get_ball_target(GF_HELL_FIRE);
                if (dun_pos_interior(cave, pos)) break;
            }
            plr_ball(1, pos, GF_HELL_FIRE, dam);
        }
        else
            msg_print("You are not in a mood for revenge.");
        timer->count = 0;
    }
}
static status_display_t _revenge_display(plr_tim_ptr timer)
{
    return status_display_create("Revenge", "Rv", _color(timer, TERM_RED));
}
static plr_tim_info_ptr _revenge(void)
{
    plr_tim_info_ptr info = _alloc(T_HEX_REVENGE, "Revenge Sentence");
    info->desc = "You are waiting for vengeance.";
    info->on_f = _revenge_on;
    info->off_f = _revenge_off;
    info->tick_f = _revenge_tick;
    info->status_display_f = _revenge_display;
    return info;
}

/************************************************************************
 * Timers
 ************************************************************************/
void hex_register_timers(void)
{
    plr_tim_register(_blessing());
    plr_tim_register(_cure_light());
    plr_tim_register(_demon_aura());
    plr_tim_register(_stink());
    plr_tim_register(_might());
    plr_tim_register(_detect_evil());
    plr_tim_register(_patience());
    plr_tim_register(_ice_armor());
    plr_tim_register(_cure_serious());
    plr_tim_register(_vamp_mist());
    plr_tim_register(_runesword());
    plr_tim_register(_confusion());
    plr_tim_register(_building());
    plr_tim_register(_anti_teleport());
    plr_tim_register(_shock_cloak());
    plr_tim_register(_cure_critical());
    plr_tim_register(_animate_dead());
    plr_tim_register(_shadow_cloak());
    plr_tim_register(_psi_drain());
    plr_tim_register(_eye_for_eye());
    plr_tim_register(_anti_multiply());
    plr_tim_register(_restore());
    plr_tim_register(_vamp_blade());
    plr_tim_register(_stun());
    plr_tim_register(_anti_magic());
    plr_tim_register(_revenge());
}

/************************************************************************
 * Cursing Gear
 ************************************************************************/
static bool _curse_weapon(void)
{
    obj_prompt_t prompt = {0};
    char o_name[MAX_NLEN];

    prompt.prompt = "Which weapon do you curse?";
    prompt.error = "You wield no weapons.";
    prompt.filter = obj_is_weapon;
    prompt.where[0] = INV_EQUIP;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    object_desc(o_name, prompt.obj, OD_NAME_ONLY | OD_COLOR_CODED);

    if (!get_check(format("Do you curse %s, really?", o_name))) return FALSE;

    if (!one_in_(3) &&
        (obj_is_art(prompt.obj) || obj_has_flag(prompt.obj, OF_BLESSED)))
    {
        msg_format("%s resists the effect.", o_name);
        if (one_in_(3))
        {
            if (prompt.obj->to_d > 0)
            {
                prompt.obj->to_d -= _1d(3) % 2;
                if (prompt.obj->to_d < 0) prompt.obj->to_d = 0;
            }
            if (prompt.obj->to_h > 0)
            {
                prompt.obj->to_h -= _1d(3) % 2;
                if (prompt.obj->to_h < 0) prompt.obj->to_h = 0;
            }
            if (prompt.obj->to_a > 0)
            {
                prompt.obj->to_a -= _1d(3) % 2;
                if (prompt.obj->to_a < 0) prompt.obj->to_a = 0;
            }
            msg_format("Your %s was disenchanted!", o_name);
        }
    }
    else
    {
        int power = 0;
        msg_format("A terrible black aura blasts your %s!", o_name);
        prompt.obj->curse_flags |= OFC_CURSED;

        if (obj_is_art(prompt.obj) || obj_is_ego(prompt.obj))
        {

            if (one_in_(3)) prompt.obj->curse_flags |= OFC_HEAVY_CURSE;
            if (one_in_(666))
            {
                prompt.obj->curse_flags |= OFC_TY_CURSE;
                if (one_in_(666)) prompt.obj->curse_flags |= OFC_PERMA_CURSE;

                add_flag(prompt.obj->flags, OF_AGGRAVATE);
                add_flag(prompt.obj->flags, OF_VORPAL);
                add_flag(prompt.obj->flags, OF_BRAND_VAMP);
                cmsg_print(TERM_RED, "Blood, Blood, Blood!");
                power = 2;
            }
        }

        prompt.obj->curse_flags |= get_curse(power, prompt.obj);
    }

    plr->update |= (PU_BONUS);
    return TRUE;
}
static void _curse_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    if (obj->curse_flags & OFC_CURSED) { _bonus_to_h(info, 5); }
    if (obj->curse_flags & OFC_HEAVY_CURSE) { _bonus_to_h(info, 7); }
    if (obj->curse_flags & OFC_PERMA_CURSE) { _bonus_to_h(info, 13); }
    if (obj->curse_flags & OFC_TY_CURSE) { _bonus_to_h(info, 5); }
}
static bool _curse_armor(void)
{
    obj_prompt_t prompt = {0};
    char o_name[MAX_NLEN];

    prompt.prompt = "Which piece of armour do you curse?";
    prompt.error = "You wield no piece of armours.";
    prompt.filter = obj_is_armor;
    prompt.where[0] = INV_EQUIP;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    object_desc(o_name, prompt.obj, OD_NAME_ONLY);

    if (!get_check(format("Do you curse %s, really?", o_name))) return FALSE;

    if (!one_in_(3) &&
        (obj_is_art(prompt.obj) || obj_has_flag(prompt.obj, OF_BLESSED)))
    {
        msg_format("%s resists the effect.", o_name);
        if (one_in_(3))
        {
            if (prompt.obj->to_d > 0)
            {
                prompt.obj->to_d -= _1d(3) % 2;
                if (prompt.obj->to_d < 0) prompt.obj->to_d = 0;
            }
            if (prompt.obj->to_h > 0)
            {
                prompt.obj->to_h -= _1d(3) % 2;
                if (prompt.obj->to_h < 0) prompt.obj->to_h = 0;
            }
            if (prompt.obj->to_a > 0)
            {
                prompt.obj->to_a -= _1d(3) % 2;
                if (prompt.obj->to_a < 0) prompt.obj->to_a = 0;
            }
            msg_format("Your %s was disenchanted!", o_name);
        }
    }
    else
    {
        int power = 0;
        msg_format("A terrible black aura blasts your %s!", o_name);
        prompt.obj->curse_flags |= OFC_CURSED;

        if (obj_is_art(prompt.obj) || obj_is_ego(prompt.obj))
        {

            if (one_in_(3)) prompt.obj->curse_flags |= OFC_HEAVY_CURSE;
            if (one_in_(666))
            {
                prompt.obj->curse_flags |= OFC_TY_CURSE;
                if (one_in_(666)) prompt.obj->curse_flags |= OFC_PERMA_CURSE;

                add_flag(prompt.obj->flags, OF_AGGRAVATE);
                add_flag(prompt.obj->flags, OF_RES_(GF_POIS));
                add_flag(prompt.obj->flags, OF_RES_(GF_DARK));
                add_flag(prompt.obj->flags, OF_RES_(GF_NETHER));
                cmsg_print(TERM_RED, "Blood, Blood, Blood!");
                power = 2;
            }
        }

        prompt.obj->curse_flags |= get_curse(power, prompt.obj);
    }

    plr->update |= PU_BONUS;
    return TRUE;
}
static void _curse_armor_bonuses(obj_ptr obj)
{
    if (obj_is_armor(obj) && obj_is_cursed(obj))
    {
        int ac = 5;
        if (obj->curse_flags & OFC_HEAVY_CURSE) ac += 7;
        if (obj->curse_flags & OFC_PERMA_CURSE) ac += 13;
        plr->to_a += ac;
        plr->dis_to_a += ac;
    }
}
static bool _drain_curse(void)
{
    obj_prompt_t prompt = {0};

    prompt.prompt = "Which cursed equipment do you drain mana from?";
    prompt.error = "You have no cursed equipment.";
    prompt.filter = obj_is_cursed;
    prompt.where[0] = INV_EQUIP;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    plr->csp += plr->lev/5 + _1d(plr->lev/5);
    if (obj_has_flag(prompt.obj, OF_TY_CURSE) || (prompt.obj->curse_flags & OFC_TY_CURSE))
        plr->csp += _1d(5);
    if (plr->csp > plr->msp)
        plr->csp = plr->msp;

    if (prompt.obj->curse_flags & OFC_PERMA_CURSE)
    {
        /* Nothing */
    }
    else if (prompt.obj->curse_flags & OFC_HEAVY_CURSE)
    {
        if (one_in_(7))
        {
            msg_print("The heavy curse was drained.");
            prompt.obj->curse_flags = 0;
        }
    }
    else if ((prompt.obj->curse_flags & OFC_CURSED) && one_in_(3))
    {
        msg_print("The curse was drained.");
        prompt.obj->curse_flags = 0;
    }
    return TRUE;
}

/************************************************************************
 * Spells
 ************************************************************************/
cptr do_hex_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

    int plev = plr->lev;
    int power;

    switch (spell)
    {
    case 0:
        if (name) return "Evil Blessing";
        if (desc) return "Attempts to increase +to_hit of a weapon and AC";
        if (cast) _begin(T_HEX_BLESS);
        break;
    case 1:
        if (name) return "Cure Light Wounds";
        if (desc) return "Heals cut and HP a little.";
        if (info) return dice_info_heal(_cure_light_dice());
        if (cast) _begin(T_HEX_CURE_LIGHT);
        break;
    case 2:
        if (name) return "Demonic Aura";
        if (desc) return "Gives fire aura and regeneration.";
        if (cast) _begin(T_HEX_DEMON_AURA);
        break;
    case 3:
        if (name) return "Stinking Mist";
        if (desc) return "Deals few damages of poison to all monsters in your sight.";
        if (info) return dice_info_dam(_stink_dice());
        if (cast) _begin(T_HEX_STINKING_MIST);
        break;
    case 4:
        if (name) return "Extra Might";
        if (desc) return "Attempts to increase your strength.";
        if (cast) _begin(T_HEX_XTRA_MIGHT);
        break;
    case 5:
        if (name) return "Curse Weapon";
        if (desc) return "Curses your weapon.";
        if (cast && !_curse_weapon()) return NULL;
        break;
    case 6:
        if (name) return "Evil Detection";
        if (desc) return "Detects evil monsters.";
        if (cast) _begin(T_HEX_DETECT_EVIL);
        break;
    case 7:
        if (name) return "Patience";
        if (desc) return "Bursts hell fire strongly after patiently enduring damage a few turns.";
        if (info) return dice_info_dam(_patience_dice());
        if (cast)
        {
            if (_vengeance.type)
            {
                msg_print("You are already patiently waiting for revenge!");
                return NULL;
            }
            _begin(T_HEX_PATIENCE);
        }
        break;
    case 8:
        if (name) return "Ice Armor";
        if (desc) return "Gives cold aura and bonus to AC.";
        if (cast) _begin(T_HEX_ICE_ARMOR);
        break;
    case 9:
        if (name) return "Cure Serious Wounds";
        if (desc) return "Heals cut and HP more.";
        if (info) return dice_info_heal(_cure_serious_dice());
        if (cast) _begin(T_HEX_CURE_SERIOUS);
        break;
    case 10:
        if (name) return "Inhale Potion";
        if (desc) return "Quaffs a potion without canceling of casting a spell.";
        if (cast)
        {
            hex_inhale = TRUE; /* XXX avoid hex_stop in do_cmd_quaff_potion_aux */
            do_cmd_quaff_potion();
            hex_inhale = FALSE;
        }
        break;
    case 11:
        if (name) return "Vampiric Mist";
        if (desc) return "Deals few damages of drain life to all monsters in your sight.";
        if (info) return dice_info_dam(_vamp_mist_dice());
        if (cast) _begin(T_HEX_VAMP_MIST);
        break;
    case 12:
        if (name) return "Swords to Runeswords";
        if (desc) return "Your melee weapon will slay good monsters and will gain sharpness (swords only). Cursed weapons gain increased deadliness.";
        if (cast) _begin(T_HEX_RUNESWORD);
        break;
    case 13:
        if (name) return "Touch of Confusion";
        if (desc) return "Confuses a monster when you attack.";
        if (cast) _begin(T_HEX_CONFUSION);
        break;
    case 14:
        if (name) return "Building Up";
        if (desc) return "Attempts to increases your strength, dexterity and constitution.";
        if (cast) _begin(T_HEX_BUILDING);
        break;
    case 15:
        if (name) return "Anti-teleport Barrier";
        if (desc) return "Obstructs all teleportations by monsters in your sight.";
        if (cast) _begin(T_HEX_ANTI_TELE);
        break;
    case 16:
        if (name) return "Shock Cloak";
        if (desc) return "Gives lightning aura and a bonus to speed.";
        if (cast) _begin(T_HEX_SHOCK_CLOAK);
        break;
    case 17:
        if (name) return "Cure Critical Wounds";
        if (desc) return "Heals cut and HP greatly.";
        if (info) return dice_info_heal(_cure_critical_dice());
        if (cast) _begin(T_HEX_CURE_CRITICAL);
        break;
    case 18:
        if (name) return "Recharging";
        if (desc) return "It attempts to recharge a device using your mana for power.";
        power = plev * 2;
        if (info) return info_power(power);
        if (cast && !recharge_from_player(power)) return NULL;
        break;
    case 19:
        if (name) return "Animate Dead";
        if (desc) return "Raises corpses and skeletons from dead.";
        if (cast) _begin(T_HEX_ANIMATE_DEAD);
        break;
    case 20:
        if (name) return "Curse Armor";
        if (desc) return "Curse a piece of armour that you wielding.";
        if (cast && !_curse_armor()) return NULL;
        break;
    case 21:
        if (name) return "Cloak of Shadow";
        if (desc) return "Gives aura of shadow.";
        if (cast)
        {
            if (!_shadow_check())
            {
                msg_print("You are not wearing a cursed cloak.");
                return NULL;
            }
            _begin(T_HEX_SHADOW_CLOAK);
        }
        break;
    case 22:
        if (name) return "Pains to Mana";
        if (desc) return "Deals psychic damages to all monsters in sight, and drains some mana.";
        if (info) return dice_info_dam(_psi_drain_dice());
        if (cast) _begin(T_HEX_PAIN_TO_MANA);
        break;
    case 23:
        if (name) return "Eye for an Eye";
        if (desc) return "Returns same damage which you got to the monster which damaged you.";
        if (cast) _begin(T_HEX_EYE_FOR_EYE);
        break;
    case 24:
        if (name) return "Anti-multiply Barrier";
        if (desc) return "Obstructs all multiplying by monsters in entire floor.";
        if (cast) _begin(T_HEX_ANTI_MULTIPLY);
        break;
    case 25:
        if (name) return "Restore Life";
        if (desc) return "Restores life energy and status.";
        if (cast) _begin(T_HEX_RESTORE);
        break;
    case 26:
        if (name) return "Drain Curse Power";
        if (desc) return "Drains curse on your weapon and heals SP a little.";
        if (cast && !_drain_curse()) return FALSE;
        break;
    case 27:
        if (name) return "Swords to Vampires";
        if (desc) return "Gives vampiric ability to your weapon.";
        if (cast) _begin(T_HEX_VAMP_BLADE);
        break;
    case 28:
        if (name) return "Word of Stun";
        if (desc) return "Stuns all monsters in your sight.";
        if (cast) _begin(T_HEX_STUN);
        break;
    case 29:
        if (name) return "Moving into Shadow";
        if (desc) return "Teleports you close to a monster.";
        if (cast)
        {
            int i, dir;
            bool flag;
            point_t pos;

            for (i = 0; i < 3; i++)
            {
                int rng = plr->lev + 2;
                dun_cell_ptr cell;

                pos = target_pos(rng);
                if (!dun_pos_interior(cave, pos)) return FALSE;
                cell = dun_cell_at(cave, pos);

                flag = FALSE;

                for (dir = 0; dir < 8; dir++)
                {
                    point_t p = point_step(pos, ddd[dir]);
                    if (dir == 5) continue;
                    if(dun_mon_at(cave, p)) flag = TRUE;
                }

                if ( dun_mon_at(cave, pos)
                  || dun_plr_at(cave, pos)
                  || (cell->flags & CELL_VAULT)
                  || !cell_allow_plr(cell)
                  || point_fast_distance(plr->pos, pos) > rng )
                {
                    msg_print("Can not teleport to there.");
                    continue;
                }
                break;
            }

            if (flag && randint0(plev * plev / 2))
            {
                teleport_player_to(pos, 0L);
            }
            else
            {
                msg_print("Oops!");
                if (mut_present(MUT_ASTRAL_GUIDE))
                    energy_use /= 3;
                teleport_player(30, 0L);
            }

        }
        break;
    case 30:
        if (name) return "Anti-magic Barrier";
        if (desc) return "Obstructs all magic spell of monsters in your sight.";
        if (cast) _begin(T_HEX_ANTI_MAGIC);
        break;
    case 31:
        if (name) return "Revenge Sentence";
        if (desc) return "Fires an avenging ball of hell fire after a few turns.";
        if (info) return dice_info_dam(_revenge_dice());
        if (cast)
        {
            if (_vengeance.type)
            {
                msg_print("You are already waiting for revenge!");
                return NULL;
            }
            _begin(T_HEX_REVENGE);
        }
        break;
    }

    return "";
}

bool hex_stop_one(void) /* UI to stop a chosen spell at high enough level */
{
    int ct = hex_count(), i;
    doc_ptr doc;
    vec_ptr v;
    bool result = FALSE;

    if (!ct)
    {
        msg_print("You are not currently chanting any foul curses.");
        return FALSE;
    }
    if (ct == 1 || plr->lev < 35)
    {
        _end_all();
        return TRUE;
    }

    doc = doc_alloc(72);
    v = _current();
    assert(vec_length(v) == ct);

    doc_insert(doc, "<color:U>Stop which foul chant?</color>\n");
    for (i = 0; i < ct; i++)
    {
        plr_tim_ptr t = vec_get(v, i);
        plr_tim_info_ptr info = plr_tim_info_find(t->id);

        assert(info);
        doc_printf(doc, "  <color:y>%c)</color> %s\n", I2A(i), info->name);
    }
    doc_insert(doc, "\n  <color:y>l)</color> Stop All Chants\n");
    doc_insert(doc, "<color:y>ESC)</color> Cancel\n");

    Term_save();
    doc_sync_menu(doc);
    for (;;)
    {
        int c = inkey_special(TRUE);
        if (c == ESCAPE || c == 'q') break;
        if (c == 'l')
        {
            _end_all();
            result = TRUE;
            break;
        }
        if (isupper(c)) c = tolower(c);
        if (islower(c))
        {
            i = A2I(c);
            if (0 <= i && i < ct)
            {
                plr_tim_ptr t = vec_get(v, i);
                _end(t->id);
                result = TRUE;
                break;
            }
        }
    }
    Term_load();

    doc_free(doc);
    vec_free(v);
    return result;
}

void hex_stop_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Stop Chanting");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_ENERGY:
        var_set_int(res, 10);
        break;
    case SPELL_CAST:
        var_set_bool(res, hex_stop_one());
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
void hex_calc_bonuses(void)
{
    /* XXX note that the skillmaster does not use plr->realm1 ...
    if (plr->realm1 != REALM_HEX) return; */
    if (hex_count())
        plr->skills.stl -= 1 + hex_count();
    equip_for_each(_curse_armor_bonuses);
}
void hex_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (hex_count())
        add_flag(flgs, OF_DEC_STEALTH);
}
void hex_calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    if (obj_is_cursed(obj))
        _curse_weapon_bonuses(obj, info);

}
