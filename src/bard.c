#include "angband.h"

#include <assert.h>

/************************************************************************
 * Music
 *
 * We'll be using the player timer system in a creative way to track
 * and maintain the current song. Bards may only sing a single song
 * at a time. Music is shared with the Skillmaster.
 ************************************************************************/

/************************************************************************
 * Timer Helpers
 ************************************************************************/
static plr_tim_ptr _current(void)
{
    plr_tim_ptr t;
    for (t = plr->timers; t; t = t->next)
    {
        if (t->id >= T_SONG_BEGIN && t->id < T_SONG_END) return t;
    }
    return NULL;
}
int music_current(void)
{
    plr_tim_ptr t = _current();
    if (t && !(t->flags & TF_INTERRUPTED))
        return t->id;
    return 0;
}
int music_duration(void)
{
    plr_tim_ptr t = _current();
    if (t && !(t->flags & TF_INTERRUPTED))
        return t->count - 1;
    return 0;
}
static void _begin(int which)
{
    assert(!_current());
    plr_tim_add_aux(which, 1, current_spell_cost);

    set_action(ACTION_SING);
    plr->update |= PU_BONUS;
    plr->redraw |= PR_STATUS;
}
static void _end(void)
{
    plr_tim_ptr t = _current();
    if (t)
        plr_tim_remove(t->id);
}
void music_stop(void)
{
    if (_current())
        _end();
}
static void _off(plr_tim_ptr timer)
{
    /* Hack -- if called from set_action(), avoid recursive loop */
    if (plr->action == ACTION_SING) set_action(ACTION_NONE);
    plr->update |= PU_BONUS;
    plr->redraw |= PR_STATUS;
}
static int _spell_index(int song) /* maintaining a song grants proficiency (plr->spell_exp[i]) */
{ /* XXX cf spell_stats_gain_skill for the new spell system. There, we use spell names, not
     integer indices. We could keep skill with just the timer info name (_find_info(song)->name)
     and not need this ugly mapping ... but book based casting is still on the old system XXX */
    switch (song)
    {
    case T_SONG_SLOW: return 0;
    case T_SONG_BLESS: return 1;
    case T_SONG_STUN: return 3;
    case T_SONG_FLOW_OF_LIFE: return 4;
    case T_SONG_FEAR: return 6;
    case T_SONG_HERO: return 7;
    case T_SONG_DETECT: return 8;
    case T_SONG_PSI: return 9;
    case T_SONG_LORE: return 10;
    case T_SONG_STEALTH: return 11;
    case T_SONG_CONFUSE: return 12;
    case T_SONG_DOOMCALL: return 13;
    case T_SONG_CHARM: return 15;
    case T_SONG_DISINTEGRATE: return 16;
    case T_SONG_RESIST: return 17;
    case T_SONG_SPEED: return 18;
    case T_SONG_DISPEL: return 20;
    case T_SONG_SARUMAN: return 21;
    case T_SONG_QUAKE: return 24;
    case T_SONG_STASIS: return 25;
    case T_SONG_HEROS_POEM: return 27;
    case T_SONG_YAVANNA: return 28;
    case T_SONG_FINGOLFIN: return 31;
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

    s_ptr = &technic_info[REALM_MUSIC - MIN_TECHNIC][i];

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
    /* dispel magic interrupts the song, giving a short window
     * of vulnerability for some effects. the song is automatically
     * re-started next _tick, but the counter is reset. Some songs
     * improve with duration of singing. */
    timer->flags |= TF_INTERRUPTED;
    timer->count = 1;

    msg_print("Your singing is interrupted.");
    plr->action = ACTION_NONE;
    _dispel_stuff();
    plr->energy_need += ENERGY_NEED();
}
static bool _tick(plr_tim_ptr timer)
{
    if (_upkeep(timer))
    {
        timer->count++;
        if (timer->flags & TF_INTERRUPTED)
        {
            timer->flags &= ~TF_INTERRUPTED;
            msg_print("You restart singing.");
            plr->action = ACTION_SING;
            _dispel_stuff();
            return FALSE; /* XXX no effect this tick */
        }
        return TRUE;
    }
    /* end this song if upkeep failed (out of mana) */
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

/************************************************************************
 * Song of Holding
 ************************************************************************/
static bool _holding_on(plr_tim_ptr timer)
{
    msg_print("You start humming a slow, steady melody...");
    return TRUE;
}
static dice_t _holding_dice(void) { return spell_dice(0, 0, 5 + plr->lev); }
static void _holding_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _holding_dice();
        plr_project_los(GF_SLOW, dice_roll(d));
    }
}
static status_display_t _holding_display(plr_tim_ptr timer)
{
    return status_display_create("Holding", "Hg", _color(timer, TERM_L_UMBER));
}
static plr_tim_info_ptr _holding(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_SLOW, "Song of Holding");
    info->desc = "Your song slows nearby monsters.";
    info->on_f = _holding_on;
    info->tick_f = _holding_tick;
    info->status_display_f = _holding_display;
    return info;
}

/************************************************************************
 * Song of Blessing
 ************************************************************************/
static bool _blessing_on(plr_tim_ptr timer)
{
    msg_print("The holy power of the Music of the Ainur enters you...");
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
    return status_display_create("Blessing", "Bs", _color(timer, TERM_WHITE));
}
static plr_tim_info_ptr _blessing(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_BLESS, "Song of Blessing");
    info->desc = "You are blessed.";
    info->on_f = _blessing_on;
    info->tick_f = _blessing_tick;
    info->calc_bonuses_f = _blessing_bonus;
    info->calc_weapon_bonuses_f = _blessing_weapon_bonus;
    info->calc_shooter_bonuses_f = _blessing_shooter_bonus;
    info->status_display_f = _blessing_display;
    return info;
}

/************************************************************************
 * Stun Pattern
 ************************************************************************/
static bool _stun_on(plr_tim_ptr timer)
{
    msg_print("You weave a pattern of sounds to bewilder and daze...");
    return TRUE;
}
static dice_t _stun_dice(void) { return spell_dice(3 + plr->lev/10, 3, 0); }
static void _stun_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
        plr_project_los(GF_STUN, dice_roll(_stun_dice()));
}
static status_display_t _stun_display(plr_tim_ptr timer)
{
    return status_display_create("Stun", "St", _color(timer, TERM_L_BLUE));
}
static plr_tim_info_ptr _stun(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_STUN, "Stun Pattern");
    info->desc = "Your song stuns nearby monsters.";
    info->on_f = _stun_on;
    info->tick_f = _stun_tick;
    info->status_display_f = _stun_display;
    return info;
}

/************************************************************************
 * Flow of Life
 ************************************************************************/
static bool _life_on(plr_tim_ptr timer)
{
    msg_print("Life flows through you as you sing a song of healing...");
    return TRUE;
}
static dice_t _life_dice(void) { return spell_dice(2, 6, 0); }
static void _life_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _life_dice();
        hp_player(dice_roll(d));
    }
}
static status_display_t _life_display(plr_tim_ptr timer)
{
    return status_display_create("Flow", "Fl", _color(timer, TERM_YELLOW));
}
static plr_tim_info_ptr _life(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_FLOW_OF_LIFE, "Flow of Life");
    info->desc = "Your song heals you.";
    info->on_f = _life_on;
    info->tick_f = _life_tick;
    info->status_display_f = _life_display;
    return info;
}

/************************************************************************
 * Song of Fear
 ************************************************************************/
static bool _fear_on(plr_tim_ptr timer)
{
    msg_print("You start weaving a fearful pattern...");
    return TRUE;
}
static dice_t _fear_dice(void) { return spell_dice(0, 0, 5 + plr->lev); }
static void _fear_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _fear_dice();
        plr_project_los(GF_FEAR, dice_roll(d));
    }
}
static status_display_t _fear_display(plr_tim_ptr timer)
{
    return status_display_create("Fear", "F", _color(timer, TERM_VIOLET));
}
static plr_tim_info_ptr _fear(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_FEAR, "Song of Fear");
    info->desc = "Your song frightens nearby monsters.";
    info->on_f = _fear_on;
    info->tick_f = _fear_tick;
    info->status_display_f = _fear_display;
    return info;
}

/************************************************************************
 * Heroic Ballad
 ************************************************************************/
static void _hero_on_aux(plr_tim_ptr timer) /* shared with Hero's Poem */
{
    fear_clear_p();
    plr->update |= PU_HP;
}
static bool _hero_on(plr_tim_ptr timer)
{
    msg_print("You start singing a song of intense fighting...");
    _hero_on_aux(timer);
    return TRUE;
}
static void _hero_off(plr_tim_ptr timer)
{
    _off(timer);
    plr->update |= PU_HP;
}
static void _hero_dispel(plr_tim_ptr timer)
{
    _dispel(timer);
    plr->update |= PU_HP;
}
static void _hero_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _hero_bonus(plr_tim_ptr timer)
{
    if (plr_tim_find(T_HERO)) return; /* XXX no double dipping (quibble) */
    if (timer->flags & TF_INTERRUPTED) return;
    hero_bonus(timer);
}
static void _hero_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    if (plr_tim_find(T_HERO)) return;
    if (timer->flags & TF_INTERRUPTED) return;
    hero_flags(timer, flgs);
}
static void _hero_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    if (plr_tim_find(T_HERO)) return;
    if (timer->flags & TF_INTERRUPTED) return;
    hero_weapon_bonus(timer, obj, info);
}
static void _hero_shooter_bonus(plr_tim_ptr timer, obj_ptr obj, plr_shoot_info_ptr info)
{
    if (plr_tim_find(T_HERO)) return;
    if (timer->flags & TF_INTERRUPTED) return;
    hero_shooter_bonus(timer, obj, info);
}
/* XXX You may wonder about the wisdom of duplicating this from plr_tim.c (_hero).
 * But consider dispel magic ... that would and should remove T_HERO (from a potion,
 * say) but should not remove our song (which simply gets interrupted for a turn). XXX */
static status_display_t _hero_display(plr_tim_ptr timer)
{
    return status_display_create("Hero", "He", _color(timer, TERM_WHITE));
}
static plr_tim_info_ptr _hero(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_HERO, "Heroic Ballad");
    info->desc = "You are the stuff of legends.";
    info->on_f = _hero_on;
    info->off_f = _hero_off;
    info->dispel_f = _hero_dispel;
    info->tick_f = _hero_tick;
    info->calc_bonuses_f = _hero_bonus;
    info->flags_f = _hero_flags;
    info->calc_weapon_bonuses_f = _hero_weapon_bonus;
    info->calc_shooter_bonuses_f = _hero_shooter_bonus;
    info->status_display_f = _hero_display;
    return info;
}

/************************************************************************
 * Clairaudience
 ************************************************************************/
static bool _detect_on(plr_tim_ptr timer)
{
    msg_print("Your quiet music sharpens your sense of hearing...");
    return TRUE;
}
static void _detect_tick(plr_tim_ptr timer)
{
    int count;
    if (!_tick(timer)) return;

    /* XXX this is noticeably slow ... map_area will PR_MAP every tick.
     * Also, each call to detect traps|doors|stairs|recall re-iterates the
     * same range and could be combined into a single pass. XXX */

    count = timer->count - 1; /* cf _music_check which uses music_duration */
    if (plr->lev >= 40 && count >= 19) wiz_lite();
    else if (plr->lev >= 25 && count >= 11) map_area(DETECT_RAD_DEFAULT);

    if (plr->lev >= 20 && count >= 6)
    {
        /* detect_treasure(DETECT_RAD_DEFAULT);  XXX too annoying */
        detect_objects_gold(DETECT_RAD_DEFAULT);
        detect_objects_normal(DETECT_RAD_DEFAULT);
    }
    if (plr->lev >= 15 && count >= 3)
    {
        detect_monsters_invis(DETECT_RAD_DEFAULT);
        detect_monsters_normal(DETECT_RAD_DEFAULT);
    }
    detect_traps(DETECT_RAD_DEFAULT, TRUE);
    detect_doors(DETECT_RAD_DEFAULT);
    detect_stairs(DETECT_RAD_DEFAULT);
    detect_recall(DETECT_RAD_DEFAULT);
}
static status_display_t _detect_display(plr_tim_ptr timer)
{
    return status_display_create("Detect", "D", _color(timer, TERM_L_BLUE));
}
static plr_tim_info_ptr _detect(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_DETECT, "Clairaudience");
    info->desc = "Your song reveals information about your surroundings.";
    info->on_f = _detect_on;
    info->tick_f = _detect_tick;
    info->status_display_f = _detect_display;
    return info;
}

/************************************************************************
 * Soul Shriek
 ************************************************************************/
static bool _psi_on(plr_tim_ptr timer)
{
    msg_print("You begin a song of a tortured soul in pain...");
    return TRUE;
}
static dice_t _psi_dice(void) { return spell_dam_dice(1, 3*plr->lev/2, 0); }
static void _psi_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _psi_dice();
        plr_project_los(GF_PSI, dice_roll(d));
    }
}
static status_display_t _psi_display(plr_tim_ptr timer)
{
    return status_display_create("Shriek", "Sk", _color(timer, TERM_L_RED));
}
static plr_tim_info_ptr _psi(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_PSI, "Soul Shriek");
    info->desc = "Your song damages all visible monsters with psionic powers.";
    info->on_f = _psi_on;
    info->tick_f = _psi_tick;
    info->status_display_f = _psi_display;
    return info;
}

/************************************************************************
 * Song of Lore
 ************************************************************************/
static void _lore_imp(void)
{
    if (plr_burst(1, GF_IDENTIFY, 0))
        plr->window |= PW_OBJECT_LIST;
}
static bool _lore_on(plr_tim_ptr timer)
{
    msg_print("You recall the rich lore of the world...");
    _lore_imp();
    return TRUE;
}
static void _lore_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        _lore_imp();
        if (plr->lev >= 25 && timer->count > 5) /* sorcery gets this at CL30 */
            mass_identify(FALSE);
    }
}
static status_display_t _lore_display(plr_tim_ptr timer)
{
    return status_display_create("Lore", "Id", _color(timer, TERM_L_BLUE));
}
static plr_tim_info_ptr _lore(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_LORE, "Song of Lore");
    info->desc = "Your song reveals the identity of all adjacent items.";
    info->on_f = _lore_on;
    info->tick_f = _lore_tick;
    info->status_display_f = _lore_display;
    return info;
}

/************************************************************************
 * Song of Stealth
 ************************************************************************/
static bool _stealth_on(plr_tim_ptr timer)
{
    msg_print("Your song carries you beyond the sight of mortal eyes...");
    return TRUE;
}
static void _stealth_off(plr_tim_ptr timer)
{
    msg_print("You are no longer hidden.");
    _off(timer);
}
static void _stealth_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _stealth_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->skills.stl += 99;
}
static void _stealth_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    if (timer->flags & TF_INTERRUPTED) return;
    add_flag(flgs, OF_STEALTH);
}
static status_display_t _stealth_display(plr_tim_ptr timer)
{
    return status_display_create("Stealth", "Sl", _color(timer, TERM_L_DARK));
}
static plr_tim_info_ptr _stealth(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_STEALTH, "Song of Stealth");
    info->desc = "You are hidden beyond the sight of mortal eyes.";
    info->on_f = _stealth_on;
    info->off_f = _stealth_off;
    info->tick_f = _stealth_tick;
    info->calc_bonuses_f = _stealth_bonus;
    info->flags_f = _stealth_flags;
    info->status_display_f = _stealth_display;
    return info;
}

/************************************************************************
 * Illusion Pattern
 ************************************************************************/
static bool _confuse_on(plr_tim_ptr timer)
{
    msg_print("You weave a pattern of sounds to beguile and confuse...");
    return TRUE;
}
static dice_t _confuse_dice(void) { return spell_dice(0, 0, 2*plr->lev); }
static void _confuse_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _confuse_dice();
        plr_project_los(GF_OLD_CONF, dice_roll(d));
    }
}
static status_display_t _confuse_display(plr_tim_ptr timer)
{
    return status_display_create("Confuse", "Cf", _color(timer, TERM_L_UMBER));
}
static plr_tim_info_ptr _confuse(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_CONFUSE, "Illusion Pattern");
    info->desc = "Your song confuses nearby monsters.";
    info->on_f = _confuse_on;
    info->tick_f = _confuse_tick;
    info->status_display_f = _confuse_display;
    return info;
}

/************************************************************************
 * Doomcall
 ************************************************************************/
static bool _doomcall_on(plr_tim_ptr timer)
{
    msg_print("The fury of the Downfall of Numenor lashes out...");
    return TRUE;
}
static dice_t _doomcall_dice(void) { return spell_dam_dice(10 + plr->lev/5, 7, 0); }
static void _doomcall_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _doomcall_dice();
        plr_project_los(GF_SOUND, dice_roll(d));
    }
}
static status_display_t _doomcall_display(plr_tim_ptr timer)
{
    return status_display_create("Doomcall", "D", _color(timer, TERM_ORANGE));
}
static plr_tim_info_ptr _doomcall(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_DOOMCALL, "Doomcall");
    info->desc = "Your song painfully stuns nearby monsters.";
    info->on_f = _doomcall_on;
    info->tick_f = _doomcall_tick;
    info->status_display_f = _doomcall_display;
    return info;
}

/************************************************************************
 * Fellowship Chant
 ************************************************************************/
static bool _charm_on(plr_tim_ptr timer)
{
    msg_print("You weave a slow, soothing melody of imploration...");
    return TRUE;
}
static dice_t _charm_dice(void) { return spell_dice(10 + plr->lev/15, 6, 0); }
static void _charm_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _charm_dice();
        plr_project_los(GF_CHARM, dice_roll(d));
    }
}
static status_display_t _charm_display(plr_tim_ptr timer)
{
    return status_display_create("Fellowship", "Fp", _color(timer, TERM_L_BLUE));
}
static plr_tim_info_ptr _charm(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_CHARM, "Fellowship Chant");
    info->desc = "Your song charms nearby monsters.";
    info->on_f = _charm_on;
    info->tick_f = _charm_tick;
    info->status_display_f = _charm_display;
    return info;
}

/************************************************************************
 * Sound of Disintegration
 ************************************************************************/
static bool _disintegrate_on(plr_tim_ptr timer)
{
    msg_print("You weave a violent pattern of sounds to break wall.");
    return TRUE;
}
static void _disintegrate_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        gf_affect_f(who_create_plr(), plr->pos, GF_DISINTEGRATE, 0, GF_AFFECT_SPELL);
        gf_affect_o(who_create_plr(), plr->pos, GF_DISINTEGRATE, 0, GF_AFFECT_SPELL);
    }
}
static void _disintegrate_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->kill_wall = TRUE;
}
static status_display_t _disintegrate_display(plr_tim_ptr timer)
{
    return status_display_create("Disintegrate", "Di", _color(timer, TERM_L_DARK));
}
static plr_tim_info_ptr _disintegrate(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_DISINTEGRATE, "Sound of Disintegration");
    info->desc = "Your song destroys nearby objects and walls.";
    info->on_f = _disintegrate_on;
    info->tick_f = _disintegrate_tick;
    info->calc_bonuses_f = _disintegrate_bonus;
    info->status_display_f = _disintegrate_display;
    return info;
}

/************************************************************************
 * Finrod's Resistance
 ************************************************************************/
static bool _resist_on(plr_tim_ptr timer)
{
    msg_print("You sing a song of perseverance against powers...");
    return TRUE;
}
static void _resist_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _resist_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    /* XXX bonus allowed to stack with !Resistance */
    res_add(GF_ACID);
    res_add(GF_ELEC);
    res_add(GF_FIRE);
    res_add(GF_COLD);
    res_add(GF_POIS);
}
static void _resist_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    if (timer->flags & TF_INTERRUPTED) return;
    add_flag(flgs, OF_RES_(GF_ACID));
    add_flag(flgs, OF_RES_(GF_ELEC));
    add_flag(flgs, OF_RES_(GF_FIRE));
    add_flag(flgs, OF_RES_(GF_COLD));
    add_flag(flgs, OF_RES_(GF_POIS));
}
static status_display_t _resist_display(plr_tim_ptr timer)
{
    return status_display_create("Finrod", "Rs", _color(timer, TERM_L_GREEN));
}
static plr_tim_info_ptr _resist(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_RESIST, "Finrod's Resistance");
    info->desc = "Your song is protecting you from the ravages of the elements.";
    info->on_f = _resist_on;
    info->tick_f = _resist_tick;
    info->calc_bonuses_f = _resist_bonus;
    info->flags_f = _resist_flags;
    info->status_display_f = _resist_display;
    return info;
}

/************************************************************************
 * Hobbit Melodies
 ************************************************************************/
static bool _speed_on(plr_tim_ptr timer)
{
    msg_print("You start singing a joyful pop song...");
    return TRUE;
}
static void _speed_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _speed_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    if (!plr->riding)
        plr_bonus_speed(10);
}
static void _speed_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    if (timer->flags & TF_INTERRUPTED) return;
    add_flag(flgs, OF_SPEED);
}
static status_display_t _speed_display(plr_tim_ptr timer)
{
    return status_display_create("Hobbit", "Hb", _color(timer, TERM_YELLOW));
}
static plr_tim_info_ptr _speed(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_SPEED, "Hobbit Melodies");
    info->desc = "Your song is granting enhanced speed.";
    info->on_f = _speed_on;
    info->tick_f = _speed_tick;
    info->calc_bonuses_f = _speed_bonus;
    info->flags_f = _speed_flags;
    info->status_display_f = _speed_display;
    return info;
}

/************************************************************************
 * Dispelling Chant
 ************************************************************************/
static bool _dispelling_on(plr_tim_ptr timer)
{
    msg_print("You cry out in an ear-wracking voice...");
    return TRUE;
}
static dice_t _dispelling_dice(void) { return spell_dam_dice(1, 3*plr->lev, 0); }
static void _dispelling_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _dispelling_dice();
        plr_project_los(GF_DISP_ALL, dice_roll(d));
        plr_project_los(GF_DISP_EVIL, dice_roll(d));
    }
}
static status_display_t _dispelling_display(plr_tim_ptr timer)
{
    return status_display_create("Dispel", "Dp", _color(timer, TERM_L_RED));
}
static plr_tim_info_ptr _dispelling(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_DISPEL, "Dispelling Chant");
    info->desc = "Your song is damaging nearby monsters.";
    info->on_f = _dispelling_on;
    info->tick_f = _dispelling_tick;
    info->status_display_f = _dispelling_display;
    return info;
}

/************************************************************************
 * Voice of Saruman
 ************************************************************************/
static bool _saruman_on(plr_tim_ptr timer)
{
    msg_print("You start humming a gentle and attractive song...");
    return TRUE;
}
static dice_t _saruman_dice(void) { return spell_dice(0, 0, 2*plr->lev); }
static void _saruman_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _saruman_dice();
        plr_project_los(GF_SLOW, dice_roll(d));
        plr_project_los(GF_SLEEP, dice_roll(d));
    }
}
static status_display_t _saruman_display(plr_tim_ptr timer)
{
    return status_display_create("Saruman", "Sm", _color(timer, TERM_RED));
}
static plr_tim_info_ptr _saruman(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_SARUMAN, "Voice of Saruman");
    info->desc = "Your song slows and sleeps nearby monsters.";
    info->on_f = _saruman_on;
    info->tick_f = _saruman_tick;
    info->status_display_f = _saruman_display;
    return info;
}

/************************************************************************
 * Wrecking Pattern
 ************************************************************************/
static bool _quake_on(plr_tim_ptr timer)
{
    msg_print("You weave a pattern of sounds to contort and shatter...");
    return TRUE;
}
static void _quake_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
        earthquake(plr->pos, 10);
}
static status_display_t _quake_display(plr_tim_ptr timer)
{
    return status_display_create("Quake", "Qk", _color(timer, TERM_L_UMBER));
}
static plr_tim_info_ptr _quake(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_QUAKE, "Wrecking Pattern");
    info->desc = "Your song shakes the surrounding dungeon.";
    info->on_f = _quake_on;
    info->tick_f = _quake_tick;
    info->status_display_f = _quake_display;
    return info;
}

/************************************************************************
 * Stationary Shriek
 ************************************************************************/
static bool _stasis_on(plr_tim_ptr timer)
{
    msg_print("You weave a very slow pattern which is almost likely to stop...");
    return TRUE;
}
static dice_t _stasis_dice(void) { return spell_dice(0, 0, 4*plr->lev); }
static void _stasis_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _stasis_dice();
        plr_project_los(GF_STASIS, dice_roll(d));
    }
}
static status_display_t _stasis_display(plr_tim_ptr timer)
{
    return status_display_create("Stasis", "F", _color(timer, TERM_BLUE));
}
static plr_tim_info_ptr _stasis(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_STASIS, "Stationary Shriek");
    info->desc = "Your song freezes nearby monsters.";
    info->on_f = _stasis_on;
    info->tick_f = _stasis_tick;
    info->status_display_f = _stasis_display;
    return info;
}

/************************************************************************
 * Hero's Poem
 *
 * Note: This song combines _hero and _speed songs with a dispel all effect.
 * It accordingly re-uses timer hooks defined above.
 ************************************************************************/
static bool _poem_on(plr_tim_ptr timer)
{
    msg_print("You chant a powerful, heroic call to arms...");
    _hero_on_aux(timer);
    return TRUE;
}
static dice_t _poem_dice(void) { return spell_dam_dice(1, 3*plr->lev, 0); }
static void _poem_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _poem_dice();
        plr_project_los(GF_DISP_ALL, dice_roll(d));
    }
}
static void _poem_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    _speed_bonus(timer);
    _hero_bonus(timer);
}
static void _poem_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    if (timer->flags & TF_INTERRUPTED) return;
    _speed_flags(timer, flgs);
    _hero_flags(timer, flgs);
}
static status_display_t _poem_display(plr_tim_ptr timer)
{
    return status_display_create("Hero", "He", _color(timer, TERM_VIOLET));
}
static plr_tim_info_ptr _poem(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_HEROS_POEM, "Hero's Poem");
    info->desc = "You song grants battle prowess and dispels nearby monsters.";
    info->on_f = _poem_on;
    info->off_f = _hero_off;
    info->dispel_f = _hero_dispel;
    info->tick_f = _poem_tick;
    info->calc_bonuses_f = _poem_bonus;
    info->flags_f = _poem_flags;
    info->calc_weapon_bonuses_f = _hero_weapon_bonus;
    info->calc_shooter_bonuses_f = _hero_shooter_bonus;
    info->status_display_f = _poem_display;
    return info;
}

/************************************************************************
 * Relief of Yavanna
 ************************************************************************/
static bool _yavanna_on(plr_tim_ptr timer)
{
    msg_print("Life flows through you as you sing the song...");
    return TRUE;
}
static dice_t _yavanna_dice(void) { return spell_dice(15, 10, 0); }
static void _yavanna_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _yavanna_dice();
        hp_player(dice_roll(d));
        plr_tim_remove(T_STUN);
        plr_tim_remove(T_CUT);
    }
}
static status_display_t _yavanna_display(plr_tim_ptr timer)
{
    return status_display_create("Yavanna", "Yv", _color(timer, TERM_YELLOW));
}
static plr_tim_info_ptr _yavanna(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_YAVANNA, "Relief of Yavanna");
    info->desc = "Your song greatly restores your health.";
    info->on_f = _yavanna_on;
    info->tick_f = _yavanna_tick;
    info->status_display_f = _yavanna_display;
    return info;
}

/************************************************************************
 * Fingolfin's Challenge
 ************************************************************************/
static bool _fingolfin_on(plr_tim_ptr timer)
{
    msg_print("You recall the valor of Fingolfin's challenge to the Dark Lord...");
    return TRUE;
}
static void _fingolfin_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _fingolfin_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    _speed_bonus(timer);
    ult_res_bonus(timer);
}
static void _fingolfin_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    if (timer->flags & TF_INTERRUPTED) return;
    _speed_flags(timer, flgs);
    ult_res_flags(timer, flgs);
}
static status_display_t _fingolfin_display(plr_tim_ptr timer)
{
    return status_display_create("Fingolfin", "Fn", _color(timer, TERM_VIOLET));
}
static plr_tim_info_ptr _fingolfin(void)
{
    plr_tim_info_ptr info = _alloc(T_SONG_FINGOLFIN, "Fingolfin's Challenge");
    info->desc = "Your song is granting ultimate resistance.";
    info->on_f = _fingolfin_on;
    info->tick_f = _fingolfin_tick;
    info->calc_bonuses_f = _fingolfin_bonus;
    info->flags_f = _fingolfin_flags;
    info->status_display_f = _fingolfin_display;
    return info;
}

/************************************************************************
 * Timer Registration
 ************************************************************************/
void music_register_timers(void)
{
    plr_tim_register(_holding());
    plr_tim_register(_blessing());
    plr_tim_register(_stun());
    plr_tim_register(_life());
    plr_tim_register(_fear());
    plr_tim_register(_hero());
    plr_tim_register(_detect());
    plr_tim_register(_psi());
    plr_tim_register(_lore());
    plr_tim_register(_stealth());
    plr_tim_register(_confuse());
    plr_tim_register(_doomcall());
    plr_tim_register(_charm());
    plr_tim_register(_disintegrate());
    plr_tim_register(_resist());
    plr_tim_register(_speed());
    plr_tim_register(_dispelling());
    plr_tim_register(_saruman());
    plr_tim_register(_quake());
    plr_tim_register(_stasis());
    plr_tim_register(_poem());
    plr_tim_register(_yavanna());
    plr_tim_register(_fingolfin());
}

/************************************************************************
 * Spells
 ************************************************************************/
cptr do_music_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
    bool fail = (mode == SPELL_FAIL) ? TRUE : FALSE;

    int rad = DETECT_RAD_DEFAULT;
    dice_t dice = {0};

    dice.scale = spell_power(1000);

    /* Stop singing before starting another ... every spell needs this. */
    if (cast || fail) _end();

    switch (spell)
    {
    case 0:
        if (name) return "Song of Holding";
        if (desc) return "Attempts to slow all monsters in sight.";
        if (info) return dice_info_power(_holding_dice());
        if (cast) _begin(T_SONG_SLOW);
        break;
    case 1:
        if (name) return "Song of Blessing";
        if (desc) return "Gives bonus to hit and AC for a few turns.";
        if (cast) _begin(T_SONG_BLESS);
        break;
    case 2:
        if (name) return "Wrecking Note";
        if (desc) return "Fires a bolt of sound.";
        dice.dd = 4 + (plr->lev - 1)/5;
        dice.ds = 4;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_bolt(GF_SOUND, dice)) return NULL;
        break;
    case 3:
        if (name) return "Stun Pattern";
        if (desc) return "Attempts to stun all monsters in sight.";
        if (info) return dice_info_power(_stun_dice());
        if (cast) _begin(T_SONG_STUN);
        break;
    case 4:
        if (name) return "Flow of Life";
        if (desc) return "Heals HP a little.";
        if (info) return dice_info_heal(_life_dice());
        if (cast) _begin(T_SONG_FLOW_OF_LIFE);
        break;
    case 5:
        if (name) return "Song of the Sun";
        if (desc) return "Lights up nearby area and the inside of a room permanently.";
        dice.dd = 2;
        dice.ds = plr->lev/2;
        if (info) return dice_info_dam(dice);
        if (cast) {
            msg_print("Your uplifting song brings brightness to dark places...");
            lite_area(dice_roll(dice), 1 + plr->lev/10);
        }
        break;
    case 6:
        if (name) return "Song of Fear";
        if (desc) return "Attempts to scare all monsters in sight.";
        if (info) return dice_info_power(_fear_dice());
        if (cast) _begin(T_SONG_FEAR);
        break;
    case 7:
        if (name) return "Heroic Ballad";
        if (desc) return "Removes fear, and gives bonus to hit and 10 more HP for a while.";
        if (cast) _begin(T_SONG_HERO);
        break;
    case 8:
        if (name) return "Clairaudience";
        if (desc) return "Detects traps, doors and stairs in your vicinity. And detects all "
                         "monsters at level 15, treasures and items at level 20. Maps nearby "
                         "area at level 25. Lights and know the whole level at level 40. The "
                         "longer you maintain the song, the stronger and more effective the "
                         "detection.";
        if (cast) _begin(T_SONG_DETECT);
        break;
    case 9:
        if (name) return "Soul Shriek";
        if (desc) return "Damages all monsters in sight with PSI damages.";
        if (info) return dice_info_dam(_psi_dice());
        if (cast) _begin(T_SONG_PSI);
        break;
    case 10:
        if (name) return "Song of Lore";
        if (desc) return "Identifies all items which are in the adjacent squares.";
        if (cast) _begin(T_SONG_LORE);
        break;
    case 11:
        if (name) return "Hiding Tune";
        if (desc) return "Gives improved stealth.";
        if (cast) _begin(T_SONG_STEALTH);
        break;
    case 12:
        if (name) return "Illusion Pattern";
        if (desc) return "Attempts to confuse all monsters in sight.";
        if (info) return dice_info_power(_confuse_dice());
        if (cast) _begin(T_SONG_CONFUSE);
        break;
    case 13:
        if (name) return "Doomcall";
        if (desc) return "Damages all monsters in sight with booming sound.";
        if (info) return dice_info_dam(_doomcall_dice());
        if (cast) _begin(T_SONG_DOOMCALL);
        break;
    case 14:
        if (name) return "Firiel's Song";
        if (desc) return "Resurrects nearby corpse and skeletons. And makes these your pets.";
        if (cast) {
            msg_print("The themes of life and revival are woven into your song...");
            plr_animate_dead();
        }
        break;
    case 15:
        if (name) return "Fellowship Chant";
        if (desc) return "Attempts to charm all monsters in sight.";
        if (info) return dice_info_power(_charm_dice());
        if (cast) _begin(T_SONG_CHARM);
        break;
    case 16:
        if (name) return "Sound of Disintegration";
        if (desc) return "Makes you be able to burrow into walls. Objects under your feet evaporate.";
        if (cast) _begin(T_SONG_DISINTEGRATE);
        break;
    case 17:
        if (name) return "Finrod's Resistance";
        if (desc) return "Gives resistance to fire, cold, electricity, acid and poison.";
        if (cast) _begin(T_SONG_RESIST);
        break;
    case 18:
        if (name) return "Hobbit Melodies";
        if (desc) return "Hastes you.";
        if (cast) _begin(T_SONG_SPEED);
        break;
    case 19:
        if (name) return "World Contortion";
        if (desc) return "Teleports all nearby monsters away unless resisted.";
        rad = 1 + plr->lev/15;
        dice.base = 3*plr->lev/2;
        if (info) return dice_info_power(dice);
        if (cast) {
            msg_print("Reality whirls wildly as you sing a dizzying melody...");
            plr_burst(rad, GF_TELEPORT, dice_roll(dice));
        }
        break;
    case 20:
        if (name) return "Dispelling Chant";
        if (desc) return "Damages all monsters in sight. Hurts evil monsters greatly.";
        if (info) return dice_info_dam(_dispelling_dice());
        if (cast) _begin(T_SONG_DISPEL);
        break;
    case 21:
        if (name) return "The Voice of Saruman";
        if (desc) return "Attempts to slow and sleep all monsters in sight.";
        if (info) return dice_info_power(_saruman_dice());
        if (cast) _begin(T_SONG_SARUMAN);
        break;
    case 22:
        if (name) return "Song of the Tempest";
        if (desc) return "Fires a beam of sound.";
        dice.dd = 15 + (plr->lev - 1)/2;
        dice.ds = 10;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_beam(GF_SOUND, dice)) return NULL;
        break;
    case 23:
        if (name) return "Ambarkanta";
        if (desc) return "Recreates current dungeon level.";
        if (cast) {
            msg_print("You sing of the primeval shaping of Middle-earth...");
            alter_reality();
        }
        break;
    case 24:
        if (name) return "Wrecking Pattern";
        if (desc) return "Shakes dungeon structure, and results in random swapping of floors and walls.";
        if (cast) _begin(T_SONG_QUAKE);
        break;
    case 25:
        if (name) return "Stationary Shriek";
        if (desc) return "Attempts to freeze all monsters in sight.";
        if (info) return dice_info_power(_stasis_dice());
        if (cast) _begin(T_SONG_STASIS);
        break;
    case 26:
        if (name) return "Endurance";
        if (desc) return "Sets a glyph on the floor beneath you. Monsters cannot attack you if you are on a glyph, but can try to break glyph.";
        if (cast) {
            msg_print("The holy power of the Music is creating a sacred field...");
            warding_glyph();
        }
        break;
    case 27:
        if (name) return "The Hero's Poem";
        if (desc) return "Hastes you. Gives heroism. Damages all monsters in sight.";
        if (info) return dice_info_dam(_poem_dice());
        if (cast) _begin(T_SONG_HEROS_POEM);
        break;
    case 28:
        if (name) return "Relief of Yavanna";
        if (desc) return "Powerful healing song. Also heals cut and stun completely.";
        if (info) return dice_info_heal(_yavanna_dice());
        if (cast) _begin(T_SONG_YAVANNA);
        break;
    case 29:
        if (name) return "Goddess' Rebirth";
        if (desc) return "Restores all stats and experience.";
        if (cast) {
            msg_print("You strew light and beauty in the dark as you sing. You feel refreshed.");
            do_res_stat(A_STR);
            do_res_stat(A_INT);
            do_res_stat(A_WIS);
            do_res_stat(A_DEX);
            do_res_stat(A_CON);
            do_res_stat(A_CHR);
            restore_level();
            plr_restore_life(1000);
        }
        break;
    case 30:
        if (name) return "Wizardry of Sauron";
        if (desc) return "Fires an extremely powerful tiny ball of sound.";
        dice.dd = 50 + plr->lev;
        dice.ds = 10;
        dice.base = plr->to_d_spell;
        if (info) return dice_info_dam(dice);
        if (cast && !plr_cast_ball(0, GF_SOUND, dice)) return NULL;
        break;
    case 31:
        if (name) return "Fingolfin's Challenge";
        if (desc) return "Grants mighty resistance to withstand The Great Enemy.";
        if (cast) _begin(T_SONG_FINGOLFIN);
        break;
    }
    return "";
}

void music_stop_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Stop Singing");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (_current())
        {
            _end();
            var_set_bool(res, TRUE);
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 1;
    spell->cost = 0;
    spell->fail = 0;
    spell->fn = music_stop_spell;

    return ct;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "song";
        me.which_stat = A_CHR;
        me.encumbrance.max_wgt = 400;
        me.encumbrance.weapon_pct = 67;
        me.encumbrance.enc_wgt = 800;
        me.realm1_choices = CH_MUSIC;
        init = TRUE;
    }
    return &me;
}

static void _calc_bonuses(void)
{
    res_add(GF_SOUND);
    if (equip_find_art("}.Daeron") || equip_find_art("}.Maglor"))
    {
        plr->dec_mana++;
        plr->easy_spell++;
    }
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_SOUND));
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_SHORT_SWORD, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_spellbooks();
}

plr_class_ptr bard_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  33,  34,  -5,  16,  20,  34,  20};
    skills_t xs = { 40,  65,  55,   0,   0,   0,  50,  40};

        me = plr_class_alloc(CLASS_BARD);
        me->name = "Bard";
        me->desc = "Bards are something like traditional musicians. Their magical "
                    "attacks are sound-based, and last as long as the Bard has mana. "
                    "Although a bard cannot sing two or more songs at the same time, he "
                    "or she does have the advantage that many songs affect all areas in "
                    "sight. A bard's prime statistic is charisma.\n \n"
                    "The songs are found in four songbooks, of which two are sold in "
                    "town. There is a special feature of music; many songs continue to "
                    "be sung until either the Bard chooses to stop, or he runs out of "
                    "the energy required to sing that type of song. Bards have a class "
                    "power - 'Stop Singing'.";

        me->stats[A_STR] = -2;
        me->stats[A_INT] =  1;
        me->stats[A_WIS] =  2;
        me->stats[A_DEX] = -1;
        me->stats[A_CON] = -2;
        me->stats[A_CHR] =  4;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 100;
        me->base_hp = 4;
        me->exp = 140;
        me->pets = 25;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_WEAK |
                   CLASS_SENSE2_MED | CLASS_SENSE2_STRONG;

        me->hooks.birth = _birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_flags = _get_flags;        
        me->hooks.caster_info = _caster_info;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = spellbook_character_dump;
    }

    return me;
}
