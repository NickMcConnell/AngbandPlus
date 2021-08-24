#include "angband.h"

#include <assert.h>

/************************************************************************
 * Benediction (REALM_BLESS)
 *
 * This is a good version of hex magic (Malediction).
 *
 * We'll be using the player timer system in a creative way to track
 * and maintain current chants. Players may weave multiple chants at
 * once depending upon level. Benediction is shared with the Skillmaster.
 ************************************************************************/


/************************************************************************
 * Timer Helpers
 ************************************************************************/
static int _count(void)
{
    plr_tim_ptr t;
    int ct = 0;
    for (t = plr->timers; t; t = t->next)
    {
        if (t->id >= T_BLESS_BEGIN && t->id < T_BLESS_END)
            ct++;
    }
    return ct;
}
int bless_count(void) 
{
    return _count();
}
int bless_max(void)
{
    return 1 + plr->lev/15;
}
static vec_ptr _current(void)
{
    vec_ptr v = vec_alloc(NULL);
    plr_tim_ptr t;
    for (t = plr->timers; t; t = t->next)
    {
        if (t->id >= T_BLESS_BEGIN && t->id < T_BLESS_END)
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
void bless_stop(void)
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
    case T_BLESS_BLESS: return 0;
    case T_BLESS_CURE_LIGHT: return 1;
    case T_BLESS_DETECT_EVIL: return 4;
    case T_BLESS_FRIENDSHIP: return 5;
    case T_BLESS_RESTORE: return 6;
    case T_BLESS_CURE_SERIOUS: return 7;
    case T_BLESS_CHASTITY: return 8;
    case T_BLESS_POVERTY: return 9;
    case T_BLESS_UNBARRING: return 10;
    case T_BLESS_PROT_EVIL: return 11;
    case T_BLESS_OBEDIENCE: return 12;
    case T_BLESS_BANISH_EVIL: return 13;
    case T_BLESS_CURE_CRITICAL: return 14;
    case T_BLESS_HOLY_CHANT: return 15;
    case T_BLESS_HERO: return 16;
    case T_BLESS_HOLY_AURA: return 17;
    case T_BLESS_REVELATION: return 20;
    case T_BLESS_RESIST: return 21;
    case T_BLESS_SPEED: return 22;
    case T_BLESS_HOLY_AVENGER: return 23;
    case T_BLESS_PEACE: return 24;
    case T_BLESS_GROWING: return 25;
    case T_BLESS_EXORCISM: return 26;
    case T_BLESS_REPEL: return 28;
    case T_BLESS_HEAL: return 29;
    case T_BLESS_JUSTICE: return 30;
    case T_BLESS_REPENTANCE: return 31;
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

    s_ptr = &technic_info[REALM_BLESS - MIN_TECHNIC][i];

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

/************************************************************************
 * Blessing
 ************************************************************************/
static bool _blessing_on(plr_tim_ptr timer)
{
    msg_print("You begin a holy chant of righteousness.");
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
    plr_tim_info_ptr info = _alloc(T_BLESS_BLESS, "Blessing");
    info->desc = "You are chanting a holy blessing.";
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
    msg_print("You begin a chant of healing.");
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
    plr_tim_info_ptr info = _alloc(T_BLESS_CURE_LIGHT, "Cure Light Wounds");
    info->desc = "Your chant heals you a small amount.";
    info->on_f = _cure_light_on;
    info->tick_f = _cure_light_tick;
    info->status_display_f = _cure_light_display;
    return info;
}

/************************************************************************
 * Detect Sinners
 ************************************************************************/
static bool _detect_evil_on(plr_tim_ptr timer)
{
    msg_print("You begin a holy chant to unmask evil thoughts.");
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
    plr_tim_info_ptr info = _alloc(T_BLESS_DETECT_EVIL, "Detect Sinners");
    info->desc = "Your chant is revealing the presence of evil.";
    info->on_f = _detect_evil_on;
    info->tick_f = _detect_evil_tick;
    info->calc_bonuses_f = _detect_evil_bonus;
    info->flags_f = _detect_evil_flags;
    info->status_display_f = _detect_evil_display;
    return info;
}

/************************************************************************
 * Friendship
 ************************************************************************/
static bool _friendship_on(plr_tim_ptr timer)
{
    msg_print("You begin a peaceful chant of friendship and love.");
    return TRUE;
}
static dice_t _friendship_dice(void) { return spell_dam_dice(0, 0, 5 + plr->lev); }
static void _friendship_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _friendship_dice();
        plr_project_los(GF_FRIENDSHIP, dice_roll(d));
    }
}
static status_display_t _friendship_display(plr_tim_ptr timer)
{
    return status_display_create("Friend", "Fd", _color(timer, TERM_ORANGE));
}
static plr_tim_info_ptr _friendship(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_FRIENDSHIP, "Friendship");
    info->desc = "Your chant soothes the hostile thoughts of nearby monsters.";
    info->on_f = _friendship_on;
    info->tick_f = _friendship_tick;
    info->status_display_f = _friendship_display;
    return info;
}

/************************************************************************
 * Restoration
 ************************************************************************/
static bool _restore_on(plr_tim_ptr timer)
{
    msg_print("You begin a holy chant of restoration and recovery.");
    return TRUE;
}
static bool _restore_check(void)
{
    /* slow down the recovery ... this chant is available very early on */
    return _1d(100) <= plr->lev;
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
            if (_restore_check())
            {
                if (d < r)
                    plr->exp = plr->max_exp;
                else
                    plr->exp += r;

                plr->notice |= PN_EXP;
            }
            flag = TRUE;
        }

        if (plr->clp < 1000)
        {
            if (_restore_check())
                plr_restore_life(50);
            flag = TRUE;
        }

        for (i = A_STR; i < 6; i ++)
        {
            if (plr->stat_cur[i] < plr->stat_max[i])
            {
                if (_restore_check())
                {
                    if (plr->stat_cur[i] < 18)
                        plr->stat_cur[i]++;
                    else
                        plr->stat_cur[i] += 10;

                    if (plr->stat_cur[i] > plr->stat_max[i])
                        plr->stat_cur[i] = plr->stat_max[i];

                    plr->update |= PU_BONUS;
                }
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
    return status_display_create("Restoration", "Rs", _color(timer, TERM_YELLOW));
}
static plr_tim_info_ptr _restore(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_RESTORE, "Restoration");
    info->desc = "Your chant restores life, experience and stats.";
    info->on_f = _restore_on;
    info->tick_f = _restore_tick;
    info->status_display_f = _restore_display;
    return info;
}

/************************************************************************
 * Cure Serious Wounds
 ************************************************************************/
static bool _cure_serious_on(plr_tim_ptr timer)
{
    msg_print("You begin a holy chant of healing.");
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
    plr_tim_info_ptr info = _alloc(T_BLESS_CURE_SERIOUS, "Cure Serious Wounds");
    info->desc = "Your chant heals you a serious amount.";
    info->on_f = _cure_serious_on;
    info->tick_f = _cure_serious_tick;
    info->status_display_f = _cure_serious_display;
    return info;
}

/************************************************************************
 * Chastity
 ************************************************************************/
static bool _chastity_on(plr_tim_ptr timer)
{
    msg_print("You begin a sacred chant to purify carnal lusts.");
    return TRUE;
}
static void _chastity_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _chastity_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->block_multiply = TRUE;
}
static status_display_t _chastity_display(plr_tim_ptr timer)
{
    return status_display_create("Chastity", "Cy", _color(timer, TERM_L_GREEN));
}
static plr_tim_info_ptr _chastity(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_CHASTITY, "Chastity");
    info->desc = "Your chant disrupts the lustful thoughts of nearby monsters.";
    info->on_f = _chastity_on;
    info->tick_f = _chastity_tick;
    info->calc_bonuses_f = _chastity_bonus;
    info->status_display_f = _chastity_display;
    return info;
}

/************************************************************************
 * Poverty
 ************************************************************************/
static bool _poverty_on(plr_tim_ptr timer)
{
    msg_print("You begin a sacred chant to purify the avarice of nearby monsters.");
    return TRUE;
}
static void _poverty_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _poverty_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->block_steal = TRUE;
}
static status_display_t _poverty_display(plr_tim_ptr timer)
{
    return status_display_create("Poverty", "Cy", _color(timer, TERM_L_GREEN));
}
static plr_tim_info_ptr _poverty(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_POVERTY, "Poverty");
    info->desc = "Your chant disrupts the greedy thoughts of nearby monsters.";
    info->on_f = _poverty_on;
    info->tick_f = _poverty_tick;
    info->calc_bonuses_f = _poverty_bonus;
    info->status_display_f = _poverty_display;
    return info;
}

/************************************************************************
 * Unbarring Ways
 ************************************************************************/
static bool _unbarring_on(plr_tim_ptr timer)
{
    msg_print("You begin a chant to remove obstacles to your holy quest.");
    return TRUE;
}
static void _unbarring_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
        destroy_doors_touch();
}
static status_display_t _unbarring_display(plr_tim_ptr timer)
{
    return status_display_create("Unbarring", "Un", _color(timer, TERM_L_BLUE));
}
static plr_tim_info_ptr _unbarring(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_UNBARRING, "Unbarring Ways");
    info->desc = "Your chant removes adjacent traps and doors.";
    info->on_f = _unbarring_on;
    info->tick_f = _unbarring_tick;
    info->status_display_f = _unbarring_display;
    return info;
}

/************************************************************************
 * Protection from Evil
 ************************************************************************/
static bool _repel_evil_on(plr_tim_ptr timer)
{
    msg_print("You begin a holy chant to block the physical assualts of evil.");
    return TRUE;
}
static void _repel_evil_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _repel_evil_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->repel_evil = TRUE;
}
static status_display_t _repel_evil_display(plr_tim_ptr timer)
{
    return status_display_create("PrtEvl", "Ev", _color(timer, TERM_L_UMBER));
}
static plr_tim_info_ptr _repel_evil(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_PROT_EVIL, "Protection from Evil");
    info->desc = "Your chant is repelling the melee attacks of evil monsters.";
    info->on_f = _repel_evil_on;
    info->tick_f = _repel_evil_tick;
    info->calc_bonuses_f = _repel_evil_bonus;
    info->status_display_f = _repel_evil_display;
    return info;
}

/************************************************************************
 * Obedience
 ************************************************************************/
static bool _obedience_on(plr_tim_ptr timer)
{
    msg_print("You begin a righteous chant of obedience and love.");
    return TRUE;
}
static dice_t _obedience_dice(void) { return spell_dam_dice(0, 0, 5 + plr->lev); }
static void _obedience_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _obedience_dice();
        plr_project_los(GF_OBEDIENCE, dice_roll(d));
    }
}
static status_display_t _obedience_display(plr_tim_ptr timer)
{
    return status_display_create("Obedience", "Ob", _color(timer, TERM_ORANGE));
}
static plr_tim_info_ptr _obedience(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_OBEDIENCE, "Obedience");
    info->desc = "Your chant commands nearby monsters to join your righteous cause.";
    info->on_f = _obedience_on;
    info->tick_f = _obedience_tick;
    info->status_display_f = _obedience_display;
    return info;
}

/************************************************************************
 * Banish Evil
 ************************************************************************/
static bool _banish_evil_on(plr_tim_ptr timer)
{
    msg_print("You begin a holy chant of banishment.");
    return TRUE;
}
static void _banish_evil_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
        plr_project_los(GF_AWAY_EVIL, MAX_SIGHT + 2*plr->lev);
}
static status_display_t _banish_evil_display(plr_tim_ptr timer)
{
    return status_display_create("Banish", "Bn", _color(timer, TERM_L_RED));
}
static plr_tim_info_ptr _banish_evil(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_BANISH_EVIL, "Banish Evil");
    info->desc = "Your chant banishes nearby evil monsters.";
    info->on_f = _banish_evil_on;
    info->tick_f = _banish_evil_tick;
    info->status_display_f = _banish_evil_display;
    return info;
}

/************************************************************************
 * Cure Critical Wounds
 ************************************************************************/
static bool _cure_critical_on(plr_tim_ptr timer)
{
    msg_print("You begin a holy chant of healing.");
    return TRUE;
}
static dice_t _cure_critical_dice(void) { return spell_dice(10, 10, 0); }
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
    return status_display_create("Cure Critical", "Cc", _color(timer, TERM_YELLOW));
}
static plr_tim_info_ptr _cure_critical(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_CURE_CRITICAL, "Cure Critical Wounds");
    info->desc = "Your chant heals you a critical amount.";
    info->on_f = _cure_critical_on;
    info->tick_f = _cure_critical_tick;
    info->status_display_f = _cure_critical_display;
    return info;
}

/************************************************************************
 * Holy Chant
 ************************************************************************/
static bool _holy_chant_on(plr_tim_ptr timer)
{
    msg_print("You begin a powerful chant of righteous fury.");
    return TRUE;
}
static dice_t _holy_chant_dice(void) { return spell_dam_dice(1, 2*plr->lev, plr->lev); }
static void _holy_chant_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _holy_chant_dice();
        plr_project_los(GF_DISP_EVIL, dice_roll(d));
        hp_player(plr->lev);
        plr_tim_remove(T_STUN);
        plr_tim_remove(T_CUT);
    }
}
static status_display_t _holy_chant_display(plr_tim_ptr timer)
{
    return status_display_create("Holy Chant", "Hc", _color(timer, TERM_VIOLET));
}
static plr_tim_info_ptr _holy_chant(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_HOLY_CHANT, "Holy Chant");
    info->desc = "Your chant dispels nearby evil monsters as well as healing yourself.";
    info->on_f = _holy_chant_on;
    info->tick_f = _holy_chant_tick;
    info->status_display_f = _holy_chant_display;
    return info;
}

/************************************************************************
 * Heroism
 ************************************************************************/
static void _hero_on_aux(plr_tim_ptr timer)
{
    fear_clear_p();
    plr->update |= PU_HP;
}
static bool _hero_on(plr_tim_ptr timer)
{
    msg_print("You begin a holy chant of battle.");
    _hero_on_aux(timer);
    return TRUE;
}
static void _hero_off(plr_tim_ptr timer)
{
    _off(timer);
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
static status_display_t _hero_display(plr_tim_ptr timer)
{
    return status_display_create("Hero", "He", _color(timer, TERM_WHITE));
}
static plr_tim_info_ptr _hero(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_HERO, "Heroism");
    info->desc = "Your chant grants legendary prowess in battle.";
    info->on_f = _hero_on;
    info->off_f = _hero_off;
    info->tick_f = _hero_tick;
    info->calc_bonuses_f = _hero_bonus;
    info->flags_f = _hero_flags;
    info->calc_weapon_bonuses_f = _hero_weapon_bonus;
    info->calc_shooter_bonuses_f = _hero_shooter_bonus;
    info->status_display_f = _hero_display;
    return info;
}

/************************************************************************
 * Holy Aura
 ************************************************************************/
static bool _holy_aura_on(plr_tim_ptr timer)
{
    msg_print("You begin a holy chant to punish the melee attacks of evil monsters.");
    return TRUE;
}
static void _holy_aura_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _holy_aura_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->sh_holy = TRUE;
}
static status_display_t _holy_aura_display(plr_tim_ptr timer)
{
    return status_display_create("Holy Aura", "(H", _color(timer, TERM_WHITE));
}
static plr_tim_info_ptr _holy_aura(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_HOLY_AURA, "Holy Aura");
    info->desc = "Your chant punishes the melee attacks of evil monsters with a holy fire.";
    info->on_f = _holy_aura_on;
    info->tick_f = _holy_aura_tick;
    info->calc_bonuses_f = _holy_aura_bonus;
    info->status_display_f = _holy_aura_display;
    return info;
}

/************************************************************************
 * Revelation
 ************************************************************************/
static bool _revelation_on(plr_tim_ptr timer)
{
    msg_print("You begin a quiet chant to reveal the world around you.");
    return TRUE;
}
static void _lore_imp(void)
{
    if (plr_burst(1, GF_IDENTIFY, 0))
        plr->window |= PW_OBJECT_LIST;
}
static void _revelation_tick(plr_tim_ptr timer)
{
    int count;
    if (!_tick(timer)) return;

    /* XXX this is noticeably slow ... map_area will PR_MAP every tick.
     * Also, each call to detect traps|doors|stairs|recall re-iterates the
     * same range and could be combined into a single pass. XXX */

    count = timer->count - 1;

    /* object lore */
    if (plr->lev >= 30 && count >= 10)
    {
        _lore_imp();
        if (plr->lev >= 40 && timer->count > 20) /* sorcery gets this at CL30 */
            mass_identify(FALSE);
    }
    if (plr->lev >= 20 && count >= 6)
    {
        detect_objects_gold(DETECT_RAD_DEFAULT);
        detect_objects_normal(DETECT_RAD_DEFAULT);
    }

    /* detection and mapping */
    if (plr->lev >= 25 && count >= 11) map_area(DETECT_RAD_DEFAULT);
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
static status_display_t _revelation_display(plr_tim_ptr timer)
{
    return status_display_create("Revelation", "Rv", _color(timer, TERM_L_BLUE));
}
static plr_tim_info_ptr _revelation(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_REVELATION, "Revelation");
    info->desc = "Your chant reveals information about your surroundings.";
    info->on_f = _revelation_on;
    info->tick_f = _revelation_tick;
    info->status_display_f = _revelation_display;
    return info;
}

/************************************************************************
 * Resistance
 ************************************************************************/
static bool _resist_on(plr_tim_ptr timer)
{
    msg_print("You begin a chant of elemental protection.");
    return TRUE;
}
static void _resist_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        int count = timer->count - 1;

        if ( (count == 15 && plr->lev >= 45)
          || (count == 10 && plr->lev >= 40)
          || count == 5 )
        {
            msg_print("You become more resistant.");
            plr->update |= PU_BONUS;
        }
    }
}
static void _resist_bonus(plr_tim_ptr timer)
{
    int count = timer->count - 1;
    if (timer->flags & TF_INTERRUPTED) return;
    res_add(GF_ACID);
    res_add(GF_ELEC);
    res_add(GF_FIRE);
    res_add(GF_COLD);
    res_add(GF_POIS);
    if (count >= 5)
    {
        res_add(GF_LIGHT);
        res_add(GF_DARK);
        res_add(GF_CONFUSION);
    }
    if (count >= 10 && plr->lev >= 40)
    {
        res_add(GF_NETHER);
        res_add(GF_NEXUS);
    }
    if (count >= 15 && plr->lev >= 45)
    {
        res_add(GF_SOUND);
        res_add(GF_SHARDS);
        res_add(GF_CHAOS);
        res_add(GF_DISENCHANT);
        res_add(GF_FEAR);
    }
}
static void _resist_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    int count = timer->count - 1;
    if (timer->flags & TF_INTERRUPTED) return;
    add_flag(flgs, OF_RES_(GF_ACID));
    add_flag(flgs, OF_RES_(GF_ELEC));
    add_flag(flgs, OF_RES_(GF_FIRE));
    add_flag(flgs, OF_RES_(GF_COLD));
    add_flag(flgs, OF_RES_(GF_POIS));
    if (count >= 5)
    {
        add_flag(flgs, OF_RES_(GF_LIGHT));
        add_flag(flgs, OF_RES_(GF_DARK));
        add_flag(flgs, OF_RES_(GF_CONFUSION));
    }
    if (count >= 10 && plr->lev >= 40)
    {
        add_flag(flgs, OF_RES_(GF_NETHER));
        add_flag(flgs, OF_RES_(GF_NEXUS));
    }
    if (count >= 15 && plr->lev >= 45)
    {
        add_flag(flgs, OF_RES_(GF_SOUND));
        add_flag(flgs, OF_RES_(GF_SHARDS));
        add_flag(flgs, OF_RES_(GF_CHAOS));
        add_flag(flgs, OF_RES_(GF_DISENCHANT));
        add_flag(flgs, OF_RES_(GF_FEAR));
    }
}
static status_display_t _resist_display(plr_tim_ptr timer)
{
    return status_display_create("Resist", "Rs", _color(timer, TERM_L_GREEN));
}
static plr_tim_info_ptr _resist(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_RESIST, "Resistance");
    info->desc = "Your chant grants increasing resistances with time.";
    info->on_f = _resist_on;
    info->tick_f = _resist_tick;
    info->calc_bonuses_f = _resist_bonus;
    info->flags_f = _resist_flags;
    info->status_display_f = _resist_display;
    return info;
}

/************************************************************************
 * Sacred Channels
 ************************************************************************/
static bool _speed_on(plr_tim_ptr timer)
{
    msg_print("You begin a powerful chant of battle.");
    _hero_on_aux(timer);
    hp_player(plr->lev);
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
    _hero_bonus(timer);
}
static void _speed_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    if (timer->flags & TF_INTERRUPTED) return;
    add_flag(flgs, OF_SPEED);
    _hero_flags(timer, flgs);
}
static status_display_t _speed_display(plr_tim_ptr timer)
{
    return status_display_create("Channels", "Ch", _color(timer, TERM_VIOLET));
}
static plr_tim_info_ptr _speed(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_SPEED, "Sacred Channels");
    info->desc = "You chant grants battle prowess and increased speed.";
    info->on_f = _speed_on;
    info->off_f = _hero_off;
    info->tick_f = _speed_tick;
    info->calc_bonuses_f = _speed_bonus;
    info->flags_f = _speed_flags;
    info->calc_weapon_bonuses_f = _hero_weapon_bonus;
    info->calc_shooter_bonuses_f = _hero_shooter_bonus;
    info->status_display_f = _speed_display;
    return info;
}

/************************************************************************
 * Holy Avenger
 ************************************************************************/
static bool _holy_avenger_on(plr_tim_ptr timer)
{
    msg_print("You begin a holy chant of righteous vengeance.");
    return TRUE;
}
static void _holy_avenger_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _holy_avenger_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    if (timer->flags & TF_INTERRUPTED) return;
    add_flag(flgs, OF_SLAY_EVIL);
    add_flag(flgs, OF_SLAY_UNDEAD);
    add_flag(flgs, OF_SLAY_DEMON);
}
static void _holy_avenger_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    if (plr_tim_find(T_BLESSED)) return;
    if (timer->flags & TF_INTERRUPTED) return;

    add_flag(info->obj_flags, OF_SLAY_EVIL);
    add_flag(info->obj_flags, OF_SLAY_UNDEAD);
    add_flag(info->obj_flags, OF_SLAY_DEMON);

    add_flag(info->obj_known_flags, OF_SLAY_EVIL);
    add_flag(info->obj_known_flags, OF_SLAY_UNDEAD);
    add_flag(info->obj_known_flags, OF_SLAY_DEMON);

    if (obj_is_blessed(obj))
    {
        info->to_d += 5;
        info->dis_to_d += 5;
    }
}
static status_display_t _holy_avenger_display(plr_tim_ptr timer)
{
    return status_display_create("Avenger", "Av", _color(timer, TERM_RED));
}
static plr_tim_info_ptr _holy_avenger(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_HOLY_AVENGER, "Holy Avenger");
    info->desc = "You chant turns your melee weapon into a Holy Avenger.";
    info->on_f = _holy_avenger_on;
    info->tick_f = _holy_avenger_tick;
    info->flags_f = _holy_avenger_flags;
    info->calc_weapon_bonuses_f = _holy_avenger_weapon_bonus;
    info->status_display_f = _holy_avenger_display;
    return info;
}

/************************************************************************
 * Peace
 ************************************************************************/
static bool _peace_on(plr_tim_ptr timer)
{
    msg_print("You begin a humble chant of love and harmony.");
    return TRUE;
}
static void _peace_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _peace_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->innocence = TRUE;
}
static status_display_t _peace_display(plr_tim_ptr timer)
{
    return status_display_create("Peace", "Pc", _color(timer, TERM_WHITE));
}
static plr_tim_info_ptr _peace(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_PEACE, "Peace");
    info->desc = "Your peaceful chant allows you to pass amongst evil unscathed.";
    info->on_f = _peace_on;
    info->tick_f = _peace_tick;
    info->calc_bonuses_f = _peace_bonus;
    info->status_display_f = _peace_display;
    return info;
}

/************************************************************************
 * Growing
 ************************************************************************/
static bool _growing_on(plr_tim_ptr timer)
{
    msg_print("You begin a vibrant chant of life.");
    return TRUE;
}
static void _grow_imp(dun_ptr dun, point_t pos)
{
    dun_cell_ptr cell = dun_cell_at(dun, pos);
    if (!cell_is_floor(cell))
        return;
    else if (floor_is_floor(cell))
    {
        if (one_in_(3))
            dun_place_dirt(dun, pos);
    }
    else if (floor_is_dirt(cell))
    {
        if (one_in_(3))
            dun_place_grass(dun, pos);
    }
    else if (floor_is_grass(cell))
    {
        if (one_in_(6))
            dun_place_flower(dun, pos);
        else if (one_in_(6))
            dun_place_brake(dun, pos);
    }
    else if (floor_is_flower(cell) || floor_is_brake(cell))
    {
        if (one_in_(7))
            dun_place_tree(dun, pos);
    }
    else if (cell_is_tree(cell))
    {
        /* XXX wake up as an Ent (or Huorn) ... s/b very rare and deep! XXX */
    }
}
static void _growing_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
        plr_view_iter(_grow_imp);
}
static status_display_t _growing_display(plr_tim_ptr timer)
{
    return status_display_create("Growing", "Gr", _color(timer, TERM_GREEN));
}
static plr_tim_info_ptr _growing(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_GROWING, "Growing");
    info->desc = "Your chant brings life to your surroundings.";
    info->on_f = _growing_on;
    info->tick_f = _growing_tick;
    info->status_display_f = _growing_display;
    return info;
}

/************************************************************************
 * Exorcism
 ************************************************************************/
static bool _exorcism_on(plr_tim_ptr timer)
{
    msg_print("You begin a holy chant to remove evil spirits from the realm of the living.");
    return TRUE;
}
static dice_t _exorcism_dice(void) { return spell_dice(0, 0, 3*plr->lev); }
static void _exorcism_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _exorcism_dice();
        plr_project_los(GF_EXORCISM, dice_roll(d));
    }
}
static status_display_t _exorcism_display(plr_tim_ptr timer)
{
    return status_display_create("Exorcism", "Ex", _color(timer, TERM_WHITE));
}
static plr_tim_info_ptr _exorcism(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_EXORCISM, "Exorcism");
    info->desc = "Your chant removes evil spirits from the realm of the living.";
    info->on_f = _exorcism_on;
    info->tick_f = _exorcism_tick;
    info->status_display_f = _exorcism_display;
    return info;
}

/************************************************************************
 * Repel Monsters
 ************************************************************************/
static bool _repel_on(plr_tim_ptr timer)
{
    msg_print("You begin a holy chant to block the physical assualts of all monsters.");
    return TRUE;
}
static void _repel_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _repel_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->repel_monsters = TRUE;
}
static status_display_t _repel_display(plr_tim_ptr timer)
{
    return status_display_create("Repel", "Rp", _color(timer, TERM_L_UMBER));
}
static plr_tim_info_ptr _repel(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_REPEL, "Repel Monsters");
    info->desc = "Your chant is repelling the melee attacks of all monsters.";
    info->on_f = _repel_on;
    info->tick_f = _repel_tick;
    info->calc_bonuses_f = _repel_bonus;
    info->status_display_f = _repel_display;
    return info;
}

/************************************************************************
 * Healing
 ************************************************************************/
static bool _heal_on(plr_tim_ptr timer)
{
    msg_print("You begin a holy chant of healing.");
    return TRUE;
}
static dice_t _heal_dice(void) { return spell_dice(0, 0, 3*plr->lev); }
static void _heal_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _heal_dice();
        hp_player(dice_roll(d));
        plr_tim_remove(T_STUN);
        plr_tim_remove(T_CUT);
    }
}
static status_display_t _heal_display(plr_tim_ptr timer)
{
    return status_display_create("Healing", "Hl", _color(timer, TERM_YELLOW));
}
static plr_tim_info_ptr _heal(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_HEAL, "Healing");
    info->desc = "Your chant heals you powerfully.";
    info->on_f = _heal_on;
    info->tick_f = _heal_tick;
    info->status_display_f = _heal_display;
    return info;
}

/************************************************************************
 * Justice
 ************************************************************************/
static bool _justice_on(plr_tim_ptr timer)
{
    msg_print("You begin a righteous chant for justice.");
    return TRUE;
}
static void _justice_tick(plr_tim_ptr timer)
{
    _tick(timer);
}
static void _justice_bonus(plr_tim_ptr timer)
{
    if (timer->flags & TF_INTERRUPTED) return;
    plr->revenge = TRUE;
}
static status_display_t _justice_display(plr_tim_ptr timer)
{
    return status_display_create("Justice", "Js", _color(timer, TERM_VIOLET));
}
static plr_tim_info_ptr _justice(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_JUSTICE, "Justice");
    info->desc = "Your chant damages all monsters that damage you.";
    info->on_f = _justice_on;
    info->tick_f = _justice_tick;
    info->calc_bonuses_f = _justice_bonus;
    info->status_display_f = _justice_display;
    return info;
}

/************************************************************************
 * Repentance
 ************************************************************************/
static bool _repentance_on(plr_tim_ptr timer)
{
    msg_print("You begin a persuasive chant to sway the allegiance of others.");
    return TRUE;
}
static dice_t _repentance_dice(void) { return spell_dice(0, 0, 2*plr->lev); }
static void _repentance_tick(plr_tim_ptr timer)
{
    if (_tick(timer))
    {
        dice_t d = _repentance_dice();
        plr_project_los(GF_REPENTANCE, dice_roll(d));
    }
}
static status_display_t _repentance_display(plr_tim_ptr timer)
{
    return status_display_create("Repent", "Rp", _color(timer, TERM_ORANGE));
}
static plr_tim_info_ptr _repentance(void)
{
    plr_tim_info_ptr info = _alloc(T_BLESS_REPENTANCE, "Repentance");
    info->desc = "Your chant causes nearby summoned monsters to re-think their allegiances.";
    info->on_f = _repentance_on;
    info->tick_f = _repentance_tick;
    info->status_display_f = _repentance_display;
    return info;
}

/************************************************************************
 * Timers
 ************************************************************************/
void bless_register_timers(void)
{
    plr_tim_register(_blessing());
    plr_tim_register(_cure_light());
    plr_tim_register(_detect_evil());
    plr_tim_register(_friendship());
    plr_tim_register(_restore());
    plr_tim_register(_cure_serious());
    plr_tim_register(_chastity());
    plr_tim_register(_poverty());
    plr_tim_register(_unbarring());
    plr_tim_register(_repel_evil());
    plr_tim_register(_obedience());
    plr_tim_register(_banish_evil());
    plr_tim_register(_cure_critical());
    plr_tim_register(_holy_chant());
    plr_tim_register(_hero());
    plr_tim_register(_holy_aura());
    plr_tim_register(_revelation());
    plr_tim_register(_resist());
    plr_tim_register(_speed());
    plr_tim_register(_holy_avenger());
    plr_tim_register(_peace());
    plr_tim_register(_growing());
    plr_tim_register(_exorcism());
    plr_tim_register(_repel());
    plr_tim_register(_heal());
    plr_tim_register(_justice());
    plr_tim_register(_repentance());
}

/************************************************************************
 * Spells
 ************************************************************************/
cptr do_bless_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

    switch (spell)
    {
    /* [Chants and Blessings] */
    case 0:
        if (name) return "Blessing";
        if (desc) return "Maintaining this chant grants a bonus to AC and fighting prowess.";
        if (cast) _begin(T_BLESS_BLESS);
        break;
    case 1:
        if (name) return "Cure Light Wounds";
        if (desc) return "Maintaining this chant heals you a small amount every move.";
        if (info) return dice_info_heal(_cure_light_dice());
        if (cast) _begin(T_BLESS_CURE_LIGHT);
        break;
    case 2:
        if (name) return "Illumination";
        if (desc) return "This spell immediately lights up your surroundings.";
        if (cast) {
            msg_print("Your uplifting prayer brings brightness to dark places...");
            lite_area(_2d(plr->lev/2), 1 + plr->lev/10);
        }
        break;
    case 3:
        if (name) return "Bless Weapon";
        if (desc) return "This spell blesses a chosen weapon, allowing you to wield edged weapons without incurring a penalty. "
                         "Blessed items also resist being cursed and grant slight benefits when worn.";
        if (cast && !bless_weapon()) return NULL;
        break;
    case 4:
        if (name) return "Detect Sinners";
        if (desc) return "Maintaining this chant will reveal the ungodly.";
        if (cast) _begin(T_BLESS_DETECT_EVIL);
        break;
    case 5:
        if (name) return "Friendship";
        if (desc) return "Maintaining this chant will soothe the hostile thoughts of nearby monsters.";
        if (info) return dice_info_power(_friendship_dice());
        if (cast) _begin(T_BLESS_FRIENDSHIP);
        break;
    case 6:
        if (name) return "Restoration";
        if (desc) return "Maintaining this chant will restore your stats and experience.";
        if (cast) _begin(T_BLESS_RESTORE);
        break;
    case 7:
        if (name) return "Cure Serious Wounds";
        if (desc) return "Maintaining this chant heals you every move.";
        if (info) return dice_info_heal(_cure_serious_dice());
        if (cast) _begin(T_BLESS_CURE_SERIOUS);
        break;
    /* [Holy Vows] */
    case 8:
        if (name) return "Chastity";
        if (desc) return "Maintaining this chant purifies the carnal lusts of nearby monsters.";
        if (cast) _begin(T_BLESS_CHASTITY);
        break;
    case 9:
        if (name) return "Poverty";
        if (desc) return "Maintaining this chant purifies the avarice of nearby thieves.";
        if (cast) _begin(T_BLESS_POVERTY);
        break;
    case 10:
        if (name) return "Unbarring Ways";
        if (desc) return "Maintaining this chant removes nearby obstructions such as traps and doors.";
        if (cast) _begin(T_BLESS_UNBARRING);
        break;
    case 11:
        if (name) return "Protection from Evil";
        if (desc) return "Maintaining this chant protects you from the melee attacks of evil monsters.";
        if (cast) _begin(T_BLESS_PROT_EVIL);
        break;
    case 12:
        if (name) return "Obedience";
        if (desc) return "Maintaining this chant will induce a desire to obey you in nearby monsters.";
        if (info) return dice_info_power(_obedience_dice());
        if (cast) _begin(T_BLESS_OBEDIENCE);
        break;
    case 13:
        if (name) return "Banish Evil";
        if (desc) return "Maintaining this chant will banish all evil monsters from your sight.";
        if (cast) _begin(T_BLESS_BANISH_EVIL);
        break;
    case 14:
        if (name) return "Cure Critical Wounds";
        if (desc) return "Maintaining this chant heals you every move.";
        if (info) return dice_info_heal(_cure_critical_dice());
        if (cast) _begin(T_BLESS_CURE_CRITICAL);
        break;
    case 15:
        if (name) return "Holy Chant";
        if (desc) return "Maintaining this chant damages evil monsters and heals you every move.";
        if (info) return dice_info_dam(_holy_chant_dice());
        if (cast) _begin(T_BLESS_HOLY_CHANT);
        break;
    /* [Holy Infusions] */
    case 16:
        if (name) return "Heroism";
        if (desc) return "Maintaining this chant grants enhanced fighting skill and resistance to fear.";
        if (cast) _begin(T_BLESS_HERO);
        break;
    case 17:
        if (name) return "Holy Aura";
        if (desc) return "Maintaining the chant envelops you in a protective shield of Holy Fire.";
        if (cast) _begin(T_BLESS_HOLY_AURA);
        break;
    case 18:
        if (name) return "Bless Armor";
        if (desc) return "This spell blesses a chosen piece of armor. Blessed items resist being cursed and also grant slight benefits when worn.";
        if (cast && !bless_armor()) return NULL;
        break;
    case 19:
        if (name) return "Hallow Ground";
        if (desc) return "This spell sanctifies the ground beneath you preventing enemy monsters from attacking.";
        if (cast) dun_place_glyph_of_warding(cave, plr->pos);
        break;
    case 20:
        if (name) return "Revelation";
        if (desc) return "Maintaining this chant grants insight into your surroundings.";
        if (cast) _begin(T_BLESS_REVELATION);
        break;
    case 21:
        if (name) return "Resistance";
        if (desc) return "Maintaining this chant grants more and more resistances over time.";
        if (cast) _begin(T_BLESS_RESIST);
        break;
    case 22:
        if (name) return "Sacred Channels";
        if (desc) return "Maintaining this chant grants heroic speed.";
        if (cast) _begin(T_BLESS_SPEED);
        break;
    case 23:
        if (name) return "Holy Avenger";
        if (desc) return "Maintaining this chant turns your melee weapon into a Holy Avenger.";
        if (cast) _begin(T_BLESS_HOLY_AVENGER);
        break;
    /* [Book of Transcendence] */
    case 24:
        if (name) return "Peace";
        if (desc) return "Maintaining this chant convinces nearby monsters of your peaceful intentions.";
        if (cast) _begin(T_BLESS_PEACE);
        break;
    case 25:
        if (name) return "Growing";
        if (desc) return "Maintaining this chant brings life to the dead places of the world.";
        if (cast) _begin(T_BLESS_GROWING);
        break;
    case 26:
        if (name) return "Exorcism";
        if (desc) return "Maintaining this chant will cast undead and demons to the netherworld.";
        if (info) return dice_info_power(_exorcism_dice());
        if (cast) _begin(T_BLESS_EXORCISM);
        break;
    case 27:
        if (name) return "Holy Matins";
        if (desc) return "This spell immediately lights up the entire dungeon.";
        if (cast) wiz_lite();
        break;
    case 28:
        if (name) return "Repel Monsters";
        if (desc) return "Maintaining this chant protects you from the melee attacks of all monsters.";
        if (cast) _begin(T_BLESS_REPEL);
        break;
    case 29:
        if (name) return "Healing";
        if (desc) return "Maintaining this chant powerfully heals you every move.";
        if (info) return dice_info_heal(_heal_dice());
        if (cast) _begin(T_BLESS_HEAL);
        break;
    case 30:
        if (name) return "Justice";
        if (desc) return "Maintaining this chant damages all monsters that damage you.";
        if (cast) _begin(T_BLESS_JUSTICE);
        break;
    case 31:
        if (name) return "Repentance";
        if (desc) return "Maintaining this chant causes summoned monsters to regret service to evil.";
        if (cast) _begin(T_BLESS_REPENTANCE);
        break;
    }
    return "";
}

bool bless_stop_one(void) /* UI to stop a chosen spell at high enough level */
{
    int ct = bless_count(), i;
    doc_ptr doc;
    vec_ptr v;
    bool result = FALSE;

    if (!ct)
    {
        msg_print("You are not currently chanting any holy blessings.");
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

    doc_insert(doc, "<color:U>Stop which holy chant?</color>\n");
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
void bless_stop_spell(int cmd, var_ptr res)
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
        var_set_bool(res, bless_stop_one());
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _bless_armor_bonuses(obj_ptr obj)
{
    if (obj_is_armor(obj) && obj_is_blessed(obj))
    {
        int ac = 1;
        if (obj_is_shield(obj)) ac += 1;
        else if (obj_is_body_armor(obj)) ac += 4;
        plr->to_a += ac;
        plr->dis_to_a += ac;
    }
}
void bless_calc_bonuses(void)
{
    /* XXX note that the skillmaster does not use plr->realm1 ...
    if (plr->realm1 != REALM_BLESS) return; */
    if (bless_count())
        plr->skills.stl -= 1 + bless_count();
    equip_for_each(_bless_armor_bonuses);
}
void bless_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (bless_count())
        add_flag(flgs, OF_DEC_STEALTH);
}
void bless_calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    if (obj_is_blessed(obj))
    {
        info->to_h += 7;
        info->dis_to_h += 7;
    }
}
