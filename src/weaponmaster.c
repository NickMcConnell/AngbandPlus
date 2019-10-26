/****************************************************************
 * The Weaponmaster
 ****************************************************************/

#include "angband.h"

static bool _check_speciality_equip(void);
static bool _check_speciality_aux(object_type *o_ptr);
static bool _can_judge(obj_ptr obj);

/* Weaponmasters have toggle based abilities */
static void _toggle_on(int toggle)
{
    switch (toggle)
    {
    case TOGGLE_SHIELD_REVENGE:
        plr_tim_lock(T_REVENGE);
        break;
    }
}
static void _toggle_off(int toggle)
{
    switch (toggle)
    {
    case TOGGLE_SHIELD_REVENGE:
        plr_tim_unlock(T_REVENGE);
        break;
    }
}
static int _get_toggle(void)
{
    return p_ptr->magic_num1[0];
}

static int _set_toggle(s32b toggle)
{
    int result = p_ptr->magic_num1[0];

    if (toggle == result) return result;
    _toggle_off(result);
    p_ptr->magic_num1[0] = toggle;
    _toggle_on(toggle);

    p_ptr->redraw |= PR_STATUS;
    p_ptr->update |= PU_BONUS;
    handle_stuff();

    return result;
}

/***********************************************************************
 * Weaponmaster Melee: Heavily Customized!
 ***********************************************************************/
enum {
    _HIT_CUSTOM = PLR_HIT_CUSTOM,
    _HIT_CRUSADERS_STRIKE,
    _HIT_VICIOUS_STRIKE,
    _HIT_CUNNING_STRIKE,
    _HIT_SMITE_EVIL,
    _HIT_ELUSIVE_STRIKE,
    _PROXIMITY_ALERT,
    _AUTO_BLOW,
    _HIT_WHIRLWIND,
    _HIT_REACH,
    _HIT_PIERCING_STRIKE,
    _HIT_REAPING,
    _HIT_FLURRY,
};

static point_t _mon_pos; /* change target monster for each strike with TOGGLE_MANY_STRIKE */
static bool _do_msg;     /* tell the user whenever the target changes */
static obj_ptr _shield;  /* change a shield into a fake weapon; remember old object */
static int _get_next_dir(int dir)
{
    switch (dir)
    {
    case 1: return 4;
    case 4: return 7;
    case 7: return 8;
    case 8: return 9;
    case 9: return 6;
    case 6: return 3;
    case 3: return 2;
    case 2: return 1;
    }
    return 5;
}
static bool _check_plr_stop_code(plr_attack_ptr context)
{
    switch (context->stop)
    {
    case STOP_PLR_FEAR:
    case STOP_PLR_DEAD:
    case STOP_PLR_LEAVING:
    case STOP_PLR_MOVED:
    case STOP_PLR_PARALYZED:
    case STOP_PLR_CONFUSED:  return FALSE;
    }
    return TRUE;
}
static bool _check_many_strike(plr_attack_ptr context)
{
    if (weaponmaster_get_toggle() != TOGGLE_MANY_STRIKE) return FALSE;
    if (context->mode) return FALSE;
    if (!_check_plr_stop_code(context)) return FALSE;
    return TRUE;
}
static bool _check_whirlwind(plr_attack_ptr context)
{
    if (!p_ptr->whirlwind) return FALSE;
    if (weaponmaster_get_toggle() == TOGGLE_PIERCING_STRIKE) return FALSE;
    if (context->mode) return FALSE;
    if (!_check_plr_stop_code(context)) return FALSE;
    return one_in_(5);
}
static bool _check_piercing_strike(plr_attack_ptr context)
{
    if (weaponmaster_get_toggle() != TOGGLE_PIERCING_STRIKE) return FALSE;
    if (context->mode) return FALSE;
    if (!_check_plr_stop_code(context)) return FALSE;
    return TRUE;
}
static void _set_pos(plr_attack_ptr context, point_t pos) /* change the target monster in middle of battle! */
{
    mon_ptr mon = mon_at(pos);
    if (mon)
    {
        context->mon = mon;
        context->race = mon_race(mon);
        monster_desc(context->mon_full_name, mon, 0);
        monster_desc(context->mon_name, mon, MD_PRON_VISIBLE);
        monster_desc(context->mon_name_obj, mon, MD_PRON_VISIBLE | MD_OBJECTIVE);
        context->mon_pos = pos;
        context->fear = FALSE;
    }
}
static int _reap(mon_race_ptr r)
{
    if (!monster_living(r)) return 0;
    if (r->flags1 & RF1_UNIQUE) return 30;
    if (r->flags3 & RF3_GOOD) return 15;
    return 10;
}

/************** hooks **************/
static bool _attack_begin_weapon(plr_attack_ptr context)
{
    if (!context->obj) return TRUE;
    _mon_pos = context->mon_pos; /* in case I move it! */
    _do_msg = FALSE;
    _shield = NULL;
    if ((_get_toggle() == TOGGLE_MANY_STRIKE || _get_toggle() == TOGGLE_PIERCING_STRIKE) && !context->mode)
        context->flags |= PAC_ANIMATE;
    switch (context->mode)
    {
    case _HIT_CUNNING_STRIKE:
        context->skill += 60;
        break;
    case _HIT_SMITE_EVIL:
        if ((context->flags & PAC_DISPLAY) || (context->race->flags3 & RF3_EVIL))
            context->skill += 600;
        break;
    case _HIT_REAPING:
        context->skill -= 120;
        break;
    }
    switch (_get_toggle())
    {
    case TOGGLE_SHIELD_BASH:
        /* shieldmasters can fight with their shields as if they were weapons.
         * let's fake things up to make life easy for plr_attack.c */
        if (obj_is_shield(context->obj))
        {
            obj_ptr obj = obj_copy(context->obj);
            _shield = obj;
            obj->dd = 3;
            obj->ds = _shield->ac;
            obj->to_h = _shield->to_a + 2*_shield->to_h;
            obj->to_d = _shield->to_a + 2*_shield->to_d;
            obj->ac = 0;
            obj->to_a = 0;
            add_flag(obj->flags, OF_FAKE);
            add_flag(context->obj_flags, OF_STUN);
            context->obj = obj;
        }
        break;
    case TOGGLE_SHARD_BLADE:
        if (p_ptr->lev >= 45)
            add_flag(context->obj_flags, OF_VORPAL2);
        else
            add_flag(context->obj_flags, OF_VORPAL);
        break;
    case TOGGLE_CHAOS_BLADE:
        add_flag(context->obj_flags, OF_BRAND_CHAOS);
        break;
    case TOGGLE_DEATH_BLADE:
        add_flag(context->obj_flags, OF_BRAND_VAMP);
        break;
    case TOGGLE_DEMON_BLADE:
        add_flag(context->obj_flags, OF_SLAY_GOOD);
        if (p_ptr->lev >= 45)
            add_flag(context->obj_flags, OF_BRAND_PLASMA);
        break;
    case TOGGLE_PATTERN_BLADE:
        add_flag(context->obj_flags, OF_SLAY_EVIL);
        add_flag(context->obj_flags, OF_SLAY_DEMON);
        add_flag(context->obj_flags, OF_SLAY_UNDEAD);
        break;
    case TOGGLE_HOLY_BLADE:
        add_flag(context->obj_flags, OF_SLAY_EVIL);
        add_flag(context->obj_flags, OF_SLAY_UNDEAD);
        add_flag(context->obj_flags, OF_SLAY_DEMON);
        if (p_ptr->lev >= 50)
            add_flag(context->obj_flags, OF_BRAND_LITE);
        break;
    }
    if (weaponmaster_is_(WEAPONMASTER_CLUBS) && p_ptr->speciality_equip)
    {
        if (context->mode == _HIT_CUNNING_STRIKE || p_ptr->lev >= 45)
            add_flag(context->obj_flags, OF_STUN);
    }
    return TRUE;
}
static bool _attack_check_hit(plr_attack_ptr context)
{
    if (_do_msg) /* message works better here in case we miss */
    {
        if (!(context->flags & PAC_NO_START_MSG))
        {
            cptr desc = context->attack_desc;
            if (!desc) desc = "attack";
            cmsg_format(TERM_L_UMBER, "You %s %s:", desc, context->mon_full_name);
        }
        _do_msg = FALSE;
    }
    if (_get_toggle() == TOGGLE_BURNING_BLADE) return TRUE;
    return plr_check_hit(context);
}
static void _attack_mod_blows(plr_attack_ptr context)
{
    if (!context->obj) return;
    switch (context->mode)
    {
    case _HIT_CRUSADERS_STRIKE:
    case _HIT_REAPING:
        context->blow_ct = 1;
        break;
    case _HIT_CUNNING_STRIKE:
        context->blow_ct = (context->blow_ct + 1)/2;
        break;
    case _PROXIMITY_ALERT:
        context->blow_ct = 1 + p_ptr->lev/25;
        break;
    case _HIT_FLURRY:
        context->blow_ct *= 2;
        break;
    }
}
static void _attack_before_hit(plr_attack_ptr context)
{
}
static void _attack_mod_damage(plr_attack_ptr context)
{
    if (!context->obj) return;
    switch (context->mode)
    {
    case _HIT_CRUSADERS_STRIKE:
        context->dam = context->dam*3/2;
        break;
    case _HIT_VICIOUS_STRIKE:
        context->dam *= 2;
        break;
    case _HIT_REAPING:
        context->dam += context->dam * NUM_BLOWS(context->info.which)/300;
        break;
    }
    switch (_get_toggle())
    {
    case TOGGLE_BURNING_BLADE:
        msg_format("%^s is <color:r>burned</color>!", context->mon_name);
        if (context->race->flagsr & RFR_RES_ALL)
        {
            msg_format("%^s is immune.", context->mon_name);
            context->dam = 0;
            mon_lore_r(context->mon, RFR_RES_ALL);
        }
        else if (context->race->flagsr & RFR_IM_FIRE)
        {
            msg_format("%^s is immune.", context->mon_name);
            context->dam = 0;
            mon_lore_r(context->mon, RFR_IM_FIRE);
        }
        else if (context->race->flagsr & RFR_RES_FIRE)
        {
            msg_format("%^s resists.", context->mon_name);
            context->dam /= 3;
            mon_lore_r(context->mon, RFR_RES_FIRE);
        }
        else if (context->race->flags3 & (RF3_HURT_FIRE))
        {
            msg_format("%^s is hit hard.", context->mon_name);
            context->dam *= 2;
            mon_lore_3(context->mon, RF3_HURT_FIRE);
        }
        break;
    case TOGGLE_ICE_BLADE:
        msg_format("%^s is <color:W>frozen</color>!", context->mon_name);
        if (context->race->flagsr & RFR_RES_ALL)
        {
            msg_format("%^s is immune.", context->mon_name);
            context->dam = 0;
            mon_lore_r(context->mon, RFR_RES_ALL);
        }
        else if (context->race->flagsr & RFR_IM_COLD)
        {
            msg_format("%^s is immune.", context->mon_name);
            context->dam = 0;
            mon_lore_r(context->mon, RFR_IM_COLD);
        }
        else if (context->race->flagsr & RFR_RES_COLD)
        {
            msg_format("%^s resists.", context->mon_name);
            context->dam /= 3;
            mon_lore_r(context->mon, RFR_RES_COLD);
        }
        else
        {
            if (context->race->flags3 & (RF3_HURT_COLD))
            {
                msg_format("%^s is hit hard.", context->mon_name);
                context->dam *= 2;
                mon_lore_3(context->mon, RF3_HURT_COLD);
            }
            if (one_in_(5)
                && !(context->race->flags1 & RF1_UNIQUE)
                && !mon_save_p(context->race->id, A_STR) )
            {
                msg_format("%^s is slowed by the cold.", context->mon_name);
                mon_tim_add(context->mon, T_SLOW, 50);
            }
        }
        break;
    case TOGGLE_THUNDER_BLADE:
        msg_format("%^s is <color:b>shocked</color>!", context->mon_name);
        if (context->race->flagsr & RFR_RES_ALL)
        {
            msg_format("%^s is immune.", context->mon_name);
            context->dam = 0;
            mon_lore_r(context->mon, RFR_RES_ALL);
        }
        else if (context->race->flagsr & RFR_IM_ELEC)
        {
            msg_format("%^s is immune.", context->mon_name);
            context->dam = 0;
            mon_lore_r(context->mon, RFR_IM_ELEC);
        }
        else if (context->race->flagsr & RFR_RES_ELEC)
        {
            msg_format("%^s resists.", context->mon_name);
            context->dam /= 3;
            mon_lore_r(context->mon, RFR_RES_ELEC);
        }
        else
        {
            if ( one_in_(5)
              && !(context->race->flags3 & RF3_NO_STUN)
              && !mon_save_stun(context->race->level, context->dam) )
            {
                msg_format("%^s is shocked convulsively.", context->mon_name);
                mon_stun(context->mon, mon_stun_amount(context->dam));
            }
        }
        break;
    }
    context->dam_drain = context->dam;
}
static void _attack_after_hit(plr_attack_ptr context)
{
    if (!context->obj) return;
    /* Special Blows */
    if (context->mode == _HIT_CRUSADERS_STRIKE)
    {
        msg_format("Your Crusader's Strike drains life from %s!", context->mon_name_obj);
        hp_player(MIN(150, context->dam));
    }
    else if (context->mode == _HIT_REAPING)
    {
        int ct = 0, dir, start_dir;
        point_t pt;
        mon_ptr mon;
        bool fear = FALSE;

        if (context->stop == STOP_MON_DEAD)
            ct = 2*_reap(context->race);
        msg_format("Your swing your %s about, reaping a harvest of death!", context->obj_name);

        /* Next hit all adjacent targets in a swinging circular arc */
        start_dir = calculate_dir(p_ptr->pos.x, p_ptr->pos.y, context->mon_pos.x, context->mon_pos.y);
        dir = start_dir;

        for (;;)
        {
            dir = _get_next_dir(dir);
            if (dir == start_dir || dir == 5) break;

            pt = point_step(p_ptr->pos, dir);
            mon = mon_at(pt);
            if (mon && (mon->ml || cave_have_flag_at(pt, FF_PROJECT)))
            {
                if (mon_take_hit(mon->id, context->dam, &fear, NULL))
                    ct += _reap(mon_race(mon));
            }
        }

        /* Finally, gain Wraithform */
        if (ct) plr_tim_add(T_WRAITH, plr_tim_amount(T_WRAITH) + ct/2); /* pretend TF_AUGMENT */
        else msg_print("You make a poor harvest!");
    }
    /* Clubmaster even enhances normal melee (context->mode == 0) */
    if (weaponmaster_is_(WEAPONMASTER_CLUBS) && p_ptr->speciality_equip && !context->stop)
    {
        int odds = 5;

        if (context->mode == _HIT_CUNNING_STRIKE)
            odds = 2;

        if (one_in_(odds))
        {
            if (context->race->flagsr & RFR_RES_ALL)
            {
                mon_lore_r(context->mon, RFR_RES_ALL);
                if (disturb_minor) msg_format("%^s is immune.", context->mon_name);
            }
            else if (context->race->flags3 & RF3_NO_CONF)
            {
                mon_lore_3(context->mon, RF3_NO_CONF);
                if (disturb_minor) msg_format("%^s is immune.", context->mon_name);
            }
            else if (mon_save_p(context->mon->r_idx, A_STR))
            {
                if (disturb_minor) msg_format("%^s resists.", context->mon_name);
            }
            else
                mon_tim_add(context->mon, T_CONFUSED, 10 + randint0(p_ptr->lev)/5);
        }

        if (p_ptr->lev >= 20 && !mon_tim_find(context->mon, T_PARALYZED) && one_in_(odds))
        {
            if (context->race->flagsr & RFR_RES_ALL)
            {
                mon_lore_r(context->mon, RFR_RES_ALL);
                if (disturb_minor) msg_format("%^s is immune.", context->mon_name);
            }
            else if (context->race->flags3 & RF3_NO_SLEEP)
            {
                mon_lore_3(context->mon, RF3_NO_SLEEP);
                if (disturb_minor) msg_format("%^s is immune.", context->mon_name);
            }
            else if (mon_save_p(context->mon->r_idx, A_STR))
            {
                if (disturb_minor) msg_format("%^s resists.", context->mon_name);
            }
            else
            {
                mon_tim_add(context->mon, T_PARALYZED, randint1(3));
            }
        }
    }
    /* polearm-master can scatter strikes among adjacent foes during battle */
    if (_check_many_strike(context))
    {
        point_t loc = context->mon_pos;
        if (random_opponent(&loc.y, &loc.x) && point_compare(loc, context->mon_pos) != 0)
        {
            context->stop = 0; /* could have been STOP_MON_DEAD or STOP_MON_MOVED */
            _set_pos(context, loc);
            _do_msg = TRUE; /* later ... we might be out of strikes with this weapon */
        }
    }
    /* polearm-master can turn a successful hit in the middle of battle into
     * a single-strike whirlwind that also tries to hit every other adjacent monster
     * (once). */
    if (_check_whirlwind(context))
    {
        int        start_dir, dir, ct = 0;
        mon_ptr    mon;
        point_t    pt;

        start_dir = point_step_dir(p_ptr->pos, context->mon_pos);
        dir = start_dir;

        for (;;)
        {
            dir = _get_next_dir(dir);
            if (dir == start_dir || dir == 5) break;

            pt = point_step(p_ptr->pos, dir);
            mon = mon_at(pt);
            if (mon && (mon->ml || cave_have_flag_at(pt, FF_PROJECT)))
            {
                plr_attack_t ctx = {0};
                ctx.mode = _HIT_WHIRLWIND;
                ctx.flags = PAC_ANIMATE;
                if (!ct++)
                    cmsg_format(TERM_L_BLUE, "(You swing your %s in a wide arc:", context->obj_name);
                plr_attack_begin(&ctx, pt);
                plr_attack_start_msg(&ctx);
                plr_attack_init_hand(&ctx, context->info.which);
                plr_hit_mon(&ctx);
                plr_attack_end(&ctx);
            }
        }
        if (ct) cmsg_print(TERM_L_BLUE, ")");
    }
    /* polearm-master can poke his spear thru one monster to hit distant monsters in line */
    if (_check_piercing_strike(context))
    {
        point_t path[10];
        int     ct_path = project_path(path, 3, p_ptr->pos, context->mon_pos, /*PROJECT_PATH |*/ PROJECT_THRU);
        int     i, ct = 0;

        for (i = 0; i < ct_path; i++)
        {
            point_t pt = path[i];
            mon_ptr mon = mon_at(pt);
            cave_ptr grid = cave_at(pt);

            if (point_equals(pt, context->mon_pos)) continue;

            if (!cave_have_flag_grid(grid, FF_PROJECT) && !mon) break;
            if (mon) /* XXX allow continue thru empty grids? */
            {
                plr_attack_t ctx = {0};
                bool hit = FALSE;
                ctx.mode = _HIT_PIERCING_STRIKE;
                ctx.flags = PAC_ANIMATE;
                if (!ct++)
                    cmsg_format(TERM_L_BLUE, "(You pierce %s:", context->mon_name);
                plr_attack_begin(&ctx, pt);
                plr_attack_start_msg(&ctx);
                plr_attack_init_hand(&ctx, context->info.which);
                hit = plr_hit_mon(&ctx);
                plr_attack_end(&ctx);
                if (!hit) break; /* need to hit to continue */
                if (!_check_plr_stop_code(&ctx)) break; /* auras could have killed/confused/moved us */
            }
        }
        if (ct) cmsg_print(TERM_L_BLUE, ")");
    }
    /* Staffmaster gains elaborate defense */
    if (weaponmaster_is_(WEAPONMASTER_STAVES) && p_ptr->speciality_equip)
    {
        bool update = FALSE;
        if (p_ptr->elaborate_defense == 0) update = TRUE;
        p_ptr->elaborate_defense = 1;
        if (update)
        {
            p_ptr->update |= PU_BONUS;
            p_ptr->redraw |= PR_ARMOR;
        }
    }
}
static void _attack_end_weapon(plr_attack_ptr context)
{
    if (context->flags & PAC_DISPLAY) return;
    if (!context->obj) return;
    if (_get_toggle() == TOGGLE_MANY_STRIKE && !context->mode)
        context->flags &= ~PAC_ANIMATE;
    if (point_compare(context->mon_pos, _mon_pos) != 0)
    {
        _set_pos(context, _mon_pos);
        _do_msg = FALSE;
    }
    if (_get_toggle() == TOGGLE_SHIELD_BASH && _shield)
    {
        obj_free(context->obj);
        context->obj = _shield;
        _shield = NULL;
    }
}
static void _attack_end(plr_attack_ptr context)
{
    if (context->flags & PAC_DISPLAY) return;
    if (p_ptr->cleave && context->stop == STOP_MON_DEAD && !context->mode)
    {
        int i;
        for (i = 1; i <= 4 + p_ptr->lev/10; i++)
        {
            point_t p = point_step(p_ptr->pos, ddd[randint0(8)]);
            if (mon_at(p))
            {
                msg_print("You attempt to cleave another foe!");
                plr_attack_normal(p); /* recursive attacks! */
                break;
            }
        }
    }
    if (context->mode == _HIT_ELUSIVE_STRIKE && context->hits)
        teleport_player(10, TELEPORT_LINE_OF_SIGHT);
    if (weaponmaster_get_toggle() == TOGGLE_TRIP && !context->mode && !context->stop && !(context->mon->mflag2 & MFLAG2_TRIPPED))
    {
        if (test_hit_norm(context->skill, mon_ac(context->mon), context->mon->ml))
        {
            if ( !(context->race->flags1 & RF1_UNIQUE)
              || !mon_save_p(context->mon->r_idx, A_STR) )
            {
                msg_format("<color:B>You trip %s!</color>", context->mon_name_obj);
                context->mon->mflag2 |= MFLAG2_TRIPPED;
            }
            else
                msg_format("%^s nimbly dodges your attempt to trip.", context->mon_name);
        }
        else
            msg_format("You attempt to trip %s but miss.", context->mon_name_obj);
    }
}
static void _attack_init(plr_attack_ptr context)
{
    context->hooks.begin_weapon_f = _attack_begin_weapon;
    context->hooks.check_hit_f = _attack_check_hit;
    context->hooks.mod_blows_f = _attack_mod_blows;
    context->hooks.before_hit_f = _attack_before_hit;
    context->hooks.mod_damage_f = _attack_mod_damage;
    context->hooks.after_hit_f = _attack_after_hit;
    context->hooks.end_weapon_f = _attack_end_weapon;
    context->hooks.end_f = _attack_end;
}
static bool _do_blow_aux(int type, int flags, int range)
{
    if (!_check_speciality_equip())
    {
        msg_print("Failed! You do not feel comfortable with your weapon.");
        return FALSE;
    }
    return plr_attack_ranged(type, flags, range);
}
static bool _do_blow(int type)
{
    return _do_blow_aux(type, PAC_NO_INNATE, 1);
}
static bool _browse_blow(int type)
{
    if (!_check_speciality_equip())
        return FALSE;
    plr_attack_display_special(type, 0);
    return TRUE;
}

/***********************************************************************
 * Weaponmaster Shooting: Heavily Customized!
 ***********************************************************************/
int       shoot_hack = SHOOT_NONE;
int       shoot_count = 0;
obj_loc_t shoot_item = {0};

#define _MAX_TARGETS 100

static bool _check_direct_shot(mon_ptr mon)
{
    bool result = FALSE;
    point_t path[512];
    point_t tgt = mon->pos;
    int ct = project_path(path, 50, p_ptr->pos, tgt, 0); /* We don't know the length ... just project from source to target, please! */
    int i;

    for (i = 0; i < ct; i++)
    {
        point_t p = path[i];

        if (point_equals(p, tgt))
        {
            result = TRUE;
            break;
        }

        /* Stopped by walls/doors (unless passwall monster) */
        if (!cave_have_flag_at(p, FF_PROJECT) && !mon_at(p)) break;

        /* Monster in the way of target */
        if (mon_at(p)) break;
    }

    return result;
}

static mon_ptr _get_nearest_target_los(void)
{
    mon_ptr result = NULL;
    int dis = AAF_LIMIT + 1;
    int rng = 0;
    int_map_iter_ptr iter;

    if (p_ptr->shooter_info.slot)
        rng = bow_range(equip_obj(p_ptr->shooter_info.slot));

    for (iter = int_map_iter_alloc(cave->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr mon = int_map_iter_current(iter);
        if (mon_is_friendly(mon)) continue;
        if (mon_is_pet(mon)) continue;
        if (mon->cdis > rng) continue;
        if (!mon->ml) continue;
        if (!plr_los(mon->pos)) continue;

        if (mon->cdis < dis)
        {
            result = mon;
            dis = mon->cdis;
        }
    }
    int_map_iter_free(iter);
    return result;
}

static vec_ptr _get_los_targets(mon_p filter)
{
    vec_ptr targets = vec_alloc(NULL);
    int_map_iter_ptr iter;
    int rng = 0;

    if (p_ptr->shooter_info.slot)
        rng = bow_range(equip_obj(p_ptr->shooter_info.slot));

    for (iter = int_map_iter_alloc(cave->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr mon = int_map_iter_current(iter);
        if (mon_is_friendly(mon)) continue;
        if (mon_is_pet(mon)) continue;
        if (mon->cdis > rng) continue;
        if (!mon->ml) continue;
        if (!plr_los(mon->pos)) continue;
        if (filter && !filter(mon)) continue;

        vec_add(targets, mon);
    }
    int_map_iter_free(iter);
    return targets;
}
static vec_ptr _get_greater_many_shot_targets(void) { return _get_los_targets(NULL); }
static vec_ptr _get_many_shot_targets(void) { return _get_los_targets(_check_direct_shot); }

static bool _fire(int power)
{
    bool result = FALSE;
    shoot_hack = power;
    shoot_count = 0;
    command_cmd = 'f'; /* Hack for inscriptions (e.g. '@f1') */
    result = do_cmd_fire();
    shoot_hack = SHOOT_NONE;
    shoot_count = 0;

    return result;
}

static obj_ptr _get_ammo(bool allow_floor)
{
    obj_prompt_t prompt = {0};

    if (allow_floor)
        prompt.prompt = "Fire which ammo?";
    else
        prompt.prompt = "Choose ammo to use for this technique.";
    prompt.error = "You have nothing to fire.";
    prompt.filter = obj_can_shoot;
    prompt.where[0] = INV_QUIVER;
    prompt.where[1] = INV_PACK;
    if (allow_floor)
        prompt.where[2] = INV_FLOOR;

    obj_prompt(&prompt);
    return prompt.obj;
}

/****************************************************************
 * Timers
 ****************************************************************/
enum { _VICIOUS_STRIKE = T_CUSTOM };
static bool _vicious_strike_on(plr_tim_ptr timer)
{
    msg_print("You feel greatly exposed by your last attack.");
    p_ptr->update |= PU_BONUS;
    return TRUE;
}
static void _vicious_strike_off(plr_tim_ptr timer)
{
    msg_print("You no longer feel greatly exposed.");
    p_ptr->update |= PU_BONUS;
}
static void _vicious_strike_bonus(plr_tim_ptr timer)
{
    p_ptr->to_a -= 120;
    p_ptr->dis_to_a -= 120;
}
static status_display_t _vicios_strike_display(plr_tim_ptr timer)
{
    return status_display_create("Exposed", "Vs", TERM_RED);
}
static plr_tim_info_ptr _vicious_strike(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_VICIOUS_STRIKE, "Vicious Strike");
    info->desc = "You are greatly exposed to enemy attacks.";
    info->on_f = _vicious_strike_on;
    info->off_f = _vicious_strike_off;
    info->calc_bonuses_f = _vicious_strike_bonus;
    info->status_display_f = _vicios_strike_display;
    info->flags = TF_FAST_TICK | TF_AUGMENT | TF_NO_DISPEL;
    return info;
}
static void _register_timers(void)
{
    plr_tim_register(_vicious_strike());
}
/****************************************************************
 * Private Spells
 ****************************************************************/
static void _fire_spell(int which, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your shooter.");
            return;
        }
        if (_fire(which))
            var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        var_set_int(res, energy_use);    /* already set correctly by do_cmd_fire() */
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _toggle_spell(int which, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your weapon.");
            return;
        }
        if (_get_toggle() == which)
            _set_toggle(TOGGLE_NONE);
        else
            _set_toggle(which);
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        if (_get_toggle() != which)
            var_set_int(res, 0);    /* no charge for dismissing a technique */
        else
            var_set_int(res, 100);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _judge_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Judge");
        break;
    case SPELL_DESC:
        var_set_string(res, "Identifies a favored item.");
        break;
    case SPELL_CAST:
        if (p_ptr->lev >= 45)
            var_set_bool(res, identify_fully(_can_judge));
        else
            var_set_bool(res, ident_spell(_can_judge));
        break;
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

/****************************************************************
 * Axemaster
 ****************************************************************/
static void _crusaders_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Crusaders Strike");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent opponent with a single blow. You regain hp.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _do_blow(_HIT_CRUSADERS_STRIKE));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _power_attack_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Power Attack");
        break;
    case SPELL_DESC:
        var_set_string(res, "You lose accuracy but gain damage.");
        break;
    default:
        _toggle_spell(TOGGLE_POWER_ATTACK, cmd, res);
        break;
    }
}

static void _vicious_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Vicious Strike");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attacks an opponent very powerfully, but you become greatly exposed by the effort.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (_do_blow(_HIT_VICIOUS_STRIKE))
        {
            plr_tim_add(_VICIOUS_STRIKE, 5 + randint1(5));
            var_set_bool(res, TRUE);
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

/****************************************************************
 * Bowmaster
 ****************************************************************/
static void _arrow_of_slaying_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Arrow of Slaying");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to kill a monster a well aimed shot.");
        break;
    default:
        _fire_spell(SHOOT_NEEDLE, cmd, res);
    }
}

static void _disintegration_arrow_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Disintegration Arrow");
        break;
    case SPELL_DESC:
        var_set_string(res, "Shoot a single arrow at a chosen opponent. Not even intervening walls can stop this shot!");
        break;
    default:
        _fire_spell(SHOOT_DISINTEGRATE, cmd, res);
    }
}

static void _piercing_arrow_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Piercing Arrow");
        break;
    case SPELL_DESC:
        var_set_string(res, "If an arrow hits opponent, it pierces and can also hit next opponent in same direction (requires another attack roll), up to 5 opponents. Each successive pierce suffers a cumulative penalty to hit.");
        break;
    default:
        _toggle_spell(TOGGLE_PIERCING_ARROW, cmd, res);
        break;
    }
}

static void _readied_shot_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Readied Shot");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fire an arrow (choose which at time of casting) in response at enemy who damages you. Active until triggered.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your shooter.");
            return;
        }
        if (_get_toggle() == TOGGLE_READIED_SHOT)
            _set_toggle(TOGGLE_NONE);
        else
        {
            obj_ptr ammo = _get_ammo(FALSE);
            if (!ammo)
            {
                flush();
                return;
            }
            shoot_item = ammo->loc;
            _set_toggle(TOGGLE_READIED_SHOT);
        }
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        if (_get_toggle() != TOGGLE_READIED_SHOT)
            var_set_int(res, 0);    /* no charge for dismissing a technique */
        else
            var_set_int(res, 100);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _tranquilizing_arrow_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Tranquilizing Arrow");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to put a monster to sleep with a well aimed shot.");
        break;
    default:
        _fire_spell(SHOOT_TRANQUILIZE, cmd, res);
    }
}

static void _volley_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Volley");
        break;
    case SPELL_DESC:
        var_set_string(res, "Launch a single arrow at chosen foe. This arrow will be shot over the heads of any intervening monsters.");
        break;
    default:
        _fire_spell(SHOOT_VOLLEY, cmd, res);
    }
}

/****************************************************************
 * Clubmaster
 ****************************************************************/
static void _combat_expertise_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Defensive Stance");
        break;
    case SPELL_DESC:
        var_set_string(res, "You lose accuracy but gain armor class.");
        break;
    default:
        _toggle_spell(TOGGLE_COMBAT_EXPERTISE, cmd, res);
        break;
    }
}

static void _cunning_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Cunning Strike");
        break;
    case SPELL_DESC:
        var_set_string(res, "Number of attacks are cut in half, but blows are more likely to confuse, stun, knock out your opponent.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _do_blow(_HIT_CUNNING_STRIKE));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _smite_evil_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Smite Evil");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack powerfully at an evil monster. You are less likely to miss and more likely to score critical hits.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _do_blow(_HIT_SMITE_EVIL));
        break;
    case SPELL_ON_BROWSE:
        if (_browse_blow(_HIT_SMITE_EVIL))
        {
            var_set_bool(res, TRUE);
            break;
        }
    default:
        default_spell(cmd, res);
        break;
    }
}

static int _smash_ground_dam(void)
{
    int result = 0;
    int hand;
    /* Modified from Samurai "Crack" */
    for (hand = 0; hand < MAX_HANDS; hand++)
    {
        if (p_ptr->attack_info[hand].type == PAT_WEAPON)
        {
            object_type *o_ptr = equip_obj(p_ptr->attack_info[hand].slot);
            int          damage, dd, ds;

            if (!o_ptr) continue;

            dd = o_ptr->dd + p_ptr->attack_info[hand].to_dd;
            ds = o_ptr->ds + p_ptr->attack_info[hand].to_ds;

            damage = dd * (ds + 1) * 50;
            damage += o_ptr->to_d * 100;
            damage *= NUM_BLOWS(hand)/100;
            result += damage / 100;
        }
    }
    return result;
}
static void _smash_ground_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Smash Ground");
        break;
    case SPELL_DESC:
        var_set_string(res, "Produces a loud, stunning noise.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _smash_ground_dam()));
        break;
    case SPELL_CAST:
    {
        int dam = _smash_ground_dam();
        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your weapon.");
            return;
        }
        msg_print("You smash your weapon mightily on the ground.");
        project(0, 8, p_ptr->pos.y, p_ptr->pos.x, device_power(dam*2), GF_SOUND, PROJECT_KILL | PROJECT_ITEM);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _toss_hit_mon(plr_throw_ptr context, int m_idx)
{
    monster_type *m_ptr = dun_mon(cave, m_idx);
    monster_race *r_ptr = mon_race(m_ptr);
    char          m_name[80];
    int           odds = 2;

    monster_desc(m_name, m_ptr, 0);
    if (one_in_(odds))
    {
        if (r_ptr->flags3 & RF3_NO_CONF)
        {
            mon_lore_3(m_ptr, RF3_NO_CONF);
            msg_format("%^s is unaffected.", m_name);
        }
        else if (mon_save_p(m_ptr->r_idx, A_STR))
        {
            msg_format("%^s is unaffected.", m_name);
        }
        else
            mon_tim_add(m_ptr, T_CONFUSED, 10 + randint0(p_ptr->lev)/5);
    }

    if (p_ptr->lev >= 20 && one_in_(odds))
    {
        if (r_ptr->flags3 & RF3_NO_SLEEP)
        {
            mon_lore_3(m_ptr, RF3_NO_SLEEP);
            msg_format("%^s is unaffected.", m_name);
        }
        else if (mon_save_p(m_ptr->r_idx, A_STR))
        {
            msg_format("%^s is unaffected.", m_name);
        }
        else
        {
            msg_format("%^s is <color:b>knocked out</color>.", m_name);
            mon_tim_add(m_ptr, MT_SLEEP, 500);
        }
    }

    if (p_ptr->lev >= 45 && one_in_(odds))
    {
        if (r_ptr->flags3 & RF3_NO_STUN)
        {
            mon_lore_3(m_ptr, RF3_NO_STUN);
            msg_format("%^s is unaffected.", m_name);
        }
        else if (mon_save_stun(r_ptr->level, context->dam))
        {
            msg_format("%^s is unaffected.", m_name);
        }
        else
        {
            msg_format("%^s is <color:B>stunned</color>.", m_name);
            mon_stun(m_ptr, mon_stun_amount(context->dam));
        }
    }
}
static void _init_throw_context(plr_throw_ptr context)
{
    context->type = THROW_BOOMERANG;
    context->mult = 100 + 4 * p_ptr->lev;
    context->back_chance = 24 + randint1(5);
    context->after_hit_f = _toss_hit_mon;
}
static void _throw_weapon_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Throw Weapon");
        break;
    case SPELL_DESC:
        var_set_string(res, "Throws your weapon which might return to you.");
        break;
    case SPELL_CAST:
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your weapon.");
            return;
        }
        else
        {
            plr_throw_t context = {0};
            _init_throw_context(&context);
            var_set_bool(res, plr_throw(&context));
        }
        break;
    case SPELL_ON_BROWSE:
    {
        bool        screen_hack = screen_is_saved();
        plr_throw_t context = {0};
        doc_ptr     doc = doc_alloc(80);

        _init_throw_context(&context);
        context.type |= THROW_DISPLAY;
        plr_throw_doc(&context, doc);

        if (screen_hack) screen_load();
        screen_save();
        doc_display(doc, "Throw Weapon", 0);
        screen_load();
        if (screen_hack) screen_save();

        doc_free(doc);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _trade_blows_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Trade Blows");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique, you are somewhat exposed. However, you will retaliate whenever a monster hits you.");
        break;
    default:
        _toggle_spell(TOGGLE_TRADE_BLOWS, cmd, res);
        break;
    }
}

/****************************************************************
 * Crossbowmaster
 ****************************************************************/
static void _careful_aim_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Careful Aim");
        break;
    case SPELL_DESC:
        var_set_string(res, "You shoot more slowly but much more accurately.");
        break;
    default:
        _toggle_spell(TOGGLE_CAREFUL_AIM, cmd, res);
        break;
    }
}

static void _elemental_bolt_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Elemental Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, "Shoot a single bolt which explodes in a shower of the elements if it hits.");
        break;
    default:
        _fire_spell(SHOOT_ELEMENTAL, cmd, res);
    }
}

static void _exploding_bolt_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Exploding Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, "Your bolts will explode on impact, but your firing rate is decreased.");
        break;
    default:
        _toggle_spell(TOGGLE_EXPLODING_BOLT, cmd, res);
        break;
    }
}

static void _knockback_bolt_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Knockback");
        break;
    case SPELL_DESC:
        var_set_string(res, "Shoot a single bolt which propels your enemy backwards if it hits.");
        break;
    default:
        _fire_spell(SHOOT_KNOCKBACK, cmd, res);
    }
}

static void _overdraw_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Overdraw");
        break;
    case SPELL_DESC:
        var_set_string(res, "You shoot with extra might, but your accuracy is diminished.");
        break;
    default:
        _toggle_spell(TOGGLE_OVERDRAW, cmd, res);
        break;
    }
}

static void _rapid_reload_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Rapid Reload");
        break;
    case SPELL_DESC:
        var_set_string(res, "Your shots per round are increased, but you lose armor class and accuracy in your haste.");
        break;
    default:
        _toggle_spell(TOGGLE_RAPID_RELOAD, cmd, res);
        break;
    }
}

static void _shattering_bolt_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shattering Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, "Shoot a single bolt which fragments on impact for extra damage.");
        break;
    default:
        _fire_spell(SHOOT_SHATTER, cmd, res);
    }
}

/****************************************************************
 * Daggermaster
 ****************************************************************/
static void _dagger_toss_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dagger Toss");
        break;
    case SPELL_DESC:
        var_set_string(res, "Throws your weapon at target monster.");
        break;
    case SPELL_CAST:
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your weapon.");
            return;
        }
        else
        {
            plr_throw_t context = {0};

            context.type = THROW_BOOMERANG;
            context.mult = 100 + 4 * p_ptr->lev;
            context.back_chance = 20;
            if (_get_toggle() == TOGGLE_FLYING_DAGGER_STANCE)
            {
                context.back_chance += 4 + randint1(5);
                context.to_dd += p_ptr->lev/15;
            }
            var_set_bool(res, plr_throw(&context));
        }
        break;
    case SPELL_ENERGY:
        if (_get_toggle() == TOGGLE_FLYING_DAGGER_STANCE)
            var_set_int(res, (100 - p_ptr->lev)*2/3);
        else
            var_set_int(res, 100 - p_ptr->lev);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _flying_dagger_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Flying Dagger Stance");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique, you gain great prowess with the Dagger Toss. Thrown weapons return more often and damage is greatly increased. However, this stance leaves you somewhat exposed to your enemies.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your weapon.");
            return;
        }
        if (_get_toggle() == TOGGLE_FLYING_DAGGER_STANCE)
            _set_toggle(TOGGLE_NONE);
        else
            _set_toggle(TOGGLE_FLYING_DAGGER_STANCE);
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        if (_get_toggle() != TOGGLE_FLYING_DAGGER_STANCE)
            var_set_int(res, 0);    /* no charge for dismissing a technique */
        else
            var_set_int(res, 100);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _elusive_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Elusive Strike");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent monster and blink to a new location in the line of sight of your current location.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _do_blow(_HIT_ELUSIVE_STRIKE));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _frenzy_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Frenzy Stance");
        break;
    case SPELL_DESC:
        var_set_string(res, "In this posture, you attack foes with great power in melee. However, your rage exposes you to your enemies!");
        break;
    default:
        _toggle_spell(TOGGLE_FRENZY_STANCE, cmd, res);
        break;
    }
}

static void _shadow_stance_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shadow Stance");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique, you walk quickly and stealthily. On attacking a foe, you will swap positions.");
        break;
    default:
        _toggle_spell(TOGGLE_SHADOW_STANCE, cmd, res);
        break;
    }
}

/****************************************************************
 * Diggermaster
 ****************************************************************/
static void _tunnel_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Tunnel");
        break;
    case SPELL_DESC:
        var_set_string(res, "Creates a tunnel down to the next level.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your weapon.");
            return;
        }
        if (!cave_valid_bold(p_ptr->pos.y, p_ptr->pos.x))
        {
            msg_print("You need room to dig!");
        }
        else
        {
            msg_print("You tunnel downwards ...");
            dun_create_stairs(cave, TRUE);
            var_set_bool(res, TRUE);
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _calamity_of_the_living_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Calamity of the Living");
        break;
    case SPELL_DESC:
        var_set_string(res, "Causes an earthquake or destruction.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your weapon.");
            return;
        }
        if (p_ptr->lev >= 50 || one_in_(3))
        {
            destroy_area(p_ptr->pos.y, p_ptr->pos.x, 12 + randint1(4), 4 * p_ptr->lev);
        }
        else
        {
            msg_print("The ground rumbles!");
            earthquake(p_ptr->pos, 10);
        }
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static bool _object_is_corpse_or_skeleton(object_type *o_ptr)
{
    if (o_ptr->tval == TV_CORPSE) return TRUE;
    if (o_ptr->tval == TV_SKELETON) return TRUE;
    return FALSE;
}

static void _bury_dead_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Bury Dead");
        break;
    case SPELL_DESC:
        var_set_string(res, "You gain temporary enchantments by burying a corpse or skeleton.");
        break;
    case SPELL_CAST:
    {
        obj_prompt_t prompt = {0};
        char o_name[MAX_NLEN];
        int turns;

        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your weapon.");
            return;
        }

        prompt.prompt = "Bury which corpse?";
        prompt.error = "You have nothing to bury.";
        prompt.filter = _object_is_corpse_or_skeleton;
        prompt.where[0] = INV_PACK;
        prompt.where[1] = INV_FLOOR;

        obj_prompt(&prompt);
        if (!prompt.obj) return;

        /* TV_CORPSE, SV_CORPSE = Corpse
           TV_CORPSE, SV_SKELETON = Skeleton
           TV_SKELETON, ??? = Skeleton */
        if (prompt.obj->tval == TV_CORPSE && prompt.obj->sval == SV_CORPSE)
            turns = 40;
        else
            turns = 15;

        object_desc(o_name, prompt.obj, OD_NAME_ONLY | OD_COLOR_CODED | OD_SINGULAR);
        msg_format("You dig a hasty grave and toss in %s.", o_name);

        prompt.obj->number--;
        obj_release(prompt.obj, 0);

        plr_tim_add(T_BLESSED, turns);
        if (p_ptr->lev >= 15)
            plr_tim_add(T_HERO, turns);
        if (p_ptr->lev >= 30)
            plr_tim_add(T_FAST, turns);
        if (p_ptr->lev >= 40)
            plr_tim_add(T_RES_MAGIC, turns);

        if (p_ptr->lev >= 15)
            plr_tim_add(T_RES_COLD, turns);
        if (p_ptr->lev >= 25)
            plr_tim_add(T_RES_POIS, turns);
        if (p_ptr->lev >= 35)
            plr_tim_add(T_RES_NETHER, turns);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _barricade_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Barricade");
        break;
    case SPELL_DESC:
        var_set_string(res, "Creates some nearby rubble.");
        break;
    case SPELL_CAST:
    {
        int y, x, dir, cdir;
        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your weapon.");
            return;
        }

        if (p_ptr->lev >= 45)
        {
            for (dir = 0; dir < 8; dir++)
            {
                y = p_ptr->pos.y + ddy_ddd[dir];
                x = p_ptr->pos.x + ddx_ddd[dir];

                if (!in_bounds(y, x)) continue;
                if (!cave_naked_bold(y, x)) continue;
                cave_set_feat(y, x, feat_rubble);
            }
        }
        else
        {
            if (!get_rep_dir2(&dir)) return;
            if (dir == 5) return;

            for (cdir = 0;cdir < 8; cdir++)
                if (cdd[cdir] == dir) break;

            if (cdir == 8) return;

            y = p_ptr->pos.y + ddy_cdd[cdir];
            x = p_ptr->pos.x + ddx_cdd[cdir];

            if (in_bounds(y, x) && cave_naked_bold(y, x))
                cave_set_feat(y, x, feat_rubble);

            if (p_ptr->lev >= 35)
            {
                y = p_ptr->pos.y + ddy_cdd[(cdir + 7) % 8];
                x = p_ptr->pos.x + ddx_cdd[(cdir + 7) % 8];
                if (in_bounds(y, x) && cave_naked_bold(y, x))
                    cave_set_feat(y, x, feat_rubble);

                y = p_ptr->pos.y + ddy_cdd[(cdir + 1) % 8];
                x = p_ptr->pos.x + ddx_cdd[(cdir + 1) % 8];
                if (in_bounds(y, x) && cave_naked_bold(y, x))
                    cave_set_feat(y, x, feat_rubble);
            }
        }
        p_ptr->update |= (PU_BONUS | PU_FLOW);
        p_ptr->redraw |= PR_MAP;
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _strength_of_the_undertaker_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Strength of Undertaker");
        break;
    case SPELL_DESC:
        var_set_string(res, "You gain additional strength based on the quality of your digger.");
        break;
    default:
        _toggle_spell(TOGGLE_STRENGTH_OF_THE_UNDERTAKER, cmd, res);
        break;
    }
}

static void _stoicism_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Stoicism");
        break;
    case SPELL_DESC:
        var_set_string(res, "You gain additional constitution and stealth based on the quality of your digger.");
        break;
    default:
        _toggle_spell(TOGGLE_STOICISM, cmd, res);
        break;
    }
}

static void _industrious_mortician_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Industrious Mortician");
        break;
    case SPELL_DESC:
        var_set_string(res, "You gain additional attacks and speed when using this technique based on the quality of your digger.");
        break;
    default:
        _toggle_spell(TOGGLE_INDUSTRIOUS_MORTICIAN, cmd, res);
        break;
    }
}

/****************************************************************
 * Polearmmaster
 ****************************************************************/
static void _many_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Many Strike");
        break;
    case SPELL_DESC:
        var_set_string(res, "Your attacks scatter widely among surrounding foes.");
        break;
    default:
        _toggle_spell(TOGGLE_MANY_STRIKE, cmd, res);
        break;
    }
}

static void _piercing_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Piercing Strike");
        break;
    case SPELL_DESC:
        var_set_string(res, "When you attack a foe, successful hits will pierce the opponent attacking additional monsters.");
        break;
    default:
        _toggle_spell(TOGGLE_PIERCING_STRIKE, cmd, res);
        break;
    }
}

static void _trip_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Trip");
        break;
    case SPELL_DESC:
        var_set_string(res, "When you attack a foe, successful hits will attempt to trip up your opponent.");
        break;
    default:
        _toggle_spell(TOGGLE_TRIP, cmd, res);
        break;
    }
}

static void _reach_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Reach");
        break;
    case SPELL_DESC:
        var_set_string(res, "This spell extends the range of your melee attack.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _do_blow_aux(_HIT_REACH, PAC_NO_INNATE, 2 + p_ptr->lev/40));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _knock_back_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Knock Back");
        break;
    case SPELL_DESC:
        var_set_string(res, "A successful attack will push your opponent back a square.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _do_blow(PLR_HIT_KNOCKBACK));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _reaping_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Reaping");
        break;
    case SPELL_DESC:
        var_set_string(res, "A successful strike will damage all adjacent opponents granting you wraithform.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _do_blow(_HIT_REAPING));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

/****************************************************************
 * Shieldmaster
 ****************************************************************/
static void _desperation_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Desperation");
        break;
    case SPELL_DESC:
        var_set_string(res, "You regain hp by disenchanting your current weapon.");
        break;
    case SPELL_CAST:
    {
        int slot, ds, hp;
        object_type *o_ptr = NULL;
        char o_name[MAX_NLEN];

        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do need a shield.");
            return;
        }
        slot = equip_random_slot(obj_is_weapon);
        if (!slot)
        {
            msg_print("Failed! You do need a weapon to disenchant.");
            return;
        }
        o_ptr = equip_obj(slot);
        ds = o_ptr->to_h + o_ptr->to_d;
        object_desc(o_name, o_ptr, OD_NAME_ONLY | OD_OMIT_PREFIX);

        if (ds > 0)
        {
            hp = damroll(7, ds);
            hp_player(hp);

            if (!obj_is_art(o_ptr) || one_in_(2))
            {
                if (o_ptr->to_h > 0) o_ptr->to_h--;
                if (o_ptr->to_d > 0) o_ptr->to_d--;
                msg_format("Your %s is disenchanted.", o_name);
            }
            else
            {
                msg_format("Your %s resists disenchantment.", o_name);
            }
        }
        else
        {
            msg_format("Your %s is too weak to help you any more.", o_name);
        }

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _sanctuary_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Sanctuary");
        break;
    case SPELL_DESC:
        var_set_string(res, "You become invulnerable until you damage a monster.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do need a shield.");
            return;
        }
        set_sanctuary(TRUE);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _shield_bash_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shield Bash");
        break;
    case SPELL_DESC:
        var_set_string(res, "You fight with your shield rather than your weapon.");
        break;
    default:
        _toggle_spell(TOGGLE_SHIELD_BASH, cmd, res);
        break;
    }
}

static void _bulwark_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Bulwark");
        break;
    case SPELL_DESC:
        var_set_string(res, "All melee damage that you receive is reduced.");
        break;
    default:
        _toggle_spell(TOGGLE_BULWARK, cmd, res);
        break;
    }
}

static void _shield_revenge_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Revenge");
        break;
    case SPELL_DESC:
        var_set_string(res, "Monsters are damaged whenever they hurt you.");
        break;
    default:
        _toggle_spell(TOGGLE_SHIELD_REVENGE, cmd, res);
        break;
    }
}

/****************************************************************
 * Slingmaster
 ****************************************************************/
static void _bouncing_pebble_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Bouncing Pebble");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a pebble or shot at an opponent. If you hit, the pebble or shot will ricochet in a random direction.");
        break;
    default:
        _fire_spell(SHOOT_BOUNCE, cmd, res);
    }
}

static void _greater_many_shot_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Greater Many Shot");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires pebbles at all visible monsters.");
        break;
    case SPELL_CAST: {
        obj_ptr bow = equip_obj(p_ptr->shooter_info.slot);
        obj_ptr ammo;
        vec_ptr targets;
        int     i;

        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your shooter.");
            return;
        }
        ammo = _get_ammo(TRUE);
        if (!ammo)
        {
            flush();
            return;
        }

        shoot_hack = SHOOT_ALL;
        targets = _get_greater_many_shot_targets();
        for (i = 0; i < vec_length(targets); i++)
        {
            mon_ptr target = vec_get(targets, i);
            do_cmd_fire_aux2(bow, ammo, p_ptr->pos.x, p_ptr->pos.y, target->pos.x, target->pos.y);
            if (!ammo->number) break;
        }
        vec_free(targets);
        shoot_hack = SHOOT_NONE;

        obj_release(ammo, 0);
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _many_shot_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Many Shot");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires pebbles at all visible monsters. You need to have a direct line of fire to each target, though.");
        break;
    case SPELL_CAST: {
        obj_ptr bow = equip_obj(p_ptr->shooter_info.slot);
        obj_ptr ammo;
        vec_ptr targets;
        int     i;

        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your shooter.");
            return;
        }
        ammo = _get_ammo(TRUE);
        if (!ammo)
        {
            flush();
            return;
        }

        shoot_hack = SHOOT_MANY;
        targets = _get_many_shot_targets();
        for (i = 0; i < vec_length(targets); i++)
        {
            mon_ptr target = vec_get(targets, i);
            do_cmd_fire_aux2(bow, ammo, p_ptr->pos.x, p_ptr->pos.y, target->pos.x, target->pos.y);
            if (!ammo->number) break;
        }
        vec_free(targets);
        shoot_hack = SHOOT_NONE;

        obj_release(ammo, 0);
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _rapid_shot_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Rapid Shot");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique, all of your shots will launch against a single opponent.");
        break;
    default:
        _toggle_spell(TOGGLE_RAPID_SHOT, cmd, res);
        break;
    }
}

static void _shot_on_the_run_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shoot on the Run");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique, you automatically fire at the closest opponent every time you move.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your shooter.");
            return;
        }
        if (_get_toggle() == TOGGLE_SHOT_ON_THE_RUN)
            _set_toggle(TOGGLE_NONE);
        else
        {
            obj_ptr ammo = _get_ammo(FALSE);
            if (!ammo)
            {
                flush();
                return;
            }
            shoot_item = ammo->loc;
            _set_toggle(TOGGLE_SHOT_ON_THE_RUN);
            /* _move_player() will handle the gritty details */
        }
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        if (_get_toggle() != TOGGLE_SHOT_ON_THE_RUN)
            var_set_int(res, 0);    /* no charge for dismissing a technique */
        else
            var_set_int(res, 100);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

/****************************************************************
 * Staffmaster
 ****************************************************************/
bool _design_monkey_clone(void)
{
    int hand;
    monster_race *r_ptr = mon_race_lookup(MON_MONKEY_CLONE);
    int acc = 0;

    if (r_ptr->cur_num == 1)
    {
        msg_print("You may only have one Monkey Clone at a time!");
        return FALSE;
    }

    r_ptr->hdice = 10;
    r_ptr->hside = p_ptr->mhp / 15;
    r_ptr->ac = p_ptr->ac + p_ptr->to_a;
    r_ptr->speed = (byte)p_ptr->pspeed;

    /* Combat */
    vec_clear(r_ptr->blows);
    for (hand = 0; hand < MAX_HANDS; hand++)
    {
        if (p_ptr->attack_info[hand].type == PAT_WEAPON)
        {
            obj_ptr obj = equip_obj(p_ptr->attack_info[hand].slot);
            mon_blow_ptr blow = mon_blow_alloc(RBM_HIT);
            plr_attack_info_ptr info = &p_ptr->attack_info[hand];
            int dd = obj->dd + info->to_dd;
            int ds = obj->ds + info->to_ds;
            int to_d = obj->to_d + info->to_d;

            mon_blow_push_effect(blow, RBE_HURT, dice_create(dd, ds, to_d));
            blow->blows = NUM_BLOWS(hand);
            vec_add(r_ptr->blows, blow);

            acc = hit_chance(hand, obj->to_h, 150);

            break; /* only clone the first hand! */
        }
    }

    r_ptr->level = (60*acc + 5200)/(300 - 3*acc); /* Don't ask ... */

    /* Resistances */
    r_ptr->flagsr = 0;
    r_ptr->flags3 = 0;
    r_ptr->flags2 = 0;
    r_ptr->flags7 = 0;

    if (p_ptr->resist[RES_ACID]) r_ptr->flagsr |= RFR_RES_ACID;
    if (p_ptr->resist[RES_ELEC]) r_ptr->flagsr |= RFR_RES_ELEC;
    if (p_ptr->resist[RES_FIRE]) r_ptr->flagsr |= RFR_RES_FIRE;
    if (p_ptr->resist[RES_COLD]) r_ptr->flagsr |= RFR_RES_COLD;
    if (p_ptr->resist[RES_POIS]) r_ptr->flagsr |= RFR_RES_POIS;
    if (p_ptr->resist[RES_LITE]) r_ptr->flagsr |= RFR_RES_LITE;
    if (p_ptr->resist[RES_DARK]) r_ptr->flagsr |= RFR_RES_DARK;
    if (p_ptr->resist[RES_NETHER]) r_ptr->flagsr |= RFR_RES_NETH;
    if (p_ptr->resist[RES_SHARDS]) r_ptr->flagsr |= RFR_RES_SHAR;
    if (p_ptr->resist[RES_SOUND]) r_ptr->flagsr |= RFR_RES_SOUN;
    if (p_ptr->resist[RES_CHAOS]) r_ptr->flagsr |= RFR_RES_CHAO;
    if (p_ptr->resist[RES_NEXUS]) r_ptr->flagsr |= RFR_RES_NEXU;
    if (p_ptr->resist[RES_DISEN]) r_ptr->flagsr |= RFR_RES_DISE;

    if (p_ptr->resist[RES_CONF]) r_ptr->flags3 |= RF3_NO_CONF;
    if (p_ptr->resist[RES_FEAR]) r_ptr->flags3 |= RF3_NO_FEAR;
    if (p_ptr->free_act) r_ptr->flags3 |= RF3_NO_SLEEP;

    if (p_ptr->reflect) r_ptr->flags2 |= RF2_REFLECTING;
    if (p_ptr->regen >= 200) r_ptr->flags2 |= RF2_REGENERATE;
    if (p_ptr->pass_wall) r_ptr->flags2 |= RF2_PASS_WALL;
    r_ptr->flags2 |= RF2_OPEN_DOOR;
    r_ptr->flags2 |= RF2_CAN_SPEAK;

    r_ptr->flags7 |= RF7_CAN_SWIM;
    if (p_ptr->levitation) r_ptr->flags7 |= RF7_CAN_FLY;

    r_ptr->r_xtra1 |= MR1_LORE;

    return TRUE;
}

static void _monkey_king_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Monkey King's Technique");
        break;
    case SPELL_DESC:
        var_set_string(res, "Create a clone of yourself, but at great cost.");
        break;
    case SPELL_CAST:
    {
        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your weapon.");
            return;
        }
        if (_design_monkey_clone() && summon_named_creature(0, p_ptr->pos, MON_MONKEY_CLONE, PM_FORCE_PET))
            var_set_bool(res, TRUE);
        break;
    }
    case SPELL_COST_EXTRA:
        var_set_int(res, (p_ptr->mhp + 2)/3);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void _circle_kick(void)
{
    int i;
    int dd = 1;
    int ds = p_ptr->lev;
    int bonus = p_ptr->to_h_m;
    int chance = p_ptr->skills.thn + (bonus * BTH_PLUS_ADJ);
    int slot = equip_find_obj(TV_BOOTS, SV_ANY);

    if (slot)
        dd = equip_obj(slot)->ac;

    for (i = 0; i < 8; i++)
    {
        point_t pos = point_step(p_ptr->pos, cdd[i]);
        mon_ptr mon = mon_at(pos);

        if (mon && (mon->ml || cave_have_flag_at(pos, FF_PROJECT)))
        {
            mon_race_ptr race = mon_race(mon);
            char         m_name[MAX_NLEN_MON];

            monster_desc(m_name, mon, 0);

            if (test_hit_norm(chance, mon_ac(mon), mon->ml))
            {
                int dam = damroll(dd, ds) + p_ptr->to_d_m;

                sound(SOUND_HIT);
                msg_format("You kick %s.", m_name);

                if (!(race->flags3 & RF3_NO_STUN))
                    mon_stun(mon, mon_stun_amount(dam));

                dam = mon_damage_mod(mon, dam, FALSE);
                if (dam > 0)
                {
                    bool fear;
                    mon_take_hit(mon->id, dam, &fear, NULL);
                    anger_monster(mon);
                }
                plr_on_touch_mon(mon);
            }
            else
            {
                sound(SOUND_MISS);
                msg_format("You miss %s.", m_name);
            }
        }
    }
}

static void _circle_kick_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Circle Kick");
        break;
    case SPELL_DESC:
        var_set_string(res, "Kicks all adjacent opponents, stunning them. Damage depends on your boots!");
        break;
    case SPELL_INFO:
    {
        int ds = p_ptr->lev;
        int dd = 0;
        int slot = equip_find_obj(TV_BOOTS, SV_ANY);

        if (slot)
            dd = equip_obj(slot)->ac;

        var_set_string(res, info_damage(dd, ds, p_ptr->to_d_m));
        break;
    }
    case SPELL_CAST:
    {
        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your weapon.");
            return;
        }
        _circle_kick();
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

 static bool _vault_attack(void)
{
    int tx, ty;
    int tm_idx = 0;
    point_t path[32];
    point_t cur;
    int path_n, i;
    bool moved = FALSE;
    int flg = PROJECT_THRU | PROJECT_KILL;
    int dir;

    project_length = 3;

    if (!get_fire_dir(&dir)) return FALSE;

    tx = p_ptr->pos.x + project_length * ddx[dir];
    ty = p_ptr->pos.y + project_length * ddy[dir];

    if ((dir == 5) && target_okay())
    {
        tx = target_col;
        ty = target_row;
    }

    if (in_bounds(ty, tx))
    {
        mon_ptr mon = mon_at_xy(tx, ty);
        if (mon) tm_idx = mon->id;
    }

    path_n = project_path(path, project_length, p_ptr->pos, point_create(tx, ty), flg);
    project_length = 0;

    if (!path_n) return FALSE;

    cur = p_ptr->pos;

    for (i = 0; i < path_n; i++)
    {
        point_t next = path[i];
        mon_ptr mon = mon_at(next);
        cave_ptr grid = cave_at(next);
        bool can_enter = FALSE;

        can_enter = !mon && player_can_enter(grid->feat, 0);

        if (can_enter)
        {
            cur = next;
            continue;
        }

        if (!mon)
        {
            msg_print("Failed!");
            break;
        }

        /* Move player before updating the monster */
        if (!plr_at(cur)) move_player_effect(cur, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
        moved = TRUE;

        update_mon(mon, TRUE);

        if (tm_idx != mon->id)
        {
            /* Just like "Acrobatic Charge." Attempts to displace monsters on route. */
            mon_tim_delete(mon, MT_SLEEP);
            move_player_effect(next, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
            cur = next;
            continue;
        }
        plr_attack_normal(next);
        break;
    }

    if (!moved && !plr_at(cur)) move_player_effect(cur, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);

    return TRUE;
}

static void _vault_attack_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Vault Attack");
        break;
    case SPELL_DESC:
        var_set_string(res, "Charge and attack a nearby opponent in a single move.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_range(3));
        break;
    case SPELL_CAST:
    {
        var_set_bool(res, FALSE);
        if (!_check_speciality_equip())
        {
            msg_print("Failed! You do not feel comfortable with your weapon.");
            return;
        }
        if (_vault_attack())
            var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _flurry_of_blows_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Flurry of Blows");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack a single opponent with a great number of blows, exhausting yourself in the process.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _do_blow(_HIT_FLURRY));
        break;
    case SPELL_ENERGY:
        var_set_int(res, 100 + ENERGY_NEED());
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}


/****************************************************************
 * Swordmaster
 ****************************************************************/
static void _burning_blade_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Burning Blade");
        break;
    case SPELL_DESC:
        var_set_string(res, "Damage from your blade becomes fire damage and never misses.");
        break;
    default:
        _toggle_spell(TOGGLE_BURNING_BLADE, cmd, res);
        break;
    }
}

static void _ice_blade_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Ice Blade");
        break;
    case SPELL_DESC:
        var_set_string(res, "Damage from your blade becomes frost damage and slows your opponent.");
        break;
    default:
        _toggle_spell(TOGGLE_ICE_BLADE, cmd, res);
        break;
    }
}

static void _thunder_blade_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Thunder Blade");
        break;
    case SPELL_DESC:
        var_set_string(res, "Damage from your blade becomes lightning damage and stuns your opponent.");
        break;
    default:
        _toggle_spell(TOGGLE_THUNDER_BLADE, cmd, res);
        break;
    }
}

static void _shard_blade_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shard Blade");
        break;
    case SPELL_DESC:
        var_set_string(res, "Your blade becomes very sharp.");
        break;
    default:
        _toggle_spell(TOGGLE_SHARD_BLADE, cmd, res);
        break;
    }
}

static void _chaos_blade_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Chaos Blade");
        break;
    case SPELL_DESC:
        var_set_string(res, "Your blade channels the forces of chaos.");
        break;
    default:
        _toggle_spell(TOGGLE_CHAOS_BLADE, cmd, res);
        break;
    }
}

static void _death_blade_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Death Blade");
        break;
    case SPELL_DESC:
        var_set_string(res, "Your blade thirsts for the blood of the living.");
        break;
    default:
        _toggle_spell(TOGGLE_DEATH_BLADE, cmd, res);
        break;
    }
}

static void _demon_blade_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Demon Blade");
        break;
    case SPELL_DESC:
        var_set_string(res, "Your blade gains devilish powers.");
        break;
    default:
        _toggle_spell(TOGGLE_DEMON_BLADE, cmd, res);
        break;
    }
}

static void _pattern_blade_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Pattern Blade");
        break;
    case SPELL_DESC:
        var_set_string(res, "Your blade becomes a Pattern weapon.");
        break;
    default:
        _toggle_spell(TOGGLE_PATTERN_BLADE, cmd, res);
        break;
    }
}

static void _holy_blade_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Holy Blade");
        break;
    case SPELL_DESC:
        var_set_string(res, "Your blade becomes a Holy Avenger.");
        break;
    default:
        _toggle_spell(TOGGLE_HOLY_BLADE, cmd, res);
        break;
    }
}

static void _armageddon_blade_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Armageddon Blade");
        break;
    case SPELL_DESC:
        var_set_string(res, "Your blade becomes an Armageddon weapon.");
        break;
    default:
        _toggle_spell(TOGGLE_ARMAGEDDON_BLADE, cmd, res);
        break;
    }
}

/****************************************************************
 * Spell Table and Exports
 ****************************************************************/

#define _MAX_OBJECTS_PER_SPECIALITY 32
#define _MAX_SPECIALITIES           11
#define _MAX_SPELLS_PER_SPECIALITY  12

int weaponmaster_get_toggle(void)
{
    /* exposed for prtstatus() in xtra1.c
       this is easier than rewriting the status code so that classes can maintain it!
    */
    int result = TOGGLE_NONE;
    if (p_ptr->pclass == CLASS_WEAPONMASTER)
        result = _get_toggle();
    return result;
}

void weaponmaster_set_toggle(int toggle)
{
    if (p_ptr->pclass == CLASS_WEAPONMASTER)
        _set_toggle(toggle);
}

#define _WEAPONMASTER_MELEE   1
#define _WEAPONMASTER_SHIELDS 2
#define _WEAPONMASTER_BOWS    3

typedef struct {
    byte tval;
    byte sval;
} _object_kind;

typedef struct {
    cptr name;
    cptr help;
    int kind;
    int stats[MAX_STATS];
    skills_t base_skills;
    skills_t extra_skills;
    _object_kind objects[_MAX_OBJECTS_PER_SPECIALITY];    /* There is always a sentinel at the end */
    spell_info spells[_MAX_SPELLS_PER_SPECIALITY];        /* There is always a sentinel at the end */
    _object_kind birth_obj;
} _speciality;


/*  p_ptr->psubclass indexes into _specialities.
    This index is persisted in savefiles and are chosen
    by the player at startup, so moving things around is
    unwise unless you put code to fix up old savefiles
    in load.c.
*/
static _speciality _specialities[_MAX_SPECIALITIES] = {
    { "Axes",
      "The mighty axe! Your blows will cleave with damage unsurpassed. "
      "Specializing in axes gives great offensive prowess, especially when "
      "your axe is wielded with two hands. However, this speciality offers "
      "little in the way of utility. Kill quickly as your life depends on it!",
      _WEAPONMASTER_MELEE,
    /*  S   I   W   D   C   C */
      {+3, -2, -1, -2, +1,  0},
    /* Dsrm Dvce Save Stlh Srch Prcp Thn Thb*/
      {  25,  23,  31,   1,  14,   2, 70, 25},
      {   9,   7,  10,   0,   0,   0, 30, 11},
      { { TV_POLEARM, SV_BATTLE_AXE },
        { TV_POLEARM, SV_BEAKED_AXE },
        { TV_POLEARM, SV_BROAD_AXE },
        { TV_POLEARM, SV_LOCHABER_AXE },
        { TV_POLEARM, SV_GREAT_AXE },
        { 0, 0 },
      },
      { { 10,   0,  0, _power_attack_spell },
        { 15,  10,  0, berserk_spell },
        { 25,  20, 50, _judge_spell },
        { 25,  12,  0, _crusaders_strike_spell },
        { 35,  25, 50, massacre_spell },
        { 40,  25,  0, _vicious_strike_spell },
        { -1,   0,  0, NULL },
      },
      { TV_POLEARM, SV_BROAD_AXE },
    },
    { "Bows",
      "You will shoot to kill! The bowmaster gains techniques to enhance shooting, "
      "including more rapid firing, the ability to volley shots over the heads of "
      "intervening monsters, reduced ammo destruction, the ability to kill with a "
      "single arrow and much more. As a shooter, your missile prowess will be quite "
      "formidable, though your melee will be somewhat lacking.",
      _WEAPONMASTER_BOWS,
    /*  S   I   W   D   C   C */
      { 0,  0,  0, +2, -1,  0},
    /* Dsrm Dvce Save Stlh Srch Prcp Thn Thb*/
      {  30,  30,  29,   4,  23,  13, 48, 72},
      {   8,  10,  10,   0,   0,   0, 13, 28},
      { { TV_BOW, SV_SHORT_BOW },
        { TV_BOW, SV_LONG_BOW },
        { TV_BOW, SV_GREAT_BOW },
        { 0, 0 },
      },
      { {  5,   0,  0, _readied_shot_spell },
        { 10,   5,  0, _volley_spell },
        { 25,  20, 50, _judge_spell },
        { 25,  10,  0, _tranquilizing_arrow_spell },
        { 30,   0,  0, _piercing_arrow_spell },
        { 35,  20,  0, _arrow_of_slaying_spell },
        { 40,  30,  0, _disintegration_arrow_spell },
        { -1,   0,  0, NULL },
      },
      { TV_BOW, SV_SHORT_BOW },
    },
    { "Clubs",
        "You will seek to club your opponents senseless! This speciality gains passive "
        "status effects against monsters, such as confusion, knock out and stunning. Also, "
        "you will gain some limited utility techniques. At high levels, your weapons will "
        "become more likely to score devastating, crushing blows against your hapless enemies.",
        _WEAPONMASTER_MELEE,
    /*  S   I   W   D   C   C */
      {+2, -1, -1, -2, +1,  0},
    /* Dsrm Dvce Save Stlh Srch Prcp Thn Thb*/
      {  25,  25,  35,   1,  14,   2, 65, 30},
      {   9,  10,  12,   0,   0,   0, 20, 15},
        { { TV_HAFTED, SV_BALL_AND_CHAIN },
          { TV_HAFTED, SV_CLUB },
          { TV_HAFTED, SV_FLAIL },
          { TV_HAFTED, SV_GREAT_HAMMER },
          { TV_HAFTED, SV_LEAD_FILLED_MACE },
          { TV_HAFTED, SV_MACE },
          { TV_HAFTED, SV_MACE_OF_DISRUPTION },
          { TV_HAFTED, SV_MORNING_STAR },
          { TV_HAFTED, SV_TWO_HANDED_FLAIL },
          { TV_HAFTED, SV_WAR_HAMMER },
          { TV_HAFTED, SV_GROND },
          { 0, 0 },
        },
        { {  5,   0,  0, _combat_expertise_spell },
          { 10,   5,  0, _throw_weapon_spell },
          { 15,  10,  0, _cunning_strike_spell },
          { 25,  20, 50, _judge_spell },
          { 25,  15, 40, _smash_ground_spell },
          { 30,  25,  0, _smite_evil_spell },
          { 35,   0,  0, _trade_blows_spell },
          { -1,   0,  0, NULL },
        },
        { TV_HAFTED, SV_CLUB },
    },
    { "Crossbows",
      "The crossbowmaster shoots deadly bolts for great damage. Their bolts may explode "
      "powerfully damaging nearby monsters. Also, they may shoot so hard as to knock "
      "their opponents backwards!",
      _WEAPONMASTER_BOWS,
    /*  S   I   W   D   C   C */
      {+1, -1, -1, +1, +1,  0},
    /* Dsrm Dvce Save Stlh Srch Prcp Thn Thb*/
      {  30,  30,  29,   3,  18,  10, 48, 72},
      {   8,   9,   9,   0,   0,   0, 13, 28},
      { { TV_BOW, SV_LIGHT_XBOW },
        { TV_BOW, SV_HEAVY_XBOW },
        { 0, 0 },
      },
      { {  5,   0,  0, _careful_aim_spell },
        { 10,  10,  0, _shattering_bolt_spell },
        { 25,  20, 50, _judge_spell },
        { 25,  15,  0, _knockback_bolt_spell },
        { 30,   0,  0, _exploding_bolt_spell },
        { 35,  30,  0, _elemental_bolt_spell },
        { 38,   0,  0, _rapid_reload_spell },
        { 42,   0,  0, _overdraw_spell },
        { -1,   0,  0, NULL },
      },
      { TV_BOW, SV_LIGHT_XBOW },
    },
    { "Daggers",
      "A knife in the back! This speciality favors dual wielding and rogue-like behavior. "
      "The daggermaster can even assume the posture of The Flying Dagger which greatly "
      "enhances their low level dagger toss capability. Indeed, their prowess with the "
      "dagger toss is legendary and appears almost magical! At high levels, you will also "
      "gain formidable melee prowess with the Frenzy Stance. Finally, daggermasters have "
      "very strong short ranged teleport techniques that synergize well with their toss "
      "abilities.",
      _WEAPONMASTER_MELEE,
    /*  S   I   W   D   C   C */
      { 0, +1,  0, +3, -1,  0},
    /* Dsrm Dvce Save Stlh Srch Prcp Thn Thb*/
      {  30,  32,  31,   5,  30,  20, 60, 66},
      {  12,  10,  10,   0,   0,   0, 18, 20},
      { { TV_SWORD, SV_BASILLARD },
        { TV_SWORD, SV_BROKEN_DAGGER },
        { TV_SWORD, SV_DAGGER },
        { TV_SWORD, SV_FALCON_SWORD },
        { TV_SWORD, SV_MAIN_GAUCHE },
        { TV_SWORD, SV_NINJATO },
        { TV_SWORD, SV_RAPIER },
        { TV_SWORD, SV_SABRE },
        { TV_SWORD, SV_TANTO },
        { TV_SWORD, SV_DRAGON_FANG },
        { 0, 0 },
      },
      {
        {  5,   5,  0, _dagger_toss_spell },
        { 10,   5, 40, strafing_spell },
        { 15,   0,  0, _flying_dagger_spell },
        { 25,  20, 50, _judge_spell },
        { 30,  10,  0, _elusive_strike_spell },
        { 35,   0,  0, _shadow_stance_spell },
        { 45,   0,  0, _frenzy_spell },
        { -1,   0,  0, NULL },
      },
      { TV_SWORD, SV_DAGGER },
    },
    { "Polearms",
      "You don a grim face before setting out to reap your harvest of death. You will swing "
      "your weapon wide often affecting multiple surrounding opponents.",
      _WEAPONMASTER_MELEE,
    /*  S   I   W   D   C   C */
      {+2, -1, -1,  0, +1,  0},
    /* Dsrm Dvce Save Stlh Srch Prcp Thn Thb*/
      {  25,  23,  31,   1,  14,   2, 68, 25},
      {  10,   8,  10,   0,   0,   0, 28, 11},
      {
        { TV_POLEARM, SV_AWL_PIKE },
        { TV_POLEARM, SV_BROAD_SPEAR },
        { TV_POLEARM, SV_DEATH_SCYTHE },
        { TV_POLEARM, SV_HALBERD },
        { TV_POLEARM, SV_FAUCHARD },
        { TV_POLEARM, SV_GLAIVE },
        { TV_POLEARM, SV_GUISARME },
        { TV_POLEARM, SV_LUCERNE_HAMMER },
        { TV_POLEARM, SV_NAGINATA },
        { TV_POLEARM, SV_PIKE },
        { TV_POLEARM, SV_SCYTHE },
        { TV_POLEARM, SV_SCYTHE_OF_SLICING },
        { TV_POLEARM, SV_SPEAR },
        { TV_POLEARM, SV_TRIDENT },
        { TV_POLEARM, SV_LANCE },
        { TV_POLEARM, SV_HEAVY_LANCE },
        { TV_POLEARM, SV_TRIFURCATE_SPEAR },
        { 0, 0 },
      },
      {
        {  5,   0,  0, _many_strike_spell },
        { 10,   5,  0, _reach_spell },
        { 15,  15,  0, _knock_back_spell },
        { 25,  20, 50, _judge_spell },
        { 25,   0,  0, _piercing_strike_spell },
        { 35,   0,  0, _trip_spell },
        { 40,  40,  0, _reaping_spell },
        { -1,   0,  0, NULL },
      },
      { TV_POLEARM, SV_SPEAR },
    },
    { "Shields",
      "Specializing in shields gives excellent powers of defense and retaliation. In addition, "
      "you can even choose to melee with your shield rather than a normal weapon, bashing your "
      "opponents senseless. This form of combat is known as Shield Bashing and is unique to this "
      "speciality.",
      _WEAPONMASTER_SHIELDS,
    /*  S   I   W   D   C   C */
      {+2,  0, +1,  0, +2,  0},
    /* Dsrm Dvce Save Stlh Srch Prcp Thn Thb*/
      {  25,  24,  40,   1,  12,   2, 68, 25},
      {  10,   9,  12,   0,   0,   0, 21, 11},
      {
        { TV_SHIELD, SV_DRAGON_SHIELD },
        { TV_SHIELD, SV_KNIGHT_SHIELD },
        { TV_SHIELD, SV_LARGE_LEATHER_SHIELD },
        { TV_SHIELD, SV_LARGE_METAL_SHIELD },
        { TV_SHIELD, SV_MIRROR_SHIELD },
        { TV_SHIELD, SV_SMALL_LEATHER_SHIELD },
        { TV_SHIELD, SV_SMALL_METAL_SHIELD },
        { 0, 0 },
      },
      {
        { 10,  0,  0, _shield_bash_spell },
        { 15, 10,  0, _desperation_spell },
        { 25,  20, 50, _judge_spell },
        { 30,  0,  0, _bulwark_spell },
        { 35, 50,  0, _sanctuary_spell },
        { 40,  0,  0, _shield_revenge_spell },
        { -1,  0,  0, NULL },
      },
      { TV_SHIELD, SV_SMALL_LEATHER_SHIELD },
    },
    { "Slings",
      "Watch out, Goliath! As a master of slings you will shoot pebbles with uncanny speed. Your "
      "shots may even ricochet off other monsters to score multiple hits. As with other archery "
      "specializations, your ammo will break less often.",
      _WEAPONMASTER_BOWS,
    /*  S   I   W   D   C   C */
      {-1, +1, +1, +3, -1,  0},
    /* Dsrm Dvce Save Stlh Srch Prcp Thn Thb*/
      {  30,  30,  29,   4,  23,  13, 48, 72},
      {   8,  10,  10,   0,   0,   0, 13, 28},
      { { TV_BOW, SV_SLING },
        { 0, 0 },
      },
      {
        {  5,   5,  0, _bouncing_pebble_spell },
        { 15,  15,  0, _many_shot_spell },
        { 25,  20, 50, _judge_spell },
        { 25,   0,  0, _shot_on_the_run_spell },
        { 30,  15,  0, _greater_many_shot_spell },
        { 35,   0,  0, _rapid_shot_spell },
        { -1,   0,  0, NULL },
      },
      { TV_BOW, SV_SLING },
    },
    { "Staves",
      "Monkey King! You will battle opponents with a flurry of blows from your mighty "
      "staff and will be prepared to counter the attacks of your enemies. You can vault into "
      "battle and circle kick enemies for stunning effects, attacking them with your boots! "
      "You may even eventually clone yourself at great cost.",
      _WEAPONMASTER_MELEE,
    /*  S   I   W   D   C   C */
      { 0,  0,  0, +2, +1,  0},
    /* Dsrm Dvce Save Stlh Srch Prcp Thn Thb*/
      {  25,  25,  31,   2,  14,   4, 63, 25},
      {  10,   9,  10,   0,   0,   0, 26, 11},
      {
        { TV_HAFTED, SV_BO_STAFF },
        { TV_HAFTED, SV_JO_STAFF },
        { TV_HAFTED, SV_QUARTERSTAFF },
        { TV_HAFTED, SV_THREE_PIECE_ROD },
        { 0, 0 },
      },
      {
        { 15, 15, 0, _vault_attack_spell },
        { 25, 20,50, _judge_spell },
        { 25, 25, 0, _circle_kick_spell },
        { 40,  0, 0, _monkey_king_spell },
        { 45, 80, 0, _flurry_of_blows_spell },
        { -1,  0, 0, NULL },
      },
      { TV_HAFTED, SV_QUARTERSTAFF },
    },
    { "Swords",
      "You will become a true swordmaster! Mastery of the blade will augment "
      "your weapon with elemental, vorpal or vampiric powers.",
      _WEAPONMASTER_MELEE,
    /*  S   I   W   D   C   C */
      {+1, -1, -1, +1, +1,  0},
    /* Dsrm Dvce Save Stlh Srch Prcp Thn Thb*/
      {  25,  23,  31,   1,  14,   2, 70, 25},
      {  11,   9,  10,   0,   0,   0, 29, 11},
      { { TV_SWORD, SV_BASTARD_SWORD },
        { TV_SWORD, SV_BROKEN_SWORD },
        { TV_SWORD, SV_BLADE_OF_CHAOS },
        { TV_SWORD, SV_BROAD_SWORD },
        { TV_SWORD, SV_CLAYMORE },
        { TV_SWORD, SV_CUTLASS },
        { TV_SWORD, SV_DIAMOND_EDGE },
        { TV_SWORD, SV_ESPADON },
        { TV_SWORD, SV_EXECUTIONERS_SWORD },
        { TV_SWORD, SV_FLAMBERGE },
        { TV_SWORD, SV_GREAT_SCIMITAR }, /* Falchion */
        { TV_SWORD, SV_KATANA },
        { TV_SWORD, SV_LONG_SWORD },
        { TV_SWORD, SV_KHOPESH },
        { TV_SWORD, SV_NO_DACHI },
        { TV_SWORD, SV_SCIMITAR },
        { TV_SWORD, SV_SHORT_SWORD },
        { TV_SWORD, SV_SMALL_SWORD },
        { TV_SWORD, SV_TULWAR },
        { TV_SWORD, SV_TWO_HANDED_SWORD },
        { TV_SWORD, SV_WAKIZASHI },
        { TV_SWORD, SV_ZWEIHANDER },
        { 0, 0 },
      },
      {
        {  5,   0,  0, _burning_blade_spell },
        { 10,   0,  0, _ice_blade_spell },
        { 15,   0,  0, _thunder_blade_spell },
        { 20,   0,  0, _shard_blade_spell },
        { 25,  20, 50, _judge_spell },
        { 25,   0,  0, _chaos_blade_spell },
        { 30,   0,  0, _death_blade_spell },
        { 35,   0,  0, _demon_blade_spell },
        { 40,   0,  0, _pattern_blade_spell },
        { 45,   0,  0, _holy_blade_spell },
        { 50,   0,  0, _armageddon_blade_spell },
        { -1,   0,  0, NULL },
      },
      { TV_SWORD, SV_LONG_SWORD } ,
    },
    { "Diggers",
      "A master of digging. You prefer rocky enclosures and don't mind "
      "lugging around a corpse or two, which you can bury in a pinch for "
      "a temporary bonus. You can choose one of several postures which allow "
      "you to use the digging bonus of your current weapon as an additional "
      "bonus, such as increased strength, constitution, or even speed! So keep "
      "your eye open for those +8 diggers!",
      _WEAPONMASTER_MELEE,
    /*  S   I   W   D   C   C */
      {+2, -1, -1,  0, +1,  0},
    /* Dsrm Dvce Save Stlh Srch Prcp Thn Thb*/
      {  25,  25,  33,   3,  14,   2, 60, 25},
      {   9,   9,  11,   0,   0,   0, 26, 11},
      {
        { TV_DIGGING, SV_SHOVEL},
        { TV_DIGGING, SV_GNOMISH_SHOVEL},
        { TV_DIGGING, SV_DWARVEN_SHOVEL},
        { TV_DIGGING, SV_PICK},
        { TV_DIGGING, SV_ORCISH_PICK},
        { TV_DIGGING, SV_DWARVEN_PICK},
        { TV_DIGGING, SV_MATTOCK},
        { 0, 0 },
      },
      {
        {  5, 10, 30, _bury_dead_spell },
        { 10,  0,  0, _strength_of_the_undertaker_spell },
        { 20, 20, 40, _tunnel_spell },
        { 25,  20, 50, _judge_spell },
        { 25,  0,  0, _stoicism_spell },
        { 30, 25, 40, _barricade_spell },
        { 35, 30,  0, _calamity_of_the_living_spell },
        { 40,  0,  0, _industrious_mortician_spell },
        { -1,  0,  0, NULL },
      },
      { TV_DIGGING, SV_PICK },
    },
};

static bool _check_speciality_aux(object_type *o_ptr)
{
    int i;
    _speciality *ptr = &_specialities[p_ptr->psubclass];

    for (i = 0; i < _MAX_OBJECTS_PER_SPECIALITY; i++)
    {
        if (ptr->objects[i].tval == 0) break;
        if (ptr->objects[i].tval == o_ptr->tval && ptr->objects[i].sval == o_ptr->sval) return TRUE;
    }

    return FALSE;
}

static bool _can_judge(obj_ptr obj)
{
    switch (p_ptr->psubclass)
    {
    case WEAPONMASTER_CROSSBOWS:
        if (obj->tval == TV_BOLT) return TRUE;
        break;
    case WEAPONMASTER_SLINGS:
        if (obj->tval == TV_SHOT) return TRUE;
        break;
    case WEAPONMASTER_BOWS:
        if (obj->tval == TV_ARROW) return TRUE;
        break;
    }
    return _check_speciality_aux(obj);
}

static bool _check_speciality_equip(void)
{
    bool result = equip_find_first(_check_speciality_aux);

    /* For melee specialities, all melee weapons must be favored. Shieldmasters
       and shooters just need a single matching object to be OK. */
    if (_specialities[p_ptr->psubclass].kind == _WEAPONMASTER_MELEE)
    {
        int slot;
        for (slot = equip_find_first(obj_is_weapon);
                slot;
                slot = equip_find_next(obj_is_weapon, slot))
        {
            object_type *o_ptr = equip_obj(slot);
            if (!_check_speciality_aux(o_ptr))
                return FALSE;
        }
    }
    return result;
}

bool weaponmaster_is_favorite(object_type *o_ptr)
{
    if (p_ptr->pclass != CLASS_WEAPONMASTER) return FALSE;
    return _check_speciality_aux(o_ptr);
}
bool weaponmaster_kind_is_favorite(obj_kind_ptr kind)
{
    int i;
    _speciality *ptr;

    if (p_ptr->pclass != CLASS_WEAPONMASTER) return FALSE;

    ptr = &_specialities[p_ptr->psubclass];
    for (i = 0; i < _MAX_OBJECTS_PER_SPECIALITY; i++)
    {
        if (ptr->objects[i].tval == 0) break;
        if (ptr->objects[i].tval == kind->tval && ptr->objects[i].sval == kind->sval) return TRUE;
    }

    return FALSE;
}

static int _get_spells_aux(spell_info* spells, int max)
{
    int i;
    int ct = 0;

    for (i = 0; ; i++)
    {
        spell_info *base = &_specialities[p_ptr->psubclass].spells[i];
        if (base->level <= 0) break;
        if (ct >= max) break;
        if (base->level <= p_ptr->lev)
        {
            spell_info* current = &spells[ct++];
            current->fn = base->fn;
            current->level = base->level;
            current->cost = base->cost;
            current->fail = calculate_fail_rate(base->level, base->fail, p_ptr->stat_ind[A_STR]);
        }
    }

    return ct;
}

static int _get_spells(spell_info* spells, int max)
{
    int ct = _get_spells_aux(spells, max);

    if (ct == 0)
        msg_print("You need more experience. Why not kill something?");

    return ct;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.which_stat = A_STR;
        me.magic_desc = "skill";
        me.options = CASTER_USE_HP;
        init = TRUE;
    }
    return &me;
}

void _on_birth(void)
{
    object_type forge;
    _object_kind kind;
    int i;

    /* Give the player a starting weapon from this group */
    kind = _specialities[p_ptr->psubclass].birth_obj;
    object_prep(&forge, lookup_kind(kind.tval, kind.sval));
    plr_birth_obj(&forge);

    if (kind.tval == TV_BOW)
    {
        switch (kind.sval)
        {
        case SV_SLING:
            object_prep(&forge, lookup_kind(TV_SHOT, SV_SHOT));
            forge.number = (byte)rand_range(15, 20);
            plr_birth_obj(&forge);
            break;
        case SV_SHORT_BOW:
        case SV_LONG_BOW:
        case SV_GREAT_BOW:
            object_prep(&forge, lookup_kind(TV_ARROW, SV_ARROW));
            forge.number = (byte)rand_range(15, 20);
            plr_birth_obj(&forge);
            break;
        case SV_LIGHT_XBOW:
        case SV_HEAVY_XBOW:
            object_prep(&forge, lookup_kind(TV_BOLT, SV_BOLT));
            forge.number = (byte)rand_range(15, 20);
            plr_birth_obj(&forge);
            break;
        }
        object_prep(&forge, lookup_kind(TV_SWORD, SV_DAGGER));
        plr_birth_obj(&forge);
    }

    for (i = 0; i < _MAX_OBJECTS_PER_SPECIALITY; i++)
    {
        kind = _specialities[p_ptr->psubclass].objects[i];
        if (kind.tval == 0) break;

        if (kind.tval != TV_SHIELD)
            p_ptr->weapon_exp[kind.tval-TV_WEAPON_BEGIN][kind.sval] = WEAPON_EXP_BEGINNER;
    }

    if (p_ptr->psubclass == WEAPONMASTER_SHIELDS)
    {
        skills_shield_init(SV_SMALL_LEATHER_SHIELD, WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);
        skills_shield_init(SV_SMALL_METAL_SHIELD, WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);
        skills_shield_init(SV_LARGE_LEATHER_SHIELD, WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);
        skills_shield_init(SV_LARGE_METAL_SHIELD, WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);
    }
    weaponmaster_adjust_skills();

    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_LEATHER_JACK, 1);
}

static void _set_max_skill(int tval, int skill)
{
    int j;
    for (j = 0; j < 64; j++)
        s_info[p_ptr->pclass].w_max[tval - TV_WEAPON_BEGIN][j] = skill;
}

void weaponmaster_adjust_skills(void)
{
    int i, j;
    _object_kind kind;

    /* Fix up skills for Speciality. This needs to be called every time the game is loaded! */
    /* Bang everything in class (melee, bows, shields) down to unskilled max */
    switch (_specialities[p_ptr->psubclass].kind)
    {
    case _WEAPONMASTER_MELEE:
        _set_max_skill(TV_DIGGING, WEAPON_EXP_UNSKILLED);
        _set_max_skill(TV_HAFTED, WEAPON_EXP_UNSKILLED);
        _set_max_skill(TV_POLEARM, WEAPON_EXP_UNSKILLED);
        _set_max_skill(TV_SWORD, WEAPON_EXP_UNSKILLED);
        break;

    case _WEAPONMASTER_BOWS:
        _set_max_skill(TV_BOW, WEAPON_EXP_UNSKILLED);
        break;

    case _WEAPONMASTER_SHIELDS:
        /* This only needs to be done once, in _birth. */
        break;
    }

    /* Now make favored weapons "masterable" */
    for (i = 0; i < _MAX_OBJECTS_PER_SPECIALITY; i++)
    {
        kind = _specialities[p_ptr->psubclass].objects[i];
        if (kind.tval == 0 || kind.tval == TV_SHIELD) break;

        s_info[p_ptr->pclass].w_max[kind.tval-TV_WEAPON_BEGIN][kind.sval] = WEAPON_EXP_MASTER;
    }

    /* Patch up current skills since we keep changing the allowed maximums */
    for (i = 0; i < 5; i++)
    {
        for (j = 0; j < 64; j++)
        {
            if (p_ptr->weapon_exp[i][j] > s_info[p_ptr->pclass].w_max[i][j])
                p_ptr->weapon_exp[i][j] = s_info[p_ptr->pclass].w_max[i][j];
        }
    }
}

static int _max_pval(void)
{
    int slot;
    int result = 0;

    for (slot = equip_find_first(obj_is_weapon);
            slot;
            slot = equip_find_next(obj_is_weapon, slot))
    {
        object_type *o_ptr = equip_obj(slot);
        result = MAX(result, o_ptr->pval);
    }
    return result;
}

static void _calc_bonuses(void)
{
    static bool last_spec = FALSE;
    static bool init = FALSE;
    bool spec = _check_speciality_equip();

    p_ptr->speciality_equip = spec;

    /* Handle cases where user swaps in unfavorable gear */
    if (!spec && _get_toggle() != TOGGLE_NONE)
    {
        /* Triggering a recursive call to calc_bonuses would be bad ... */
        /*    _set_toggle(TOGGLE_NONE); */
        /* This assumes all bonus calcs are handled here and in _calc_weapon_bonuses() */
        p_ptr->magic_num1[0] = TOGGLE_NONE;
        p_ptr->redraw |= (PR_STATUS);
    }

    if (p_ptr->psubclass == WEAPONMASTER_SLINGS)
    {
        if (spec)
        {
            p_ptr->return_ammo = TRUE;

            if (p_ptr->lev >= 10)
                p_ptr->painted_target = TRUE;

            if (p_ptr->lev >= 20)
                p_ptr->big_shot = TRUE;
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_BOWS)
    {
        if (spec)
        {
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_CROSSBOWS)
    {
        if (spec)
        {
            switch (_get_toggle())
            {
            case TOGGLE_RAPID_RELOAD:
                p_ptr->to_a -= 30;
                p_ptr->dis_to_a -= 30;
                break;
            }
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_DAGGERS)
    {
        if (spec)
        {
            p_ptr->easy_2weapon = TRUE;
            if (p_ptr->lev >= 25 && p_ptr->weapon_ct > 1)
            {
                p_ptr->to_a += 10 + p_ptr->lev/2;
                p_ptr->dis_to_a += 10 + p_ptr->lev/2;
            }

            p_ptr->skills.stl += p_ptr->lev/12;

            if (p_ptr->lev >= 30)
                p_ptr->ambush = 300 + p_ptr->lev*4;

            switch (_get_toggle())
            {
            case TOGGLE_SHADOW_STANCE:
                p_ptr->shooter_info.to_d -= 10;
                p_ptr->shooter_info.dis_to_d -= 10;
                p_ptr->to_d_m -= 10;
                p_ptr->skills.stl += p_ptr->lev/12;
                break;

            case TOGGLE_FRENZY_STANCE:
                p_ptr->skills.stl -= 8;
                p_ptr->to_a -= 25;
                p_ptr->dis_to_a -= 25;
                break;
            }
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_CLUBS)
    {
        if (spec)
        {
            if (p_ptr->lev >= 40)
            {
                p_ptr->crit_freq_mul = 150;
                p_ptr->crit_qual_add = 650;
            }

            switch (_get_toggle())
            {
            case TOGGLE_TRADE_BLOWS:
                p_ptr->to_a -= 30;
                p_ptr->dis_to_a -= 30;
                break;
            case TOGGLE_COMBAT_EXPERTISE:
                p_ptr->to_a += 5 + p_ptr->lev;
                p_ptr->dis_to_a += 5 + p_ptr->lev;
                break;
            }
            p_ptr->skill_tht += 2*p_ptr->lev;
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_AXES)
    {
        if (spec)
        {
            if (p_ptr->lev >= 5)
                p_ptr->skill_dig += (5 * 20); /* As if you had a +5 digger ... */

            if (p_ptr->lev >= 30)
                p_ptr->cleave = TRUE;
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_SWORDS)
    {
        if (spec)
        {
            if (p_ptr->lev >= 20)
                plr_tim_lock(T_HERO);
            else
                plr_tim_unlock(T_HERO);

            switch (_get_toggle())
            {
            case TOGGLE_CHAOS_BLADE:
                res_add(RES_CHAOS);
                break;
            case TOGGLE_DEATH_BLADE:
                res_add(RES_NETHER);
                if (p_ptr->lev >= 35) res_add(RES_POIS);
                if (p_ptr->lev >= 40) res_add(RES_DARK);
                p_ptr->hold_life++;
                break;
            case TOGGLE_DEMON_BLADE:
                p_ptr->skills.stl -= 3;
                break;
            case TOGGLE_PATTERN_BLADE:
                p_ptr->free_act++;
                p_ptr->see_inv++;
                break;
            case TOGGLE_HOLY_BLADE:
                res_add(RES_FEAR);
                p_ptr->see_inv++;
                break;
            }
        }
        else if (last_spec && p_ptr->lev >= 20)
        {
            p_ptr->redraw |= (PR_STATUS);
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_POLEARMS)
    {
        if (spec)
        {
            if (p_ptr->lev >= 45)
                p_ptr->whirlwind = TRUE;

            if (p_ptr->lev >= 30)
            {
                if (p_ptr->entrench_ct >= 3)
                {
                    p_ptr->entrenched = TRUE;
                    p_ptr->redraw |= PR_EFFECTS;

                    p_ptr->to_a += 20 + p_ptr->entrench_ct;
                    p_ptr->dis_to_a += 20 + p_ptr->entrench_ct;
                }
            }
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_SHIELDS)
    {
        if (spec)
        {
            if (p_ptr->lev >= 20)
                p_ptr->inven_prot = TRUE;

            if (p_ptr->lev >= 45)
            {
                res_add(RES_ACID);
                res_add(RES_COLD);
                res_add(RES_FIRE);
                res_add(RES_ELEC);
                p_ptr->reflect = TRUE;
            }

            /* Block: Shield AC doubled. */
            if (p_ptr->lev >= 5)
            {
                int slot;
                for (slot = equip_find_first(obj_is_shield);
                        slot;
                        slot = equip_find_next(obj_is_shield, slot))
                {
                    object_type *o_ptr = equip_obj(slot);
                    p_ptr->to_a += k_info[o_ptr->k_idx].ac;
                    p_ptr->dis_to_a += k_info[o_ptr->k_idx].ac;
                    p_ptr->to_a += o_ptr->to_a;
                    if (obj_is_known(o_ptr))
                        p_ptr->dis_to_a += o_ptr->to_a;
                }
            }

            /* Stalwart: +20 saving throws */
            if (p_ptr->lev >= 25)
                p_ptr->skills.sav += 20;
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_STAVES)
    {
        if (spec)
        {
            if (p_ptr->elaborate_defense)
            {
                p_ptr->to_a += 10 + p_ptr->lev*2/3;
                p_ptr->dis_to_a += 10 + p_ptr->lev*2/3;
            }

            if (p_ptr->lev >= 10)
                p_ptr->sh_retaliation = TRUE;

            if (p_ptr->lev >= 20)
                p_ptr->pspeed += 2;

            if (p_ptr->cloak_of_shadows && !p_ptr->elaborate_defense)
            {
                p_ptr->to_a += 10 + p_ptr->lev*2/3;
                p_ptr->dis_to_a += 10 + p_ptr->lev*2/3;
            }

            if (p_ptr->lev >= 35)
                p_ptr->lightning_reflexes = TRUE;
        }

        if (equip_find_first(obj_is_shield))
            p_ptr->pspeed -= 5;
    }
    else if (p_ptr->psubclass == WEAPONMASTER_DIGGERS)
    {
        if (spec)
        {
            int pval = _max_pval();

            p_ptr->skill_dig += (5 + p_ptr->lev/5) * 20;

            if (p_ptr->lev >= 45)
                p_ptr->kill_wall = TRUE;

            if (p_ptr->lev >= 15) /* Earthen Shield */
            {
                int dir, x, y;
                int count = 0;
                int mult = 2 + p_ptr->lev/10;

                for (dir = 0; dir < 8; dir++)
                {
                    y = p_ptr->pos.y + ddy_ddd[dir];
                    x = p_ptr->pos.x + ddx_ddd[dir];

                    if (!in_bounds(y, x)) continue;

                    if ( cave_have_flag_bold(y, x, FF_WALL)
                      || cave_at_xy(x, y)->feat == feat_rubble )
                    {
                        count++;
                    }
                }
                p_ptr->to_a += mult*count;
                p_ptr->dis_to_a += mult*count;
            }
            switch (_get_toggle())
            {
            case TOGGLE_STOICISM:
                p_ptr->skills.stl += pval;
                break;
            case TOGGLE_INDUSTRIOUS_MORTICIAN:
                p_ptr->pspeed += pval;
                break;
            }
        }
    }

    if (!p_ptr->painted_target)
    {
        p_ptr->painted_target_idx = 0;
        p_ptr->painted_target_ct = 0;
    }

    /* Hack -- handle "xtra" mode
       This works around a startup glitch where the screen is only half painted.
       If we do a msg_print at this point, the user gets a face full of yuk!
    */
    if (character_xtra) return;

    /* Message about favored gear */
    if (!init || spec != last_spec)
    {
        int kind = _specialities[p_ptr->psubclass].kind;
        if (!spec)
        {
            switch (kind)
            {
            case _WEAPONMASTER_BOWS:
                msg_print("You do not feel comfortable with your shooter.");
                break;
            case _WEAPONMASTER_SHIELDS:
                msg_print("You do not feel comfortable with your shield.");
                break;
            default:
                msg_print("You do not feel comfortable with your weapon.");
                break;
            }
        }
        else if (init)
        {
            switch (kind)
            {
            case _WEAPONMASTER_BOWS:
                msg_print("You love your shooter.");
                break;
            case _WEAPONMASTER_SHIELDS:
                msg_print("You love your shield.");
                break;
            default:
                msg_print("You love your weapon.");
                break;
            }
        }

        init = TRUE;
        last_spec = spec;
    }
}

static void _calc_stats(s16b stats[MAX_STATS])
{
    /* Note: _calc_stats() gets called before _calc_bonuses, so p_ptr->speciality_equip
       won't be set yet. I suppose we could take over setting this field, but I don't like
       relying on the non-obvious ordering of callbacks */
    if (p_ptr->psubclass == WEAPONMASTER_DAGGERS)
    {
        if (_check_speciality_equip())
        {
            switch (_get_toggle())
            {
            case TOGGLE_FLYING_DAGGER_STANCE:
                stats[A_CON] -= 4;
                break;
            }
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_DIGGERS)
    {
        if (_check_speciality_equip())
        {
            int pval = _max_pval();

            switch (_get_toggle())
            {
            case TOGGLE_STRENGTH_OF_THE_UNDERTAKER:
                stats[A_STR] += pval;
                break;
            case TOGGLE_STOICISM:
                stats[A_CON] += pval;
                break;
            }
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_SWORDS)
    {
        if (_check_speciality_equip())
        {
            switch (_get_toggle())
            {
            case TOGGLE_PATTERN_BLADE:
                stats[A_STR] += 3;
                stats[A_CON] += 3;
                break;
            case TOGGLE_HOLY_BLADE:
                stats[A_WIS] += 3;
                break;
            }
        }
    }
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (p_ptr->psubclass == WEAPONMASTER_DAGGERS)
    {
        if (p_ptr->speciality_equip)
        {
            if (p_ptr->lev >= 10) add_flag(flgs, OF_STEALTH);
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_SWORDS)
    {
        if (p_ptr->speciality_equip)
        {
            switch (_get_toggle())
            {
            case TOGGLE_SHARD_BLADE:
                if (p_ptr->lev >= 45)
                    add_flag(flgs, OF_VORPAL2);
                else
                    add_flag(flgs, OF_VORPAL);
                break;
            case TOGGLE_CHAOS_BLADE:
                add_flag(flgs, OF_BRAND_CHAOS);
                add_flag(flgs, OF_RES_CHAOS);
                break;
            case TOGGLE_DEATH_BLADE:
                add_flag(flgs, OF_BRAND_VAMP);
                add_flag(flgs, OF_RES_NETHER);
                if (p_ptr->lev >= 35) add_flag(flgs, OF_RES_POIS);
                if (p_ptr->lev >= 40) add_flag(flgs, OF_RES_DARK);
                add_flag(flgs, OF_HOLD_LIFE);
                break;
            case TOGGLE_DEMON_BLADE:
                add_flag(flgs, OF_BLOWS);
                add_flag(flgs, OF_SLAY_GOOD);
                add_flag(flgs, OF_DEC_STEALTH);
                if (p_ptr->lev >= 50)
                    add_flag(flgs, OF_BRAND_PLASMA);
                break;
            case TOGGLE_PATTERN_BLADE:
                add_flag(flgs, OF_SLAY_EVIL);
                add_flag(flgs, OF_SLAY_DEMON);
                add_flag(flgs, OF_SLAY_UNDEAD);
                add_flag(flgs, OF_FREE_ACT);
                add_flag(flgs, OF_SEE_INVIS);
                add_flag(flgs, OF_STR);
                add_flag(flgs, OF_CON);
                break;
            case TOGGLE_HOLY_BLADE:
                add_flag(flgs, OF_SLAY_EVIL);
                add_flag(flgs, OF_SLAY_DEMON);
                add_flag(flgs, OF_SLAY_UNDEAD);
                add_flag(flgs, OF_RES_FEAR);
                add_flag(flgs, OF_SEE_INVIS);
                add_flag(flgs, OF_WIS);
                if (p_ptr->lev >= 50)
                    add_flag(flgs, OF_BRAND_LITE);
                break;
            }
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_SHIELDS)
    {
        if (p_ptr->speciality_equip)
        {
            if (p_ptr->lev >= 45)
            {
                add_flag(flgs, OF_RES_ACID);
                add_flag(flgs, OF_RES_COLD);
                add_flag(flgs, OF_RES_FIRE);
                add_flag(flgs, OF_RES_ELEC);
                add_flag(flgs, OF_REFLECT);
            }
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_STAVES)
    {
        if (p_ptr->speciality_equip)
        {
            if (p_ptr->lev >= 10) add_flag(flgs, OF_AURA_REVENGE);
            if (p_ptr->lev >= 20) add_flag(flgs, OF_SPEED);
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_DIGGERS)
    {
        if (p_ptr->speciality_equip)
        {
            switch (_get_toggle())
            {
            case TOGGLE_STOICISM:
                add_flag(flgs, OF_STEALTH);
                break;
            case TOGGLE_INDUSTRIOUS_MORTICIAN:
                add_flag(flgs, OF_SPEED);
                break;
            }
        }
    }
}

static void _calc_shooter_bonuses(object_type *o_ptr, shooter_info_t *info_ptr)
{
    int spec = _check_speciality_aux(o_ptr);

    if (!spec) p_ptr->shooter_info.base_shot = 100;
    if (spec && !p_ptr->shooter_info.heavy_shoot)
    {
        p_ptr->shooter_info.to_d += p_ptr->lev/5;
        p_ptr->shooter_info.dis_to_d += p_ptr->lev/5;
        p_ptr->shooter_info.to_h += p_ptr->lev/5;
        p_ptr->shooter_info.dis_to_h += p_ptr->lev/5;

        if (p_ptr->psubclass == WEAPONMASTER_CROSSBOWS)
        {
            switch (_get_toggle())
            {
            case TOGGLE_RAPID_RELOAD:
                p_ptr->shooter_info.to_h -= 10;
                p_ptr->shooter_info.dis_to_h -= 10;
                p_ptr->shooter_info.base_shot += p_ptr->shooter_info.base_shot * (10 + p_ptr->lev/3) / 100;
                break;
            case TOGGLE_EXPLODING_BOLT:
                p_ptr->shooter_info.base_shot -= p_ptr->lev;
                break;
            case TOGGLE_OVERDRAW:
                p_ptr->shooter_info.to_mult += 100;
                p_ptr->shooter_info.to_h -= 20;
                p_ptr->shooter_info.dis_to_h -= 20;
                break;
            case TOGGLE_CAREFUL_AIM:
                p_ptr->shooter_info.to_h += 20;
                p_ptr->shooter_info.dis_to_h += 20;
                p_ptr->shooter_info.base_shot -= p_ptr->lev * 2;
                break;
            }
        }
        if (p_ptr->psubclass == WEAPONMASTER_BOWS && p_ptr->lev >= 20)
            p_ptr->shooter_info.breakage -= 10;
    }
}

static void _init_blows_calc(obj_ptr obj, plr_attack_info_ptr info)
{
    info->blows_calc.max = 100; /* Default for melee specialties with unfavored weapons */
    info->blows_calc.wgt = 70;
    info->blows_calc.mult = 50;
    switch (_specialities[p_ptr->psubclass].kind)
    {
    case _WEAPONMASTER_BOWS:
        info->blows_calc.max = 300;
        break;

    case _WEAPONMASTER_SHIELDS: /* Shieldmaster can bash or melee with aplomb */
        info->blows_calc.max = 500;
        break;

    case _WEAPONMASTER_MELEE:
        if (_check_speciality_aux(obj))
        {
            switch (p_ptr->psubclass)
            {
            case WEAPONMASTER_AXES:
                if (have_flag(info->paf_flags, PAF_TWO_HANDS))
                    info->blows_calc.max = 600;
                else
                    info->blows_calc.max = 500;
                break;
            case WEAPONMASTER_DAGGERS:
                if (_get_toggle() == TOGGLE_FRENZY_STANCE)
                    info->blows_calc.max = 600;
                else
                    info->blows_calc.max = 500;
                break;
            case WEAPONMASTER_CLUBS:
                info->blows_calc.max = 525;
                break;
            case WEAPONMASTER_POLEARMS:
                info->blows_calc.max = 525;
                break;
            case WEAPONMASTER_STAVES:
                info->blows_calc.max = 500;
                break;
            case WEAPONMASTER_SWORDS:
                info->blows_calc.max = 525;
                break;
            case WEAPONMASTER_DIGGERS:
                info->blows_calc.max = 550;
                break;
            }
        }
        break;
    }
}

static void _calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    int spec1 = _check_speciality_aux(obj);

    _init_blows_calc(obj, info);

    /* (+L/5, +L/5) for all weaponmasters replacing 'Signature Weapon' */
    if (spec1 && p_ptr->speciality_equip)
    {
        info->to_d += p_ptr->lev/5;
        info->dis_to_d += p_ptr->lev/5;
        info->to_h += p_ptr->lev/5;
        info->dis_to_h += p_ptr->lev/5;
    }

    if (p_ptr->psubclass == WEAPONMASTER_AXES)
    {
        if (spec1 && p_ptr->speciality_equip)
        {
            info->to_d += 5;
            info->dis_to_d += 5;

            if (p_ptr->lev >= 20)
            {
                info->to_d += 5;
                info->dis_to_d += 5;
            }

            if (p_ptr->lev >= 45)
            {
                info->to_d += 10;
                info->dis_to_d += 10;
            }

            switch (_get_toggle())
            {
            case TOGGLE_POWER_ATTACK:
                info->dis_to_h -= 2*p_ptr->lev/3;
                info->to_h -= 2*p_ptr->lev/3;
                if (plr_attack_info_two_hand_bonus(info))
                {
                    info->dis_to_d += p_ptr->lev/2;
                    info->to_d += p_ptr->lev/2;
                }
                else
                {
                    info->dis_to_d += p_ptr->lev/4;
                    info->to_d += p_ptr->lev/4;
                }
                break;
            }
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_DAGGERS)
    {
        switch (_get_toggle())
        {
        case TOGGLE_SHADOW_STANCE:
            info->to_d -= 10;
            info->dis_to_d -= 10;
            break;
        case TOGGLE_FRENZY_STANCE:
            info->to_h += p_ptr->lev/5;
            info->dis_to_h += p_ptr->lev/5;
            break;
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_CLUBS)
    {
        if (p_ptr->speciality_equip)
        {
            if (p_ptr->lev >= 25)
            {
                info->to_h += 20;
                info->dis_to_h += 20;
            }
        }

        switch (_get_toggle())
        {
        case TOGGLE_COMBAT_EXPERTISE:
            info->to_h -= 5 + p_ptr->lev/2;
            info->dis_to_h -= 5 + p_ptr->lev/2;
            break;
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_SHIELDS)
    {
    }
    else if (p_ptr->psubclass == WEAPONMASTER_SWORDS)
    {
        if (spec1 && p_ptr->speciality_equip)
        {
            info->to_h += 10;
            info->dis_to_h += 10;
            switch (_get_toggle())
            {
            case TOGGLE_DEMON_BLADE:
                info->xtra_blow += 100;
                break;
            case TOGGLE_ARMAGEDDON_BLADE:
                info->to_dd += 2;
                info->to_ds += 1;
                break;
            }
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_POLEARMS)
    {
        if (p_ptr->entrenched)
        {
            info->to_h += 20 + p_ptr->entrench_ct;
            info->dis_to_h += 20 + p_ptr->entrench_ct;
        }

        switch (_get_toggle())
        {
        case TOGGLE_MANY_STRIKE:
            info->to_h -= 5;
            info->dis_to_h -= 5;
            break;
        case TOGGLE_TRIP:
            info->to_h -= 30;
            info->dis_to_h -= 30;
            break;
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_STAVES)
    {
        if (spec1 && p_ptr->speciality_equip && p_ptr->lev >= 30 && p_ptr->chp == p_ptr->mhp)
            info->xtra_blow += 100;
    }
    else if (p_ptr->psubclass == WEAPONMASTER_DIGGERS)
    {
        if (spec1 && p_ptr->speciality_equip)
        {
            switch (_get_toggle())
            {
            case TOGGLE_INDUSTRIOUS_MORTICIAN:
                info->xtra_blow += 100;
                /*info->xtra_blow += MIN(obj->pval*50, 250);*/
                break;
            }
        }
    }
}

static void _move_monster(mon_ptr mon)
{
    if (p_ptr->psubclass == WEAPONMASTER_POLEARMS)
    {
        if (p_ptr->lev >= 20 && p_ptr->speciality_equip)
        {
            if (mon->cdis == 1)
            {
                plr_attack_t context = {0};
                char m_name[80];
                monster_desc(m_name, mon, 0);
                msg_format("%^s gets too close!", m_name);
                context.mode = _PROXIMITY_ALERT;
                context.flags = PAC_NO_INNATE;
                plr_attack(&context, mon->pos);
            }
        }
    }
}

static void _process_player(void)
{
    if (p_ptr->psubclass == WEAPONMASTER_POLEARMS)
    {
        /* process_player() fires before move_player() */
        if (p_ptr->pos.x == p_ptr->entrench_x && p_ptr->pos.y == p_ptr->entrench_y)
        {
            if (p_ptr->entrench_ct < 30)
                p_ptr->entrench_ct++;
            p_ptr->update |= PU_BONUS;
            p_ptr->redraw |= PR_EFFECTS;
        }
        else
        {
            p_ptr->entrench_x = p_ptr->pos.x;
            p_ptr->entrench_y = p_ptr->pos.y;
            p_ptr->entrench_ct = 0;
            p_ptr->update |= PU_BONUS;
            p_ptr->redraw |= PR_EFFECTS;
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_STAVES)
    {
        if (p_ptr->elaborate_defense)
        {
            p_ptr->elaborate_defense--;
            if (p_ptr->elaborate_defense <= 0)
            {
                p_ptr->update |= PU_BONUS;
                p_ptr->redraw |= PR_ARMOR;
            }
        }
        if (p_ptr->cloak_of_shadows)
        {
            p_ptr->cloak_of_shadows--;
            if (p_ptr->cloak_of_shadows <= 0)
            {
                p_ptr->update |= PU_BONUS;
                p_ptr->redraw |= PR_ARMOR;
            }
        }
    }
}

static obj_ptr _relocate_ammo(obj_loc_t loc)
{
    obj_ptr ammo = NULL;
    if (loc.where == INV_QUIVER && loc.v.slot)
        ammo = quiver_obj(loc.v.slot);
    else if (loc.where == INV_PACK && loc.v.slot)
        ammo = pack_obj(loc.v.slot);
    if (ammo && obj_can_shoot(ammo))
        return ammo;
    return NULL;
}

static obj_ptr _find_ammo(void)
{
    slot_t slot = quiver_find_first(obj_can_shoot);
    if (slot)
        return quiver_obj(slot);
    slot = pack_find_first(obj_can_shoot);
    if (slot)
        return pack_obj(slot);
    return NULL;
}

static void _move_player(void)
{
    if (_get_toggle() == TOGGLE_SHOT_ON_THE_RUN)
    {
        int num_shots = 1 + NUM_SHOTS / 400;
        obj_ptr bow = equip_obj(p_ptr->shooter_info.slot);
        obj_ptr ammo;
        int i;

        /* Paranoia:  Did the player remove their sling? */
        if (!_check_speciality_equip() || !bow)
        {
            _set_toggle(TOGGLE_NONE);
            return;
        }

        ammo = _relocate_ammo(shoot_item);
        if (!ammo)
            ammo = _find_ammo();
        if (!ammo)
        {
            msg_print("You are out of ammo.");
            _set_toggle(TOGGLE_NONE);
            return;
        }

        for (i = 0; i < num_shots; i++)
        {
            mon_ptr tgt_mon;
            /* End the technique when the ammo runs out.  Note that "return ammo"
               might not consume the current shot. Note that we will intentionally spill
               over into the next stack of shots, provided they are legal for this shooter. 
            if (shoot_item != INVEN_UNLIMITED_QUIVER)??? */
            if (!ammo->number)
            {
                obj_release(ammo, 0);
                ammo = _find_ammo();
                if (!ammo)
                {
                    msg_print("Your ammo has run out. Time to reload!");
                    _set_toggle(TOGGLE_NONE);
                    return;
                }
            }

            /* Pick a target to blast */
            tgt_mon = _get_nearest_target_los();
            if (tgt_mon)
            {
                shoot_hack = SHOOT_RUN;
                do_cmd_fire_aux2(bow, ammo, p_ptr->pos.x, p_ptr->pos.y, tgt_mon->pos.x, tgt_mon->pos.y);
                shoot_hack = SHOOT_NONE;
            }
        }
        obj_release(ammo, OBJ_RELEASE_QUIET);
    }
    if (p_ptr->psubclass == WEAPONMASTER_POLEARMS)
    {
        int y, x;
        if (one_in_(5) && random_opponent(&y, &x))
        {
            plr_attack_t context = {0};
            context.mode = _AUTO_BLOW;
            context.flags = PAC_NO_INNATE;
            plr_attack(&context, point_create(x, y));
            energy_use = 0;
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_STAVES)
    {
        if (p_ptr->speciality_equip && p_ptr->lev >= 5)
        {
            if (!p_ptr->cloak_of_shadows)
            {
                p_ptr->update |= PU_BONUS;
                p_ptr->redraw |= PR_ARMOR;
            }
            p_ptr->cloak_of_shadows = 1;
        }
    }
    else if (p_ptr->psubclass == WEAPONMASTER_DIGGERS)
    {
        if (p_ptr->speciality_equip && p_ptr->lev >= 15)
            p_ptr->update |= PU_BONUS;
    }
}

static void _character_dump(doc_ptr doc)
{
    doc_printf(doc, "<topic:Abilities>================================== <color:keypress>A</color>bilities ==================================\n\n");

    if (p_ptr->psubclass == WEAPONMASTER_AXES)
    {
        if (p_ptr->lev >= 45)
            doc_printf(doc, "  * You gain a huge bonus to damage when wielding an axe.\n");
        else if (p_ptr->lev >= 20)
            doc_printf(doc, "  * You gain a large bonus to damage when wielding an axe.\n");
        else
            doc_printf(doc, "  * You gain a small bonus to damage when wielding an axe.\n");

        doc_printf(doc, "  * You gain +1 max attack when wielding an axe with two hands.\n");

        if (p_ptr->lev >= 5)
            doc_printf(doc, "  * You gain a bonus to tunneling when wielding an axe.\n");

        if (p_ptr->lev >= 30)
            doc_printf(doc, "  * <indent>You occasionally attack an adjacent opponent after killing a foe when wielding an axe.</indent>\n");
    }
    else if (p_ptr->psubclass == WEAPONMASTER_CLUBS)
    {
        doc_printf(doc, "  * Your attacks have a chance to confuse when wielding a club.\n");

        if (p_ptr->lev >= 20)
            doc_printf(doc, "  * Your attacks have a chance to knock out when wielding a club.\n");

        if (p_ptr->lev >= 45)
            doc_printf(doc, "  * Your attacks have a chance to stun when wielding a club.\n");

        if (p_ptr->lev >= 25)
            doc_printf(doc, "  * You gain a bonus to hit when wielding a club.\n");

        if (p_ptr->lev >= 40)
            doc_printf(doc, "  * You gain crushing blows when wielding a club.\n");
    }
    else if (p_ptr->psubclass == WEAPONMASTER_DAGGERS)
    {
        doc_printf(doc, "  * You pay reduced energy costs when equipping a dagger.\n");
        doc_printf(doc, "  * You dual wield very effectively with daggers.\n");
        if (p_ptr->lev >= 25)
            doc_printf(doc, "  * You gain a bonus to AC when wielding dual wielding daggers.\n");
        if (p_ptr->lev >= 12)
            doc_printf(doc, "  * You gain a bonus to stealth when wielding a dagger.\n");
        if (p_ptr->lev >= 30)
            doc_printf(doc, "  * You gain sneak attack and backstab when wielding a dagger.\n");
    }
    else if (p_ptr->psubclass == WEAPONMASTER_DIGGERS)
    {
        doc_printf(doc, "  * You gain a bonus to tunneling when wielding a digger.\n");
        if (p_ptr->lev >= 45)
            doc_printf(doc, "  * Your steps break walls when wielding a digger.\n");

        if (p_ptr->lev >= 15)
            doc_printf(doc, "  * <indent>You gain an AC bonus depending on the number of adjacent walls when wielding a digger.</indent>\n");
    }
    else if (p_ptr->psubclass == WEAPONMASTER_POLEARMS)
    {
        doc_printf(doc, "  * You occasionally get a free round of attacks after moving when wielding a polearm.\n");
        if (p_ptr->lev >= 20)
            doc_printf(doc, "  * You automatically attack any enemy that steps next to you when wielding a polearm.\n");
        if (p_ptr->lev >= 45)
            doc_printf(doc, "  * You occasionally strike all adjacent foes when wielding a polearm.\n");

        if (p_ptr->lev >= 30)
            doc_printf(doc, "  * <indent>You gain a bonus to hit and to AC when you don't move for 3 rounds when wielding a polearm.</indent>\n");
    }
    else if (p_ptr->psubclass == WEAPONMASTER_SLINGS)
    {
        doc_printf(doc, "  * Your ammo often returns to you when wielding a sling.\n");
        if (p_ptr->lev >= 20)
            doc_printf(doc, "  * Your ammo gains extra damage dice when wielding a sling.\n");

        if (p_ptr->lev >= 10)
            doc_printf(doc, "  * <indent>Your shots never miss your target once you score 3 consecutive hits when wielding a sling.</indent>\n");
        if (p_ptr->lev >= 40)
            doc_printf(doc, "  * You gain extra shots when wielding a sling.\n");
    }
    else if (p_ptr->psubclass == WEAPONMASTER_SHIELDS)
    {
        doc_printf(doc, "  * You gain two handed wielding bonuses even when wielding a shield.\n");
        if (p_ptr->lev >= 20)
            doc_printf(doc, "  * <indent>Your inventory items are somewhat protected from destruction when wielding a shield.</indent>\n");
        if (p_ptr->lev >= 45)
            doc_printf(doc, "  * You gain basic resistance and reflection when wielding a shield.\n");

        if (p_ptr->lev >= 5)
            doc_printf(doc, "  * You gain double the AC benefit when wielding a shield.\n");

        if (p_ptr->lev >= 25)
            doc_printf(doc, "  * You gain a bonus to saving throws when wielding a shield.\n");

    }
    else if (p_ptr->psubclass == WEAPONMASTER_STAVES)
    {
        doc_printf(doc, "  * <indent>You gain a bonus to AC until your next turn after any successful hit when wielding a staff.</indent>\n");
        doc_printf(doc, "  * You suffer a penalty to speed when wielding a shield.\n");
        if (p_ptr->lev >= 5)
            doc_printf(doc, "  * <indent>You gain a bonus AC after moving until your next turn when wielding a staff.</indent>\n");
        if (p_ptr->lev >= 10)
            doc_printf(doc, "  * You retaliate when struck when wielding a staff.\n");
        if (p_ptr->lev >= 20)
            doc_printf(doc, "  * You gain a bonus to speed when wielding a staff.\n");
        if (p_ptr->lev >= 30)
            doc_printf(doc, "  * <indent>You gain an extra attack when you are at full health and wielding a staff.</indent>\n");
        if (p_ptr->lev >= 35)
            doc_printf(doc, "  * You are unaffected by monster auras when wielding a staff.\n");
    }
    else if (p_ptr->psubclass == WEAPONMASTER_SWORDS)
    {
        doc_printf(doc, "  * You gain a bonus to hit when wielding a sword.\n");
        if (p_ptr->lev >= 20)
            doc_printf(doc, "  * You gain constant heroism when wielding a sword.\n");
    }

    doc_newline(doc);

    {
        spell_info spells[MAX_SPELLS];
        int        ct = _get_spells_aux(spells, MAX_SPELLS);

        if (ct)
            plr_display_spells(doc, spells, ct);
    }
}
static status_display_t _status_display(void)
{
    status_display_t d = {0};
    switch (_get_toggle())
    {
    case TOGGLE_SHOT_ON_THE_RUN:
        d.name = "Shoot on Run"; d.abbrev = "Rn"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_RAPID_SHOT:
        d.name = "Rapid Shot"; d.abbrev = "Rp"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_FLYING_DAGGER_STANCE:
        d.name = "Flying Dagger"; d.abbrev = "FD"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_SHADOW_STANCE:
        d.name = "Shadow"; d.abbrev = "Sw"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_FRENZY_STANCE:
        d.name = "Frenzy"; d.abbrev = "Fz"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_COMBAT_EXPERTISE:
        d.name = "Defensive Stance"; d.abbrev = "DS"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_TRADE_BLOWS:
        d.name = "Trade Blows"; d.abbrev = "Tr"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_POWER_ATTACK:
        d.name = "Power Attack"; d.abbrev = "Pw"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_BURNING_BLADE:
        d.name = "Burning"; d.abbrev = "/Fi"; d.color = TERM_RED;
        break;
    case TOGGLE_ICE_BLADE:
        d.name = "Ice"; d.abbrev = "/Co"; d.color = TERM_BLUE;
        break;
    case TOGGLE_THUNDER_BLADE:
        d.name = "Thunder"; d.abbrev = "/El"; d.color = TERM_YELLOW;
        break;
    case TOGGLE_SHARD_BLADE:
        d.name = "Shard"; d.abbrev = "/V"; d.color = TERM_L_UMBER;
        break;
    case TOGGLE_CHAOS_BLADE:
        d.name = "Chaos"; d.abbrev = "/Ca"; d.color = TERM_VIOLET;
        break;
    case TOGGLE_DEATH_BLADE:
        d.name = "Death"; d.abbrev = "/V"; d.color = TERM_L_DARK;
        break;
    case TOGGLE_DEMON_BLADE:
        d.name = "Demon"; d.abbrev = "/A"; d.color = TERM_RED;
        break;
    case TOGGLE_PATTERN_BLADE:
        d.name = "Pattern"; d.abbrev = "/*"; d.color = TERM_ORANGE;
        break;
    case TOGGLE_HOLY_BLADE:
        d.name = "Holy"; d.abbrev = "/*"; d.color = TERM_WHITE;
        break;
    case TOGGLE_ARMAGEDDON_BLADE:
        d.name = "Armageddon"; d.abbrev = "Am"; d.color = TERM_VIOLET;
        break;
    case TOGGLE_MANY_STRIKE:
        d.name = "Many Strike"; d.abbrev = "MS"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_PIERCING_STRIKE:
        d.name = "Piercing Strike"; d.abbrev = "PS"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_TRIP:
        d.name = "Trip"; d.abbrev = "Trp"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_STRENGTH_OF_THE_UNDERTAKER:
        d.name = "Undertaker"; d.abbrev = "Str"; d.color = TERM_UMBER;
        break;
    case TOGGLE_STOICISM:
        d.name = "Stoicism"; d.abbrev = "Sc"; d.color = TERM_ORANGE;
        break;
    case TOGGLE_INDUSTRIOUS_MORTICIAN:
        d.name = "Mortician"; d.abbrev = "At"; d.color = TERM_YELLOW;
        break;
    case TOGGLE_SHIELD_BASH:
        d.name = "Shield Bash"; d.abbrev = "SB"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_BULWARK:
        d.name = "Bulwark"; d.abbrev = "Bw"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_SHIELD_REVENGE:
        /* already using T_REVENGE */
        break;
    case TOGGLE_READIED_SHOT:
        d.name = "Ready"; d.abbrev = "RS"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_PIERCING_ARROW:
        d.name = "Pierce"; d.abbrev = "PA"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_RAPID_RELOAD:
        d.name = "Reload"; d.abbrev = "RR"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_EXPLODING_BOLT:
        d.name = "Explode"; d.abbrev = "Ex"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_OVERDRAW:
        d.name = "Overdraw"; d.abbrev = "OD"; d.color = TERM_L_BLUE;
        break;
    case TOGGLE_CAREFUL_AIM:
        d.name = "Aim"; d.abbrev = "Aim"; d.color = TERM_L_BLUE;
        break;
    }
    return d;
}
static void _prt_effects(doc_ptr doc)
{
    if (p_ptr->entrenched)
        doc_insert(doc, "<color:u>Entrenched</color>\n");
}

plr_class_ptr weaponmaster_get_class(int subclass)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {
        me = plr_class_alloc(CLASS_WEAPONMASTER);
        me->name = "Weaponmaster";
        me->desc = "The weaponmaster is great with a single class of weapons. "
                  "The character gets combat bonuses and special powers "
                  "depending on the type of specialization. Alas, the "
                  "weaponmaster is truly lousy when using any weapon "
                  "outside their chosen specialty so focus is key.";

        me->life = 109;
        me->base_hp = 12;
        me->exp = 135;
        me->pets = 40;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_STRONG;

        me->hooks.attack_init = _attack_init;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_spells = _get_spells;
        me->hooks.birth = _on_birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_stats = _calc_stats;
        me->hooks.get_flags = _get_flags;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.calc_shooter_bonuses = _calc_shooter_bonuses;
        me->hooks.move_player = _move_player;
        me->hooks.move_monster = _move_monster;
        me->hooks.process_player = _process_player;
        me->hooks.character_dump = _character_dump;
        me->hooks.status_display = _status_display;
        me->hooks.prt_effects = _prt_effects;
        me->hooks.register_timers = _register_timers;
    }
    {
        me->stats[A_STR] =  1;
        me->stats[A_INT] = -1;
        me->stats[A_WIS] = -1;
        me->stats[A_DEX] =  1;
        me->stats[A_CON] =  1;
        me->stats[A_CHR] =  1;
    }
    if (0 <= subclass && subclass < WEAPONMASTER_MAX)
    {
        _speciality *ptr = &_specialities[subclass];
        int          i;

        for (i = 0; i < MAX_STATS; i++)
            me->stats[i] += ptr->stats[i];

        me->skills = ptr->base_skills;
        me->extra_skills = ptr->extra_skills;

        me->subname = ptr->name;
        me->subdesc = ptr->help;
    }
    else
    {
        me->subname = "";
        me->subdesc = "";
    }

    return me;
}

int weaponmaster_wield_hack(object_type *o_ptr)
{
    int result = 100;

    if (p_ptr->pclass == CLASS_WEAPONMASTER)
    {
        if (p_ptr->psubclass == WEAPONMASTER_DAGGERS)
        {
            if (_check_speciality_aux(o_ptr))
                result = 50 - p_ptr->lev/2;
        }
    }
    return result;
}

cptr weaponmaster_speciality_name(int psubclass)
{
    /* assert(p_ptr->pclass == CLASS_WEAPONMASTER); */
    return _specialities[psubclass].name;
}

void weaponmaster_do_readied_shot(monster_type *m_ptr)
{
    if (weaponmaster_get_toggle() == TOGGLE_READIED_SHOT)
    {
        int tx = m_ptr->pos.x;
        int ty = m_ptr->pos.y;
        obj_ptr bow = equip_obj(p_ptr->shooter_info.slot);

        if (bow)
        {
            obj_ptr ammo = _relocate_ammo(shoot_item);
            /* Probably, the player should ready another? Also, 
             * do_cmd_fire should clear the toggle? There is no
             * game concept of a "loaded arrow", but ideally, the
             * object would be copied out at time of casting and
             * stored someplace else ... then do_cmd_fire would simply
             * use the loaded arrow (rather than prompting) with
             * less energy ... but that seems too complicated to implement.
            if (!ammo)
                ammo = _find_ammo();*/
            if (!ammo)
            {
                msg_print("The ammo you readied no longer exists!");
                _set_toggle(TOGGLE_NONE);
                return;
            }
            shoot_hack = SHOOT_RETALIATE;
            do_cmd_fire_aux2(bow, ammo, p_ptr->pos.x, p_ptr->pos.y, tx, ty);
            obj_release(ammo, 0);
            shoot_hack = SHOOT_NONE;
        }
        /* Force the player to ready another arrow ... */
        _set_toggle(TOGGLE_NONE);
    }
}
