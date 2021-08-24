#include "angband.h"

#include <assert.h>

/* call graph */
extern bool plr_shoot_aux(plr_shoot_ptr context);
  static bool _begin(plr_shoot_ptr context);
    static bool _prompt_bow(plr_shoot_ptr context);
    static void _init_bow(plr_shoot_ptr context);
    static bool _begin_hook(plr_shoot_ptr context);
    static bool _prompt_ammo(plr_shoot_ptr context);
    static bool _prompt_target(plr_shoot_ptr context);
    static bool _begin_bow_hook(plr_shoot_ptr context);
    static void _init_ammo(plr_shoot_ptr context);
    static void _init_target(plr_shoot_ptr context);
  static void _shoot(plr_shoot_ptr context);
    static void _shoot_aux(plr_shoot_ptr context);
      static void _hit_mon(plr_shoot_ptr context, mon_ptr mon);
        static void _apply_vorpal(plr_shoot_ptr context, mon_ptr mon);
        static void _apply_crit(plr_shoot_ptr context, mon_ptr mon);
        static void _apply_slays(plr_shoot_ptr context, mon_ptr mon);
        static bool _hack_cupid(plr_shoot_ptr context, mon_ptr mon);
  static void _end(plr_shoot_ptr context); 

/* helpers */
static int _num_fire(plr_shoot_ptr context)
{
    return context->info.base_shot + context->info.xtra_shot;
}
static int _energy(plr_shoot_ptr context)
{
    if (context->energy) return context->energy;
    return bow_energy(context->bow->sval)/_num_fire(context);
}

/* public */
void plr_shoot_info_wipe(plr_shoot_info_ptr info)
{
    memset(info, 0, sizeof(plr_shoot_info_t));
    info->base_shot = 100;
    info->breakage = 100;
    info->crit.qual_mul = 100;
    info->crit.freq_mul = 100;
}
bool plr_shoot(void)
{
    plr_shoot_t context = {0};
    return plr_shoot_aux(&context);
}
bool plr_shoot_special(int type, u32b flags)
{
    plr_shoot_t context = {0};
    context.mode = type;
    context.flags = flags;
    return plr_shoot_aux(&context);
}
bool plr_shoot_aux(plr_shoot_ptr context)
{
    if (!_begin(context)) return FALSE; /* allow UI cancel (or invalid bow, lack of matching ammo, etc. ) */

    if (fear_allow_shoot())
        _shoot(context);
    else
        msg_print("You are too scared!");

    _end(context);
    if (!(context->flags & PSC_NO_ENERGY))
        energy_use = _energy(context);

    return TRUE;
}

/* hooks */
static bool _begin_hook(plr_shoot_ptr context)
{
    if (!context->hooks.begin_f) return TRUE;
    return context->hooks.begin_f(context);
}
static bool _begin_bow_hook(plr_shoot_ptr context)
{
    if (!context->hooks.begin_bow_f) return TRUE;
    return context->hooks.begin_bow_f(context);
}
static void _plr_custom_init(plr_shoot_ptr context)
{
    plr_race_ptr r = plr_race();
    plr_class_ptr c = plr_class();
    if (r->hooks.shoot_init) r->hooks.shoot_init(context);
    if (c->hooks.shoot_init) c->hooks.shoot_init(context);
}
/* begin */
static bool _begin(plr_shoot_ptr context)
{
    if (!_prompt_bow(context)) return FALSE;
    _init_bow(context);                        /* XXX copy plr->shooter_info to info; set range */
    _plr_custom_init(context); /* give race/class a chance to install a begin_f! */
    if (!_begin_hook(context)) return FALSE;   /* XXX override info, range; set ammo, target, etc. */
    if (!_prompt_ammo(context)) return FALSE;  /* XXX ui if !ammo */
    if (!_prompt_target(context)) return FALSE;/* XXX ui if !target */

    _init_ammo(context);                       /* XXX use info.flags to compute ammo_flags */
    _init_target(context);                     /* XXX compute path */
    if (!_begin_bow_hook(context)) return FALSE;/* XXX override info.ammo_flags; adjust skill */
    return TRUE;
}
static obj_ptr _current_bow(void)
{
    int slot = equip_find_first(obj_is_bow);
    if (slot)
        return equip_obj(slot);
    return NULL;
}
static bool _prompt_bow(plr_shoot_ptr context)
{
    context->bow = _current_bow();
    if (!context->bow)
    {
        msg_print("You have nothing to fire with.");
        flush();
        return FALSE;
    }
    if (prace_is_(MIMIC_MIST))
    {
        msg_print("You cannot shoot while incorporeal.");
        flush();
        return FALSE;
    }
    if (context->bow->sval == SV_CRIMSON || context->bow->sval == SV_RAILGUN)
    {
        msg_print("You should activate your Gun instead.");
        flush();
        return FALSE;
    }
    if (context->bow->sval == SV_HARP)
    {
        msg_print("You play a soothing melody, but not much else happens.");
        flush();
        return FALSE;
    }
    if (plr->shooter_info.base_shot + plr->shooter_info.xtra_shot <= 0) /* paranoia */
    {
        msg_print("You are unable to shoot that bow.");
        flush();
        return FALSE;
    }
    return TRUE;
}
static void _init_bow(plr_shoot_ptr context)
{
    context->info = plr->shooter_info;
    context->range = bow_range(context->bow);
    context->mult = bow_mult(context->bow);
    if (context->flags & PSC_DISPLAY)
        obj_flags_known(context->bow, context->bow_flags);
    else
        obj_flags(context->bow, context->bow_flags);
}
static bool _prompt_ammo(plr_shoot_ptr context)
{
    obj_prompt_t prompt = {0};

    if (context->ammo) return TRUE;

    prompt.prompt = "Shoot which item?";
    prompt.error = "You have nothing to shoot.";
    prompt.filter = obj_can_shoot;
    prompt.where[0] = INV_QUIVER;
    prompt.where[1] = INV_PACK;
    prompt.where[2] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    context->ammo = prompt.obj;
    return TRUE;
}
static bool _prompt_target(plr_shoot_ptr context)
{
    if (dun_pos_interior(cave, context->target)) return TRUE;
    if (context->hooks.target_f)
    {
        if (!context->hooks.target_f(context)) return FALSE; /* user cancel */
        if (dun_pos_interior(cave, context->target)) return TRUE; /* hook worked */
        /* vvv hook wants normal target selection code vvv */
    }
    context->target = plr_get_target_aux(GF_ARROW, context->range);
    return dun_pos_interior(cave, context->target);
}
static void _init_ammo(plr_shoot_ptr context)
{
    u32b flags[OF_ARRAY_SIZE];
    int i;
    int to_h = context->info.to_h + context->bow->to_h + context->ammo->to_h + context->to_h;
    int stun = MIN(100, plr_tim_amount(T_STUN));

    context->skill = plr->skills.thb + to_h * BTH_PLUS_ADJ;
    if (stun)
        context->skill -= context->skill * stun / 150;

    object_desc(context->ammo_desc, context->ammo, OD_OMIT_PREFIX | OD_NO_PLURAL | OD_OMIT_INSCRIPTION | OD_NAME_ONLY | OD_COLOR_CODED);

    if (context->flags & PSC_DISPLAY)
    {
        obj_flags_known(context->ammo, flags);
        for (i = 0; i < OF_ARRAY_SIZE; i++)
            context->ammo_flags[i] = context->bow_flags[i] | flags[i] | context->info.known_flags[i];
    }
    else
    {
        obj_flags(context->ammo, flags);
        for (i = 0; i < OF_ARRAY_SIZE; i++)
            context->ammo_flags[i] = context->bow_flags[i] | flags[i] | context->info.flags[i];
    }
}
static void _init_target(plr_shoot_ptr context)
{
    context->path = dun_path_alloc_aux(cave, plr->pos, context->target,
        context->path_flags | PROJECT_THRU, context->range);
    context->path_pos = 0;
}
/* shoot */
static void _shoot(plr_shoot_ptr context)
{
    int i, ct = 1;

    if (context->hooks.get_shots_f)
        ct = context->hooks.get_shots_f(context);

    for (i = 0; i < ct; i++)
    {
        if (!context->ammo->number)
        {
            msg_print("Your ammo has run out. Time to reload!");
            break;
        }
        _shoot_aux(context);
        context->shots++; /* XXX _shoot_aux may bounce, which should not count as an extra shot */
        stats_on_use(context->ammo, 1);
    }
    obj_release(context->ammo, OBJ_RELEASE_QUIET);
}
static bool _return_ammo(plr_shoot_ptr context)
{
    if (context->flags & PSC_BOUNCE) return FALSE; 
    if (plr->return_ammo || context->ammo->name2 == EGO_AMMO_RETURNING)
        return _1d(100) <= 50 + plr->lev/2;
    return FALSE;
}
static void _animate(plr_shoot_ptr context, point_t pos)
{
    point_t uip = cave_pt_to_ui_pt(pos);
    rect_t  map = ui_map_rect();

    if (rect_contains_point(map, uip) && plr_view(pos))
    {
        if (!msg_line_contains(uip.y, uip.x))
        {
            obj_kind_ptr k = &k_info[context->ammo->k_idx]; 
            Term_queue_bigchar(uip.x, uip.y, k->x_attr, k->x_char, 0, 0);
        }
        Term_gotoxy(uip.x, uip.y);
        Term_fresh();
        Term_xtra(TERM_XTRA_DELAY, delay_animation);
        draw_pos(pos);
    }
    else
    {
        Term_xtra(TERM_XTRA_DELAY, delay_animation);
    }
}
static int _breakage_chance(plr_shoot_ptr context)
{
    int chance = breakage_chance(context->ammo);
    if (context->info.breakage >= 0)
        chance = chance * context->info.breakage / 100;
    return chance;
}
static bool _breakage(plr_shoot_ptr context)
{
    int chance;

    /* defense: prevent accidental destruction of artifact|endurance ammo.
     * client hooks might set AMMO_BREAK w/o checking (cf the Sniper) */
    if (obj_is_art(context->ammo)) return FALSE;
    if (context->ammo->name2 == EGO_AMMO_ENDURANCE) return FALSE;

    if (context->action == AMMO_BREAK) return TRUE;
    if (context->action == AMMO_PIERCE) return TRUE;
    if (!context->hit_body) return FALSE;

    chance = _breakage_chance(context);
    return randint0(100) < chance;
}
static void _shoot_aux(plr_shoot_ptr context)
{
    point_t last = context->path->start;
    int rc;

    context->action = AMMO_DROP;
    if (_return_ammo(context))
        context->action = AMMO_RETURN;
    if (context->mode == PLR_SHOOT_PIERCE)
        context->action = AMMO_PIERCE;
    if (context->ammo->name2 == EGO_AMMO_EXPLODING || context->mode == PLR_SHOOT_EXPLODE)
        context->action = AMMO_EXPLODE;

    context->hit_body = FALSE;
    context->pierce = 0;

    /* project along the path */
    if (context->hooks.begin_path_f)
        context->hooks.begin_path_f(context);
    for (context->path_pos = 0; context->path_pos < context->path->count; context->path_pos++)
    {
        point_t pos = context->path->points[context->path_pos];
        dun_grid_ptr grid;
        mon_ptr mon;

        _animate(context, pos);

        /* pre-hook to alter terrain (disintegration arrows) */
        if (context->hooks.prepath_f)
        {
            rc = context->hooks.prepath_f(context, pos);
            if (rc == PATH_STOP) break;
            if (rc == PATH_SKIP) continue;
        }

        mon = dun_mon_at(cave, pos);
        grid = dun_grid_at(cave, pos);

        /* stop on walls */
        if (cell_project(grid))
        {
            last = pos; /* drop *before* walls */
            if (floor_has_illusion(grid) && one_in_(2))
                dun_tim_remove_at(cave, pos, DT_ILLUSION);
        }
        else if (!mon)
        {
            context->hit_body = TRUE;
            break;
        }

        /* hook to alter terrain (light arrow; kill traps; etc.) */
        if (context->hooks.path_f)
        {
            rc = context->hooks.path_f(context, pos);
            if (rc == PATH_STOP) break;
            if (rc == PATH_SKIP) continue;
        }

        /* hit a monster */
        if (mon)
        {
            _hit_mon(context, mon);
            if (!cell_project(grid)) break; /* walls block PIERCE */
            if (context->action != AMMO_PIERCE) break; /* monsters block non-PIERCE */
            context->skill -= 20*BTH_PLUS_ADJ;
            context->pierce++;
            if (context->pierce > 4) break;
        }
    }
    if (context->hooks.end_path_f)
        context->hooks.end_path_f(context);

    /* only bounce once */
    if (context->action == AMMO_BOUNCE && (context->flags & PSC_BOUNCE))
        context->action = AMMO_DROP;

    /* only explode on impact */
    if (context->action == AMMO_EXPLODE && !context->hit_body)
        context->action = AMMO_DROP;

    /* break/drop/explode/return/bounce ammo */
    if (context->action == AMMO_BOUNCE)
    {
        dun_path_ptr old_path = context->path;
        point_t      old_target = context->target;

        context->flags |= PSC_BOUNCE;
        context->target = dun_scatter(cave, last, 4);
        context->skill -= 20*BTH_PLUS_ADJ;
        context->path = dun_path_alloc_aux(cave, last, context->target,
            context->path_flags | PROJECT_THRU, context->range);
        context->path_pos = 0;
        _shoot_aux(context); /* recurse ... this will drop/break ammo */
        context->flags &= ~PSC_BOUNCE;

        dun_path_free(context->path);
        context->path = old_path;
        context->target = old_target;
    }
    else if (context->action == AMMO_RETURN)
    {
        int i;
        msg_format("The %s returns to your pack.", context->ammo_desc);
        for (i = context->path_pos; i >= 0; i--)
        {
            point_t pos = context->path->points[i];
            _animate(context, pos);
        }
    }
    else if (context->action == AMMO_EXPLODE)
    {
        context->ammo->number--;
        cmsg_format(TERM_L_RED, "The %s explodes.", context->ammo_desc);
        if (!context->dam) /* hit wall */
            context->dam = damroll(context->ammo->dd, context->ammo->ds) + context->ammo->to_d;
        if (context->hooks.explode_f)
            context->hooks.explode_f(context, last);
        else
            plr_ball_direct(3, last, GF_MISSILE, context->dam);
        stats_on_p_destroy(context->ammo, 1);
    }
    else if (_breakage(context))
    {
        context->ammo->number--;
        cmsg_format(TERM_L_RED, "The %s disappears.", context->ammo_desc);
        stats_on_p_destroy(context->ammo, 1);
    }
    else
    {
        obj_t ammo = *context->ammo;
        ammo.number = 1;
        dun_drop_near(cave, &ammo, last);
        context->ammo->number--;
        plr->update |= PU_BONUS; /* Weight changed */
    }
}
static void _hit_msg(plr_shoot_ptr context, mon_ptr mon)
{
    if (!mon->ml)
        msg_format("The %s finds a mark.", context->ammo_desc);
    else
    {
        char m_name[80];
        monster_desc(m_name, mon, 0);
        if (context->shoot_sleeping)
            cmsg_format(TERM_VIOLET, "You cruelly shoot %s!", m_name);
        else if (context->shoot_fleeing)
            cmsg_format(TERM_VIOLET, "You shoot %s in the back!", m_name);
        else
            msg_format("The %s hits %s.", context->ammo_desc, m_name);

        if (mon->ml)
        {
            if (!plr_tim_find(T_HALLUCINATE)) mon_track(mon);
            health_track(mon);
        }
    }
}
static int _shoot_sleeping(plr_shoot_ptr context)
{
    if (plr_tim_find(T_BLIND)) return 0;
    if (plr_tim_find(T_CONFUSED)) return 0;
    if (plr_tim_find(T_HALLUCINATE)) return 0;
    if (randint0(100) < plr_tim_amount(T_STUN)) return 0;
    if (context->mode == PLR_SHOOT_AMBUSH) return 300;
    return plr->shoot_sleeping;
}
static int _shoot_fleeing(plr_shoot_ptr context)
{
    if (plr_tim_find(T_BLIND)) return 0;
    if (plr_tim_find(T_CONFUSED)) return 0;
    if (plr_tim_find(T_HALLUCINATE)) return 0;
    if (randint0(100) < plr_tim_amount(T_STUN)) return 0;
    return plr->shoot_fleeing;
}
static void _hit_mon(plr_shoot_ptr context, mon_ptr mon)
{
    mon_race_ptr race = mon->race;
    bool hit = FALSE;
    int stun = MIN(100, plr_tim_amount(T_STUN));
    int skill = context->skill - context->path_pos;

    /* virtues and proficiency */
    if (mon_tim_find(mon, MT_SLEEP))
    {
        if (!mon_is_evil(mon) || one_in_(5)) virtue_add(VIRTUE_COMPASSION, -1);
        if (!mon_is_evil(mon) || one_in_(5)) virtue_add(VIRTUE_HONOUR, -1);
    }
    skills_bow_gain(context->bow->sval, race->alloc.lvl);
    if (plr->riding) skills_riding_gain_archery(race);

    /* stealth techniques scale base damage and give better accuracy */
    context->shoot_sleeping = context->shoot_fleeing = 0;
    if (mon_tim_find(mon, MT_SLEEP) && mon->ml)
    {
        context->shoot_sleeping = _shoot_sleeping(context);
        if (context->shoot_sleeping)
            skill = skill * 3;
    }
    else if (mon_tim_find(mon, T_FEAR) && mon->ml)
    {
        context->shoot_fleeing = _shoot_fleeing(context);
        if (context->shoot_fleeing)
            skill = skill * 3 / 2;
    }

    /* test for hit */
    if (context->hooks.check_hit_f)
        hit = context->hooks.check_hit_f(context, mon);
    else if (!hit) /* XXX melee ambush always hits ... but this is a distance attack */
        hit = test_hit_fire(skill, mon_ac(mon), mon->ml);

    if (hit)
    {
        dice_t dice = {0};

        _hit_msg(context, mon);
        context->hit_body = TRUE;
        context->hits++;

        dice.dd = context->ammo->dd + context->info.to_dd;
        dice.ds = context->ammo->ds + context->info.to_ds;
        if (stun)
            dice.scale = 1000 - 1000 * stun / 150;

        /* calculate base damage */
        context->dam_base = dice_roll(dice);  /* XdY */
        _apply_vorpal(context, mon);          /* scaled by vorpals */
        _apply_crit(context, mon);            /* scaled by crits */
        if (context->shoot_sleeping)          /* scaled by ambushes on sleeping monsters */
            context->dam_base = context->dam_base * context->shoot_sleeping / 100;
        if (context->shoot_fleeing)           /* scaled by shooting fleeing monsters in the back */
            context->dam_base = context->dam_base * context->shoot_fleeing / 100;

        /* calculate damage */
        context->dam = context->dam_base; /* begin with base damage */
        if (!(context->flags & PSC_NO_SLAY))
            _apply_slays(context, mon);  /* scale for slays */
        context->dam += context->ammo->to_d + context->to_d; /* add in pre-mult bonuses */
        if (context->hooks.mod_dam1_f)    /* apply custom hook */
            context->hooks.mod_dam1_f(context, mon);
        context->dam = context->dam * context->mult / 100; /* apply bow multiplier */
        context->dam += context->bow->to_d + context->info.to_d; /* add in post-mult bonuses */
        if (context->hooks.mod_dam2_f)    /* apply custom hook */
            context->hooks.mod_dam2_f(context, mon);

        context->dam = mon_damage_mod(mon, context->dam, FALSE);

        if (context->hooks.before_hit_f)
            context->hooks.before_hit_f(context, mon);

        if (context->action == AMMO_EXPLODE)
        {
            /* better handled in _shoot_aux ... note mon does not
             * take hit and we don't call after_hit_f ... exploding
             * ammo bypasses normal hits and converts the damage
             * into an explosion instead */
            return;
        }
        else if (mon_take_hit(mon, context->dam, &context->fear, NULL))
        {
        }
        else
        {
            message_pain(mon->id, context->dam);
            if (context->dam > 0)
            {
                anger_monster(mon);
                mon_set_target(mon, plr->pos);
            }

            if (context->dam > 0 && mon->cdis > 1 && allow_ticked_off(mon->race))
            {
                if (!plr->stealthy_snipe)
                    mon_anger_shoot(mon, context->dam);
            }

            if (!_hack_cupid(context, mon))
            {
                if (context->fear && mon->ml)
                {
                    char m_name[80];
                    monster_desc(m_name, mon, MD_PRON_VISIBLE);
                    msg_format("%^s flees in terror!", m_name);
                }
            }
        }
        context->dam_total += context->dam;
        if (context->hooks.after_hit_f)
            context->hooks.after_hit_f(context, mon);
    }
    else
    {
        char m_name[80];
        monster_desc(m_name, mon, 0);
        msg_format("The %s misses %s.", context->ammo_desc, m_name);
        context->misses++;
        if (context->hooks.miss_f)
            context->hooks.miss_f(context, mon);
    }
}
static void _apply_vorpal(plr_shoot_ptr context, mon_ptr mon)
{
    int chance = 0;

    if (have_flag(context->ammo_flags, OF_VORPAL2))
        chance = 2;
    else if (have_flag(context->ammo_flags, OF_VORPAL))
        chance = 4;
    else
        return;

    if (one_in_(chance * 3 / 2))
    {
        int mult = 2;
        char m_name[80];

        while (one_in_(chance))
            mult++;

        context->dam_base *= mult;

        monster_desc(m_name, mon, MD_PRON_VISIBLE | MD_OBJECTIVE);
        switch (mult)
        {
        case 2: msg_format("Your %s <color:U>gouges</color> %s!", context->ammo_desc, m_name); break;
        case 3: msg_format("Your %s <color:y>maims</color> %s!", context->ammo_desc, m_name); break;
        case 4: msg_format("Your %s <color:R>carves</color> %s!", context->ammo_desc, m_name); break;
        case 5: msg_format("Your %s <color:r>cleaves</color> %s!", context->ammo_desc, m_name); break;
        case 6: msg_format("Your %s <color:v>smites</color> %s!", context->ammo_desc, m_name); break;
        case 7: msg_format("Your %s <color:v>eviscerates</color> %s!", context->ammo_desc, m_name); break;
        default: msg_format("Your %s <color:v>shreds</color> %s!", context->ammo_desc, m_name); break;
        }

        if (one_in_(chance) && _1d(1 + context->path_pos) < mult)
            context->action = AMMO_PIERCE;

        if (have_flag(context->ammo_flags, OF_VORPAL2))
            obj_learn_slay(context->ammo, OF_VORPAL2, "is <color:v>*Sharp*</color>");
        else
            obj_learn_slay(context->ammo, OF_VORPAL, "is <color:R>Sharp</color>");
    }
}
static int _crit_weight(obj_ptr obj)
{
    return 200 * obj->weight / 5;
}
static void _apply_crit(plr_shoot_ptr context, mon_ptr mon)
{
    slay_t slay = plr_crit(CRIT_FREQ_ROLL, _crit_weight(context->ammo), context->skill, &context->info.crit);
    if (slay.id)
    {
        context->dam_base = context->dam_base * slay.mul / 100;
        context->dam_base += slay.add;
        if (slay.msg) msg_print(slay.msg);
    }
}
static int _slay_dam(slay_ptr slay, int dam)
{
    return dam * slay->mul / 100 + slay->add;
}
static slay_t _slay_find_best(plr_shoot_ptr context, mon_ptr mon, slay_info_ptr tbl)
{
    slay_t best = {0};
    int i, dam, best_dam = context->dam;
    for (i = 0; ; i++)
    {
        slay_info_ptr info = &tbl[i];
        if (!info->id) break;
        if (have_flag(context->ammo_flags, info->id))
        {
            slay_t tmp = slay_info_apply(info, mon, context->ammo);
            if (!tmp.id) continue;
            dam = _slay_dam(&tmp, context->dam);
            if (dam > best_dam)
            {
                best = tmp;
                best_dam = dam;
            }
        }
    }
    return best;
}
static void _apply_slays(plr_shoot_ptr context, mon_ptr mon)
{
    slay_t best_slay, best_brand, slay = {0};
    char m_name[MAX_NLEN_MON];

    monster_desc(m_name, mon, 0);

    /* find the best slay */
    best_slay = _slay_find_best(context, mon, slay_tbl);
    if (context->hooks.calc_slay_f)
    {
        slay_t tmp = context->hooks.calc_slay_f(context, mon, &best_slay);
        int    best_dam = _slay_dam(&best_slay, context->dam);
        int    dam = _slay_dam(&tmp, context->dam);
        if (dam > best_dam)
            best_slay = tmp;
    }
    if (best_slay.msg)
        msg_format(best_slay.msg, m_name);

    /* find the best brand */
    best_brand = _slay_find_best(context, mon, brand_tbl);
    if (context->hooks.calc_brand_f)
    {
        slay_t tmp = context->hooks.calc_brand_f(context, mon, &best_brand);
        int    best_dam = _slay_dam(&best_brand, context->dam);
        int    dam = _slay_dam(&tmp, context->dam);
        if (dam > best_dam)
            best_brand = tmp;
    }
    if (best_brand.msg)
        msg_format(best_brand.msg, m_name);

    /* compute the total slay */
    if (best_slay.id)
    {
        slay = best_slay;
        if (best_brand.id)
        {
            slay.mul += best_brand.mul - 100;
            slay.add += best_brand.add;
        }
    }
    else if (best_brand.id)
        slay = best_brand;

    if (!slay.id) slay.mul = 100;

    /* use the force */
    if (have_flag(context->ammo_flags, OF_BRAND_MANA))
    {
        int cost;
        int dd = context->ammo->dd + context->info.to_dd;
        int ds = context->ammo->ds + context->info.to_ds;

        if (plr->pclass == CLASS_SAMURAI)
            cost = (1 + (dd * ds * 2 / 7));
        else
            cost = (1 + (dd * ds / 7));

        slay = slay_apply_force(slay, context->ammo, cost, FALSE);
    }

    #if DEVELOPER
    if (0 && slay.mul != 100)
    {
        str_ptr s = str_alloc_format("<color:D>%d x %d.%02d", context->dam, slay.mul/100, slay.mul%100);
        if (slay.add)
            str_printf(s, " + %d", slay.add);
        str_printf(s, " = %d</color>", context->dam*slay.mul/100 + slay.add);
        msg_print(str_buffer(s));
        str_free(s);
    }
    #endif

    /* boost the damage */
    if (slay.mul > 100)
        context->dam = context->dam * slay.mul / 100;
    if (slay.add > 0)
        context->dam += slay.add;
}
static bool _hack_cupid(plr_shoot_ptr context, mon_ptr mon)
{
    char m_name[MAX_NLEN];

    if (!obj_is_specified_art(context->ammo, "{.Cupid")) return FALSE;
    if (mon->mflag2 & MFLAG2_QUESTOR) return FALSE;
    if (mon_save_p(mon, A_CHR)) return FALSE;

    monster_desc(m_name, mon, 0);
    if (!mon_save_p(mon, A_CHR))
    {
        if (!mon_is_pet(mon))
        {
            set_pet(mon);
            msg_format("%^s is charmed!", m_name);
        }
        else if (!mon_is_friendly(mon))
        {
            set_friendly(mon);
            msg_format("%^s suddenly becomes friendly.", m_name);
        }
    }
    else if (!mon_is_pet(mon) && !mon_is_friendly(mon))
    {
        set_friendly(mon);
        msg_format("%^s suddenly becomes friendly.", m_name);
    }
    return TRUE;
}
/* end */
static void _end(plr_shoot_ptr context)
{
    if (context->hooks.end_bow_f)
        context->hooks.end_bow_f(context);
    if (context->hooks.end_f)
        context->hooks.end_f(context);
    if (context->path)
        dun_path_free(context->path);

    #ifdef DEVELOPER
    if (1 || plr->wizard)
    {
        rect_t r = ui_char_info_rect();
        int    a = TERM_WHITE;
        if (context->dam_total > 700) a = TERM_VIOLET;
        else if (context->dam_total > 500) a = TERM_RED;
        else if (context->dam_total > 300) a = TERM_L_RED;
        else if (context->dam_total > 150) a = TERM_YELLOW;
        else if (context->dam_total > 70) a = TERM_L_UMBER;
        c_put_str(a, format("Dam:%5d", context->dam_total), 0, r.x);
    }
    #endif
}

/*************************************************************************
 * Hit Testing
 *************************************************************************/
static int _hit_per_mil(int skill, int ac, bool vis)
{
    int odds;
    if (!vis) skill = (skill + 1)/2;
    if (plr_tim_find(T_STUN))
        skill -= skill * MIN(100, plr_tim_amount(T_STUN)) / 150;
    if (skill <= 0) return 0;

    odds = 95*(skill - ac*3/4)*1000/(skill*100);
    if (odds < 50) odds = 50;
    return odds;
}
static int _hit_pct(int skill, int ac, bool vis)
{
    int per_mil = _hit_per_mil(skill, ac, vis);
    return (per_mil + 5)/10;
}
/*************************************************************************
 * Display Code (Character Sheet)
 *************************************************************************/
void plr_shoot_display(void)
{
    plr_shoot_t context = {0};
    plr_shoot_display_aux(&context);
}
void plr_shoot_display_special(int type, int flags)
{
    plr_shoot_t context = {0};
    context.mode = type;
    context.flags = flags;
    plr_shoot_display_aux(&context);
}
void plr_shoot_display_aux(plr_shoot_ptr context)
{
    doc_ptr doc = doc_alloc(80);

    plr_shoot_doc_aux(context, doc);
    if (doc_line_count(doc))
    {
        /* screen_load|save do nothing if already saved. I'll fix this someday, but
         * for now we need ugly hacks to prevent garbage when browsing spells.
         * these hacks only work if screen_depth is 1 and if clients know to redraw
         * their menus inside their menu loops */
        bool screen_hack = screen_is_saved();
        if (screen_hack) screen_load(); /* screen_depth back to 0 */
    
        screen_save(); /* <=== bug: this would not save our current state without the screen_hack */
        doc_display(doc, "Shoot", 0);
        screen_load(); /* <=== bug: this would not erase our drawing without the screen_hack */

        if (screen_hack) screen_save(); /* screen_depth back to 1 with (hopefully) the original screen */
    }
    else
        msg_print("You have no archery attacks.");

    doc_free(doc);
}
static void _display_bow(plr_shoot_ptr context, doc_ptr doc);
static void _display_ammo(plr_shoot_ptr context, obj_ptr ammo, int counter, doc_ptr doc);
void plr_shoot_doc(doc_ptr doc)
{
    plr_shoot_t context = {0};
    plr_shoot_doc_aux(&context, doc);
}
void plr_shoot_doc_aux(plr_shoot_ptr context, doc_ptr doc)
{
    int i, j;

    /* begin */
    context->flags |= PSC_DISPLAY;
    context->bow = _current_bow();
    if (!context->bow) return;
    _init_bow(context);
    _plr_custom_init(context);
    if (context->hooks.begin_f)
        context->hooks.begin_f(context);

    /* Bow */
    _display_bow(context, doc);

    /* Ammo */
    j = 0;
    for (i = quiver_find_first(obj_can_shoot); i; i = quiver_find_next(obj_can_shoot, i))
    {
        obj_ptr ammo = quiver_obj(i);
        _display_ammo(context, ammo, ++j, doc);
    }
    for (i = pack_find_first(obj_can_shoot); i; i = pack_find_next(obj_can_shoot, i))
    {
        obj_ptr ammo = pack_obj(i);
        _display_ammo(context, ammo, ++j, doc);
    }

    /* end */
    if (context->hooks.end_f)
        context->hooks.end_f(context);
}
static void _display_bow(plr_shoot_ptr context, doc_ptr doc)
{
    int  num_fire = 100;
    int  to_h = 0;
    int  to_d = 0;
    char o_name[MAX_NLEN];
    bool sniper = plr->pclass == CLASS_SNIPER && plr->concent;

    num_fire = _num_fire(context) * 100 * 100 / bow_energy(context->bow->sval);

    if (obj_is_known(context->bow))
    {
        to_h = context->bow->to_h;
        to_d = context->bow->to_d;
    }
    to_h += skills_bow_calc_bonus(context->bow->sval);

    object_desc(o_name, context->bow, OD_OMIT_INSCRIPTION | OD_COLOR_CODED);
    doc_printf(doc, " <color:y>Shooting</color>: <indent><style:indent>%s</style></indent>\n", o_name);

    doc_printf(doc, " %-8.8s: %d'\n", "Range", context->range * 10);
    if (sniper)
    {
        doc_printf(doc, " <color:D>%-8.8s: %d.%02d</color>\n", "Shots", num_fire/100, num_fire%100);
        doc_printf(doc, " %-8.8s: %d\n", "Concent", plr->concent);
    }
    else
        doc_printf(doc, " %-8.8s: %d.%02d\n", "Shots", num_fire/100, num_fire%100);
    doc_printf(doc, " %-8.8s: %d.%02dx\n", "Mult", context->mult/100, context->mult%100);
    doc_printf(doc, " %-8.8s: %s (%+d To Hit)\n", "Profic",
        skills_bow_describe_current(context->bow->sval),
        skills_bow_calc_bonus(context->bow->sval));
    doc_printf(doc, " %-8.8s: %d + %d = %d\n", "To Hit", to_h, plr->shooter_info.dis_to_h,
        to_h + plr->shooter_info.dis_to_h);
    if (context->to_d)
        doc_printf(doc, " %-8.8s: %d (%s)\n", "To Dam", context->to_d, "Multiplier Applies");
    doc_printf(doc, " %-8.8s: %d (%s)\n", "Xtra Dam", plr->shooter_info.dis_to_d + to_d, "Multiplier Does Not Apply");
    if (sniper)
        doc_printf(doc, "\n <color:B>Note:</color> <indent>Only showing damage based on 1 shot since you are concentrating. Once you fire, your concentration will reset.</indent>\n");
    doc_newline(doc);
}
typedef struct { /* Damage is: F[M(S(XdY)+Z)+W)] */
    dice_t  dice;       /* XdY+Z (e.g. Seeker Arrow(6d4+12) */
    int     to_d_xtra;  /* W */
    slay_t  crit;       /* S for crits and vorpals */
    int     bow_mult;   /* M */
    int     num_fire;   /* F */
    doc_ptr doc;
} _display_info_t, *_display_info_ptr;
static void _display_ammo_slay(_display_info_ptr info, slay_t slay, bool force, cptr name, int color)
{
    int dam1, dam2, dam3, base1, base2, base3;
    int roll;
    const int scale = 100;
    dice_t dice2 = info->dice;

    dice2.base = 0;
    roll = scale*dice2.dd*(dice2.ds + 1)/2; /* scale dice_avg_roll (e.g. 1d4 s/b 250, not 200) */
    if (dice2.scale)
        roll = (roll*dice2.scale + 500)/1000;
    base1 = info->crit.mul*roll/100 + info->crit.add; /* S(XdY) using baseline S (XXX crit.add is scaled by 100) */
    base2 = info->bow_mult*(base1 + scale*info->dice.base)/100; /* M(S(XdY)+Z) */
    base3 = info->num_fire*(base2 + scale*info->to_d_xtra)/100; /* F[M(S(XdY)+Z)+W] */

    if (force)
        slay.mul = slay.mul * 3/2 + 150;

    dam1 = slay.mul*(base1)/100 + scale*slay.add;             /* S(XdY) using true S */
    dam2 = info->bow_mult*(dam1 + scale*info->dice.base)/100; /* M(S(XdY)+Z) */
    dam3 = info->num_fire*(dam2 + scale*info->to_d_xtra)/100; /* F[M(S(XdY)+Z)+W] */

    doc_printf(info->doc, "<color:%c> %s</color><tab:8>", attr_to_attr_char(color), name);
    if (slay.mul == 100)
        doc_printf(info->doc, ": %4d\n", dam3/scale);
    else
        doc_printf(info->doc, ": %4d  (+%d)\n", dam3/scale, (dam3 - base3)/scale);
}
static void _display_ammo(plr_shoot_ptr context, obj_ptr ammo, int ct, doc_ptr doc)
{
    _display_info_t info = {{0}};
    slay_t slay = {0};
    int    to_h = 0;
    int    to_h_bow = 0;
    int    to_h_xtra = plr->shooter_info.dis_to_h;
    int    i, crit_chance;
    bool    force = FALSE, custom = FALSE;
    char    o_name[MAX_NLEN];
    doc_ptr cols[2] = {0};
    int stun = plr_tim_amount(T_STUN);
    bool sniper = plr->pclass == CLASS_SNIPER && plr->concent;

    context->ammo = ammo;
    _init_ammo(context);

    if (context->hooks.begin_bow_f)
        context->hooks.begin_bow_f(context);

    info.bow_mult = context->mult;
    if (!obj_is_art(ammo) && !sniper)
        info.num_fire = _num_fire(context) * 100 * 100 / bow_energy(context->bow->sval);
    else
        info.num_fire = 100;

    info.to_d_xtra = plr->shooter_info.dis_to_d;
    if (obj_is_known(context->bow))
    {
        to_h_bow = context->bow->to_h;
        info.to_d_xtra += context->bow->to_d;
    }
    to_h_bow += skills_bow_calc_bonus(context->bow->sval);

    info.dice.dd = ammo->dd + context->info.to_dd;
    info.dice.ds = ammo->ds + context->info.to_ds;
    if (obj_is_known(ammo))
    {
        info.dice.base += ammo->to_d;
        to_h += ammo->to_h;
    }
    info.dice.base += context->to_d;
    if (stun)
        info.dice.scale = 1000 - 1000 * stun / 150;

    if (have_flag(context->ammo_flags, OF_BRAND_MANA))
    {
        caster_info *caster = get_caster_info();
        int          cost = 0;

        if (plr->pclass == CLASS_SAMURAI)
            cost = (1 + (info.dice.dd * info.dice.ds * 2 / 7));
        else
            cost = (1 + (info.dice.dd * info.dice.ds / 7));

        if (caster && (caster->options & CASTER_USE_AU))
        {
            cost *= 10;
            if (plr->au >= cost)
                force = TRUE;
        }
        else if (plr->csp >= cost)
            force = TRUE;
    }

    /* Display in 2 columns, side by side */
    cols[0] = doc_alloc(60);
    cols[1] = doc_alloc(10);

    /* Column #1 */
    info.doc = cols[0];
    object_desc(o_name, ammo, OD_OMIT_INSCRIPTION | OD_COLOR_CODED);
    doc_printf(cols[0], "<color:u> Ammo #%-2d</color>: <indent><style:indent>%s</style></indent>\n", ct, o_name);

    doc_printf(cols[0], " %-8.8s: %d%%\n", "Breakage", _breakage_chance(context));
    doc_printf(cols[0], " %-8.8s: %d.%d lbs\n", "Weight", ammo->weight/10, ammo->weight%10);
    doc_printf(cols[0], " %-8.8s: %d + %d = %d\n", "To Hit", to_h, to_h_bow + to_h_xtra, to_h + to_h_bow + to_h_xtra);
    doc_printf(cols[0], " %-8.8s: %d (%s)\n", "To Dam", info.dice.base, "Multiplier Applies");
    doc_printf(cols[0], " %-8.8s: %d (%s)\n", "To Dam", info.to_d_xtra, "Multiplier Does Not Apply");
    doc_printf(cols[0], " <color:G>%-8.8s</color>\n", "Damage");

    /* criticals and vorpals give a "base slay" */
    crit_chance = plr_crit_chance(CRIT_FREQ_ROLL, _crit_weight(ammo), context->skill, &context->info.crit);
    info.crit = plr_crit_avg(CRIT_QUAL_ROLL, _crit_weight(ammo), &context->info.crit, crit_chance);
    crit_doc(cols[0], info.crit, crit_chance);

    if (have_flag(context->ammo_flags, OF_VORPAL2))
    {
        info.crit.mul = info.crit.mul * 5 / 3;  /* 1 + 1/3(1 + 1/2 + ...) = 1.667x */
        info.crit.add = info.crit.add * 5 / 3;
    }
    else if (have_flag(context->ammo_flags, OF_VORPAL))
    {
        info.crit.mul = info.crit.mul * 11 / 9; /* 1 + 1/6(1 + 1/4 + ...) = 1.222x */
        info.crit.add = info.crit.add * 11 / 9;
    }

    slay.mul = 100;
    _display_ammo_slay(&info, slay, FALSE, "Normal", TERM_WHITE);
    if (force)
        _display_ammo_slay(&info, slay, force, "Force", TERM_L_BLUE);

    /* slays */
    for (i = 0; ; i++)
    {
        slay_info_ptr si = &slay_tbl[i];
        slay_t slay = {0};
        if (!si->id) break;
        if (!have_flag(context->ammo_flags, si->id)) continue;
        slay = slay_info_apply(si, NULL, NULL);
        if (context->hooks.calc_slay_f)
        {
            slay_t tmp = context->hooks.calc_slay_f(context, NULL, &slay);
            if (tmp.id == slay.id)
            {
                slay = tmp;
                custom = TRUE;
            }
        }
        _display_ammo_slay(&info, slay, force, slay.name, TERM_YELLOW);
    }
    if (!custom && context->hooks.calc_slay_f) /* skip custom slay if hook improved an existing one */
    {
        slay_t dummy = {0};
        slay_t slay = context->hooks.calc_slay_f(context, NULL, &dummy);
        if (slay.name)
            _display_ammo_slay(&info, slay, force, slay.name, TERM_YELLOW);
    }
    /* brands */
    custom = FALSE;
    for (i = 0; ; i++)
    {
        slay_info_ptr bi = &brand_tbl[i];
        slay_t brand = {0};
        if (!bi->id) break;
        if (!have_flag(context->ammo_flags, bi->id)) continue;
        brand = slay_info_apply(bi, NULL, NULL);
        if (context->hooks.calc_brand_f)
        {
            slay_t tmp = context->hooks.calc_brand_f(context, NULL, &brand);
            if (tmp.id == brand.id) /* e.g. Lightning Eagle improves existing OF_BRAND_ELEC to 7x */
            {
                brand = tmp;
                custom = TRUE;
            }
        }
        _display_ammo_slay(&info, brand, force, brand.name, TERM_RED);
    }
    if (!custom && context->hooks.calc_brand_f) /* e.g. Lightning Eagle is a mere 5x if no exising OF_BRAND_ELEC */
    {
        slay_t dummy = {0};
        slay_t brand = context->hooks.calc_brand_f(context, NULL, &dummy);
        if (brand.name)
            _display_ammo_slay(&info, brand, force, brand.name, TERM_RED);
    }

    /* Column #2 */
    doc_insert(cols[1], "<color:G>Accuracy</color>\n");
    doc_insert(cols[1], " AC Hit\n");

    doc_printf(cols[1], "%3d %2d%%\n", 25, _hit_pct(context->skill, 25, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 50, _hit_pct(context->skill, 50, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 75, _hit_pct(context->skill, 75, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 100, _hit_pct(context->skill, 100, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 125, _hit_pct(context->skill, 125, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 150, _hit_pct(context->skill, 150, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 175, _hit_pct(context->skill, 175, TRUE));
    doc_printf(cols[1], "%3d %2d%%\n", 200, _hit_pct(context->skill, 200, TRUE));

    /* Assemble the result */
    doc_insert_cols(doc, cols, 2, 1);
    doc_free(cols[0]);
    doc_free(cols[1]);
}
