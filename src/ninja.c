#include "angband.h"
#include "equip.h"

bool kawarimi(bool success)
{
    point_t old_pos = plr->pos;

    if (plr->is_dead) return FALSE;
    if (plr_tim_find(T_CONFUSED) || plr_tim_find(T_BLIND) || plr_tim_find(T_PARALYZED) || plr_tim_find(T_HALLUCINATE)) return FALSE;
    if (randint0(200) < plr_tim_amount(T_STUN)) return FALSE;

    if (!success && one_in_(3))
    {
        msg_print("Failed! You could not escape.");
        plr->special_defense &= ~(NINJA_KAWARIMI);
        plr->redraw |= (PR_STATUS);
        return FALSE;
    }

    teleport_player(10 + _1d(90), 0);

    if (plr->pclass == CLASS_NINJA)
    {
        object_type forge;
        object_wipe(&forge);
        object_prep(&forge, lookup_kind(TV_STATUE, SV_WOODEN_STATUE));
        forge.race_id = mon_race_parse("p.ninja")->id;
        dun_drop_near(cave, &forge, old_pos);
    }

    if (success) msg_print("You have escaped just before the attack hit you.");
    else msg_print("Failed! You are hit by the attack.");

    plr->special_defense &= ~(NINJA_KAWARIMI);
    plr->redraw |= (PR_STATUS);

    return TRUE;
}

static void _absconding_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Absconding");
        break;
    default:
        teleport_spell(cmd, res);
    }
}

static void _ancient_knowledge_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Ancient Knowledge");
        break;
    default:
        identify_spell(cmd, res);
    }
}

static void _bind_monster_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Bind Monster");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to freeze a monster.");
        break;
    default:
        direct_spell(cmd, res, GF_STASIS, 2*plr->lev);
    }
}

static void _bunshin_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Bunshin");
        break;
    case SPELL_DESC:
        var_set_string(res, "Creates shadows of yourself which gives you ability to completely evade any attacks at one in two chance for a while.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_MULTISHADOW, 6 + _1d(6));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}

static void _chain_hook_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Chain Hook");
        break;
    case SPELL_DESC:
        var_set_string(res, "Teleport a monster to a place adjacent to you.");
        break;
    case SPELL_CAST:
    {
        mon_ptr mon = NULL;
        char m_name[MAX_NLEN_MON];
        int i;
        int path_n;
        point_t path[MAX_SIGHT];
        point_t tgt, cur, next;

        var_set_bool(res, FALSE);
        if (!target_set(TARGET_KILL)) return;
        if (who_is_mon(plr->target))
        {
            mon = who_mon(plr->target);
            tgt = mon->pos;
        }
        else if (who_is_pos(plr->target))
        {
            tgt = who_pos(plr->target);
            mon = dun_mon_at(cave, tgt);
        }
        if (!mon) return;
        if (mon->id == plr->riding) return;
        if (!plr_view(tgt)) return;
        if (!plr_project(tgt)) return;

        monster_desc(m_name, mon, 0);
        msg_format("You pull back %s.", m_name);

        /* start at target, and project back to the player */
        path_n = project_path(path, MAX_RANGE, tgt, plr->pos, 0);
        cur = tgt;
        for (i = 1; i < path_n; i++)
        {
            dun_cell_ptr cell;
            next = path[i];
            cell = dun_cell_at(cave, next);

            if (dun_allow_mon_at(cave, next) && !floor_has_object(cell))
                cur = next;
        }
        mon_tim_delete(mon, MT_SLEEP);
        dun_move_mon(cave, mon, cur);
        draw_pos(tgt);
        draw_pos(cur);

        if (mon->race->light || mon->race->lantern)
            plr->update |= PU_MON_LIGHT;

        if (mon->ml)
        {
            if (!plr_tim_find(T_HALLUCINATE)) mon_track(mon);
            health_track(mon);
        }

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
    }
}

static void _detect_near_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Near");
        break;
    case SPELL_DESC:
        if (plr->lev >= 45)
            var_set_string(res, "Detects nearby monsters, traps, doors, stairs and objects. Maps the entire level.");
        else if (plr->lev >= 15)
            var_set_string(res, "Detects nearby monsters, traps, doors, stairs and objects.");
        else if (plr->lev >= 5)
            var_set_string(res, "Detects nearby monsters, traps, doors and stairs.");
        else 
            var_set_string(res, "Detects nearby monsters.");
        break;
    case SPELL_CAST:
        if (plr->lev >= 45)
        {
            wiz_map();
        }
        detect_monsters_normal(DETECT_RAD_DEFAULT);
        if (plr->lev >= 5)
        {
            detect_traps(DETECT_RAD_DEFAULT, TRUE);
            detect_doors(DETECT_RAD_DEFAULT);
            detect_stairs(DETECT_RAD_DEFAULT);
            detect_recall(DETECT_RAD_DEFAULT);
        }
        if (plr->lev >= 15)
        {
            detect_objects_normal(DETECT_RAD_DEFAULT);
        }
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}

static void _floating_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Floating");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives levitation for a while.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_LEVITATION, 20 + _1d(20));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}

static void _glyph_of_explosion_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Glyph of Explosion");
        break;
    default:
        explosive_rune_spell(cmd, res);
    }
}

void hide_in_flame_spell(int cmd, var_ptr res)
{
    int dam = 25 + plr->lev/2;
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Hide in Flame");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generate a fire ball and teleport in a time. Gives resistance to fire for a while.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, dam));
        break;
    case SPELL_CAST:
        plr_burst(2 + plr->lev/10, GF_FIRE, dam);
        teleport_player(30, 0);
        plr_tim_add(T_RES_FIRE, plr->lev);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}

static void _hide_in_leaves_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Hide in Leaves");
        break;
    default:
        phase_door_spell(cmd, res);
    }
}

void hide_in_mist_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Hide in Mist");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generates huge balls of poison, drain life and confusion, then teleport in a time.");
        break;
    case SPELL_CAST: {
        int rad = MIN(MAX_PRECOMPUTE_DISTANCE, 2 + plr->lev/5);
        int dam = 38 + plr->lev/3;
        plr_burst(rad, GF_POIS, dam);
        plr_burst(rad, GF_OLD_DRAIN, dam);
        plr_burst(rad, GF_CONFUSION, dam);
        teleport_player(30, 0);
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
    }
}

static void _hit_and_away_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Hit and Away");
        break;
    default:
        panic_hit_spell(cmd, res);
    }
}

static void _kawarimi_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Kawarimi");
        break;
    case SPELL_DESC:
        var_set_string(res, "Teleport as you receive an attack. Might be able to teleport just before receiving damages at higher level.");
        break;
    case SPELL_CAST:
        if (!(plr->special_defense & NINJA_KAWARIMI))
        {
            msg_print("You are now prepare to evade any attacks.");
            plr->special_defense |= NINJA_KAWARIMI;
            plr->redraw |= PR_STATUS;
        }
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}

static void _nyusin_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Nyusin");
        break;
    case SPELL_DESC:
        var_set_string(res, "Steps close to a monster and attacks at a time.");
        break;
    case SPELL_CAST:
        var_set_bool(res, rush_attack(5, NULL));
        break;
    default:
        default_spell(cmd, res);
    }
}

void quick_walk_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Quick Walk");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
        if (plr->action == ACTION_QUICK_WALK) set_action(ACTION_NONE);
        else set_action(ACTION_QUICK_WALK);
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        var_set_int(res, 0);
        break;
    default:
        default_spell(cmd, res);
    }
}

static void _rengoku_kaen_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Rengoku-Kaen");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires some number of beams of fire, nether or plasma in random directions.");
        break;
    case SPELL_CAST: {
        int i;
        int ct = _3d(9);
        dice_t dice = dice_create(6 + plr->lev/8, 10, 0);
        for (i = 0; i < ct; i++)
        {
            int gf = one_in_(2) ? GF_FIRE : one_in_(3) ? GF_NETHER : GF_PLASMA;
            point_t p;
            int attempt = 1000;

            while (attempt--)
            {
                p = scatter(plr->pos, 4);
                if (!dun_plr_at(cave, p)) break;
            }
            if (attempt < 0) continue;
            plr_beam(p, gf, dice_roll(dice));
        }
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _smoke_ball_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Smoke Ball");
        break;
    case SPELL_DESC:
        var_set_string(res, "Releases a confusion ball which doesn't inflict any damage.");
        break;
    default:
        ball_spell(cmd, res, 3, GF_OLD_CONF, 3*plr->lev);
    }
}

static bool _obj_is_shuriken(obj_ptr obj) { return obj->tval == TV_SPIKE; }
static void _syuriken_spreading_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Syuriken Spreading");
        break;
    case SPELL_DESC:
        var_set_string(res, "Shoots 8 iron Spikes in 8 random directions.");
        break;
    case SPELL_CAST:
    {
        int i;
        for (i = 0; i < 8; i++)
        {
            int         slot = pack_find_first(_obj_is_shuriken);
            plr_throw_t context = {0}; /* better reset for each shot! */
            if (!slot)
            {
                if (!i) msg_print("You have no Iron Spikes.");
                else msg_print("You have no more Iron Spikes.");
                break;
            }
            context.dir = DIR_RANDOM;
            context.obj = pack_obj(slot);
            plr_throw(&context);
        }
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

/****************************************************************
 * Spell Table and Exports
 ****************************************************************/
static spell_info _spells[] = 
{
    /*lvl cst fail spell */
    { 1,   1,  20, create_darkness_spell},
    { 2,   2,  25, _detect_near_spell},
    { 3,   3,  25, _hide_in_leaves_spell},
    { 5,   3,  30, _kawarimi_spell},
    { 7,   8,  35, _absconding_spell},
    { 8,  10,  35, _hit_and_away_spell},
    {10,  10,  40, _bind_monster_spell},
    {12,  12,  70, _ancient_knowledge_spell},
    {15,  10,  50, _floating_spell},
    {17,  12,  45, hide_in_flame_spell},
    {18,  20,  40, _nyusin_spell},
    {20,   5,  50, _syuriken_spreading_spell},
    {22,   5,  55, _chain_hook_spell},
    {25,  32,  60, _smoke_ball_spell},
    {28,  32,  60, swap_pos_spell},
    {30,  30,  70, _glyph_of_explosion_spell},
    {32,  40,  40, hide_in_mud_spell},
    {34,  35,  50, hide_in_mist_spell},
    {38,  40,  60, _rengoku_kaen_spell},
    {41,  50,  55, _bunshin_spell},
    { -1, -1,  -1, NULL}
};

static power_info _powers[] =
{
    { A_DEX, { 20, 0,  0, quick_walk_spell}}, 
    { -1, {-1, -1, -1, NULL}}
};

static int _get_spells(spell_info* spells, int max)
{
    return get_spells_aux(spells, max, _spells);
}

static void _character_dump(doc_ptr doc)
{
    spell_info spells[MAX_SPELLS];
    int        ct = _get_spells(spells, MAX_SPELLS);

    plr_display_spells(doc, spells, ct);
}

static int _get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _powers);
}

static void _calc_bonuses(void)
{
    if (heavy_armor())
    {
        plr->pspeed -= plr->lev/10;
        plr->skills.stl -= plr->lev/10;
    }
    else if (!equip_find_obj(TV_SHIELD, SV_ANY))
    {
        plr->pspeed += 3;
        plr->pspeed += plr->lev/10;
        plr->skills.stl += plr->lev/10;
        if (plr->lev >= 25)
            plr->free_act++;
        /* Ninjas are not archers, and have relatively poor thb skills.
         * However, they excel at throwing (tht)! */
        plr->skill_tht += 30 + plr->lev;
        plr->ambush = 300 + plr->lev*4;
        plr->backstab = 150;
    }
    if (!equip_find_obj(TV_SHIELD, SV_ANY))
    {
        plr->to_a += plr->lev/2 + 5;
        plr->dis_to_a += plr->lev/2 + 5;
    }
    plr->slow_digest = TRUE;
    res_add(GF_FEAR);
    if (plr->lev >= 20) res_add(GF_POIS);
    if (plr->lev >= 25) plr->sustain_dex = TRUE;
    if (plr->lev >= 30) plr->see_inv++;
    if (plr->lev >= 45) res_add(GF_POIS);
    plr->see_nocto = DUN_VIEW_MAX;
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (heavy_armor())
        add_flag(flgs, OF_SPEED);
    else
    {
        if (!equip_find_obj(TV_SHIELD, SV_ANY))
        {
            add_flag(flgs, OF_SPEED);
        }
        if (plr->lev >= 25)
            add_flag(flgs, OF_FREE_ACT);
    }
    add_flag(flgs, OF_SLOW_DIGEST);
    add_flag(flgs, OF_RES_(GF_FEAR));
    if (plr->lev >= 20) add_flag(flgs, OF_RES_(GF_POIS));
    if (plr->lev >= 25) add_flag(flgs, OF_SUST_DEX);
    if (plr->lev >= 30) add_flag(flgs, OF_SEE_INVIS);
}

static void _calc_weapon_bonuses(object_type *o_ptr, plr_attack_info_ptr info)
{
    if ( skills_weapon_is_icky(o_ptr->tval, o_ptr->sval) 
      || equip_find_obj(TV_SHIELD, SV_ANY) )
    {
        info->to_h -= 40;
        info->dis_to_h -= 40;
        add_flag(info->paf_flags, PAF_ICKY);
        info->base_blow /= 2;
        info->xtra_blow /= 2;
        if (info->base_blow < 100) info->base_blow = 100;
    }
    else if (!heavy_armor())
        info->crit.freq_add += 100; /* add 10.0% to critical frequency since daggers never crit! */
}

static bool _is_critical_blow(plr_attack_ptr context)
{
    int odds = context->technique ? 14 : 27;
    return one_in_(odds);
}
static bool _is_fatal_blow(plr_attack_ptr context)
{
    int maxhp = dice_hi_roll(context->race->hp);
    if (context->mon->hp < maxhp/2 && one_in_(context->blow_ct*10)) return TRUE;
    if (mon_race_is_unique(context->race)) return FALSE;
    if (one_in_(666)) return TRUE;
    if (context->technique > 150 && one_in_(11)) return TRUE;
    return FALSE;
}
static void _mod_damage(plr_attack_ptr context)
{
    if (context->info.type != PAT_WEAPON && !mut_present(MUT_DRACONIAN_METAMORPHOSIS)) return;
    if (have_flag(context->info.paf_flags, PAF_ICKY)) return;
    if (!context->dam) return;  /* MON_INVULNER() */
    if (plr->cur_light <= 0 || one_in_(7))
    {
        if (_is_critical_blow(context))
        {
            context->dam *= 5;
            context->dam_drain *= 2;
            msg_format("<color:R>You critically injured %s!</color>", context->mon_name_obj);
        }
        else if (_is_fatal_blow(context))
        {
            int maxhp = dice_hi_roll(context->race->hp);
            if (mon_race_is_unique(context->race) || context->mon->hp >= maxhp/2)
            {
                context->dam = MAX(context->dam*5, context->mon->hp/2);
                context->dam_drain *= 2;
                msg_format("<color:v>You fatally injured %s!</color>", context->mon_name_obj);
            }
            else
            {
                context->dam = context->mon->hp + 1;
                msg_format("<color:v>You hit %s on a fatal spot!</color>", context->mon_name_obj);
            }
        }
    }
}
static void _attack_init(plr_attack_ptr context)
{
    context->hooks.mod_damage_f = _mod_damage;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "ninjutsu";
        me.options = CASTER_USE_HP | CASTER_GAIN_SKILL;
        me.which_stat = A_DEX;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_DAGGER, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_obj_aux(TV_POTION, SV_POTION_SPEED, 1);
    plr_birth_obj_aux(TV_SPIKE, 0, rand_range(15, 20));
}

static status_display_t _status_display(void)
{
    status_display_t d = {0};
    if (plr->special_defense & NINJA_KAWARIMI)
    {
        d.name = "Kawarimi";
        d.abbrev = "Kw";
        d.color = TERM_L_BLUE;
    }
    return d;
}

static bool _moved = FALSE;

static void _update_light(void)
{
    int light = plr_light(plr->pos);
    /* stealth always goes OFF if light > 0
     * e.g. equip lantern
     *      monster wakes up (mon light)
     *      monster approaches (mon light)
     *      move to lit square */
    if (plr->special_defense & NINJA_S_STEALTH)
    {
        if (light > 0)
            set_superstealth(FALSE);
    }
    /* stealth can only ever go ON by moving to an unlit grid */
    if (_moved)
    {
        _moved = FALSE;
        if (!(plr->special_defense & NINJA_S_STEALTH) && light <= 0)
            set_superstealth(TRUE);
    }
}

static void _move_player(void)
{
    /* wait for PU_LIGHT to process (handle_stuff) */
    _moved = TRUE;
}

plr_class_ptr ninja_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 45,  24,  36,   8,  48,  32,  70,  35 };
    skills_t xs = { 75,  50,  50,   0,   0,   0, 125,  55 };

        me = plr_class_alloc(CLASS_NINJA);
        me->name = "Ninja";
        me->desc = "A Ninja is a fearful assassin lurking in darkness. He or she can "
                    "navigate effectively with no light source, catch enemies by surprise, "
                    "and kill with a single blow. Ninjas can use Ninjutsu, and are "
                    "good at locating hidden traps and doors, disarming traps and "
                    "picking locks. Since heavy armors, heavy weapons, or shields will "
                    "restrict their motion greatly, they prefer light clothes, and "
                    "become faster and more stealthy as they gain levels. A Ninja "
                    "knows no fear and, at high level, becomes almost immune to poison "
                    "and able to see invisible things. Dexterity determines a Ninja's "
                    "ability to use Ninjutsu.\n \n"
                    "A Ninja can use Ninjutsu for lurking and surprise attacks. They "
                    "gain more Ninjutsu techniques as they gain levels. They have a "
                    "class power - 'Quick Walk' - which makes their walking speed "
                    "extremely fast.";

        me->stats[A_STR] =  0;
        me->stats[A_INT] = -1;
        me->stats[A_WIS] = -1;
        me->stats[A_DEX] =  3;
        me->stats[A_CON] =  2;
        me->stats[A_CHR] =  2;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 100;
        me->base_hp = 4;
        me->exp = 120;
        me->pets = 40;
        me->flags = CLASS_SENSE1_MED | CLASS_SENSE1_STRONG |
                    CLASS_SENSE2_STRONG;

        me->hooks.birth = _birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_spells = _get_spells;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = _character_dump;
        me->hooks.attack_init = _attack_init;
        me->hooks.status_display = _status_display;
        me->hooks.update_light = _update_light;
        me->hooks.move_player = _move_player;
    }

    return me;
}
