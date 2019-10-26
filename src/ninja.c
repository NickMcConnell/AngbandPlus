#include "angband.h"
#include "equip.h"

static void _absconding_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Absconding");
        break;
    default:
        teleport_spell(cmd, res);
        break;
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
        break;
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
    case SPELL_CAST:
    {
        int dir;
        var_set_bool(res, FALSE);
        if (!get_fire_dir(&dir)) return;
        stasis_monster(dir);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
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
        plr_tim_add(T_MULTISHADOW, 6+randint1(6));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
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
        mon_ptr mon;
        char m_name[MAX_NLEN_MON];
        int i;
        int path_n;
        point_t path[MAX_SIGHT];
        point_t tgt, cur, next;

        var_set_bool(res, FALSE);
        if (!target_set(TARGET_KILL)) return;
        tgt = point_create(target_col, target_row);
        mon = mon_at(tgt);
        if (!mon) return;
        if (mon->id == p_ptr->riding) return;
        if (!player_has_los_bold(target_row, target_col)) return;
        if (!projectable(p_ptr->pos.y, p_ptr->pos.x, target_row, target_col)) return;

        monster_desc(m_name, mon, 0);
        msg_format("You pull back %s.", m_name);

        /* start at target, and project back to the player */
        path_n = project_path(path, MAX_RANGE, tgt, p_ptr->pos, 0);
        cur = tgt;
        for (i = 1; i < path_n; i++)
        {
            cave_type *c_ptr;
            next = path[i];
            c_ptr = cave_at(next);

            if (cave_empty_at(next) && !(c_ptr->info & CAVE_OBJECT) && !pattern_tile(next.y, next.x))
                cur = next;
        }
        mon_tim_delete(mon, MT_SLEEP);
        dun_move_mon(cave, mon, cur);
        lite_pos(tgt);
        lite_pos(cur);

        if (mon_race(mon)->flags7 & (RF7_LITE_MASK | RF7_DARK_MASK))
            p_ptr->update |= (PU_MON_LITE);

        if (mon->ml)
        {
            if (!plr_tim_find(T_HALLUCINATE)) mon_track(mon);
            health_track(mon->id);
        }

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
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
        if (p_ptr->lev >= 45)
            var_set_string(res, "Detects nearby monsters, traps, doors, stairs and objects. Maps the entire level.");
        else if (p_ptr->lev >= 15)
            var_set_string(res, "Detects nearby monsters, traps, doors, stairs and objects.");
        else if (p_ptr->lev >= 5)
            var_set_string(res, "Detects nearby monsters, traps, doors and stairs.");
        else 
            var_set_string(res, "Detects nearby monsters.");
        break;
    case SPELL_CAST:
        if (p_ptr->lev >= 45)
        {
            wiz_lite();
        }
        detect_monsters_normal(DETECT_RAD_DEFAULT);
        if (p_ptr->lev >= 5)
        {
            detect_traps(DETECT_RAD_DEFAULT, TRUE);
            detect_doors(DETECT_RAD_DEFAULT);
            detect_stairs(DETECT_RAD_DEFAULT);
            detect_recall(DETECT_RAD_DEFAULT);
        }
        if (p_ptr->lev >= 15)
        {
            detect_objects_normal(DETECT_RAD_DEFAULT);
        }
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
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
        plr_tim_add(T_LEVITATION, randint1(20) + 20);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
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
        break;
    }
}

void hide_in_flame_spell(int cmd, var_ptr res)
{
    int dam = 50 + p_ptr->lev;
    int rad = 2 + p_ptr->lev/10;
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Hide in Flame");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generate a fire ball and teleport in a time. Gives resistance to fire for a while.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, dam/2));
        break;
    case SPELL_CAST:
        fire_ball(GF_FIRE, 0, dam, rad);
        teleport_player(30, 0);
        plr_tim_add(T_RES_FIRE, p_ptr->lev);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
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
        break;
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
    case SPELL_CAST:
        fire_ball(GF_POIS, 0, 75+p_ptr->lev*2/3, p_ptr->lev/5+2);
        fire_ball(GF_OLD_DRAIN, 0, 75+p_ptr->lev*2/3, p_ptr->lev/5+2);
        fire_ball(GF_CONFUSION, 0, 75+p_ptr->lev*2/3, p_ptr->lev/5+2);
        teleport_player(30, 0L);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
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
        break;
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
        if (!(p_ptr->special_defense & NINJA_KAWARIMI))
        {
            msg_print("You are now prepare to evade any attacks.");
            p_ptr->special_defense |= NINJA_KAWARIMI;
            p_ptr->redraw |= PR_STATUS;
        }
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
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
        break;
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
        if (p_ptr->action == ACTION_QUICK_WALK) set_action(ACTION_NONE);
        else set_action(ACTION_QUICK_WALK);
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        var_set_int(res, 0);
        break;
    default:
        default_spell(cmd, res);
        break;
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
        int k;
        int num = damroll(3, 9);

        for (k = 0; k < num; k++)
        {
            int typ = one_in_(2) ? GF_FIRE : one_in_(3) ? GF_NETHER : GF_PLASMA;
            point_t p;
            int attempt = 1000;

            while (attempt--)
            {
                p = scatter(p_ptr->pos, 4);
                if (!plr_at(p)) break;
            }
            if (attempt < 0) continue;
            project(0, 0, p.y, p.x, damroll(6 + p_ptr->lev / 8, 10), typ,
                (PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID | PROJECT_KILL));
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
    case SPELL_CAST:
    {
        int dir;
        var_set_bool(res, FALSE);
        if (!get_fire_dir(&dir)) return;
        fire_ball(GF_OLD_CONF, dir, p_ptr->lev*3, 3);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
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
        p_ptr->pspeed -= p_ptr->lev/10;
        p_ptr->skills.stl -= p_ptr->lev/10;
    }
    else if (!equip_find_obj(TV_SHIELD, SV_ANY))
    {
        p_ptr->pspeed += 3;
        p_ptr->pspeed += p_ptr->lev/10;
        p_ptr->skills.stl += p_ptr->lev/10;
        if (p_ptr->lev >= 25)
            p_ptr->free_act++;
        /* Ninjas are not archers, and have relatively poor thb skills.
         * However, they excel at throwing (tht)! */
        p_ptr->skill_tht += 30 + p_ptr->lev;
        p_ptr->ambush = 300 + p_ptr->lev*4;
        p_ptr->backstab = 150;
        p_ptr->crit_freq_add = 100; /* add 10.0% to critical frequency since daggers never crit! */
    }
    if (!equip_find_obj(TV_SHIELD, SV_ANY))
    {
        p_ptr->to_a += p_ptr->lev/2 + 5;
        p_ptr->dis_to_a += p_ptr->lev/2 + 5;
    }
    p_ptr->slow_digest = TRUE;
    res_add(RES_FEAR);
    if (p_ptr->lev >= 20) res_add(RES_POIS);
    if (p_ptr->lev >= 25) p_ptr->sustain_dex = TRUE;
    if (p_ptr->lev >= 30) p_ptr->see_inv++;
    if (p_ptr->lev >= 45) res_add(RES_POIS);
    p_ptr->see_nocto = TRUE;
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
        if (p_ptr->lev >= 25)
            add_flag(flgs, OF_FREE_ACT);
    }
    add_flag(flgs, OF_SLOW_DIGEST);
    add_flag(flgs, OF_RES_FEAR);
    if (p_ptr->lev >= 20) add_flag(flgs, OF_RES_POIS);
    if (p_ptr->lev >= 25) add_flag(flgs, OF_SUST_DEX);
    if (p_ptr->lev >= 30) add_flag(flgs, OF_SEE_INVIS);
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
}

static bool _is_critical_blow(plr_attack_ptr context)
{
    int odds = context->technique ? 14 : 27;
    return one_in_(odds);
}
static bool _is_unique(mon_race_ptr race)
{
    if (race->flags1 & RF1_UNIQUE) return TRUE;
    if (race->flags7 & RF7_UNIQUE2) return TRUE;
    return FALSE;
}
static bool _is_fatal_blow(plr_attack_ptr context)
{
    int maxhp = maxroll(context->race->hdice, context->race->hside);
    if (context->mon->hp < maxhp/2 && one_in_(context->blow_ct*10)) return TRUE;
    if (_is_unique(context->race)) return FALSE;
    if (one_in_(666)) return TRUE;
    if (context->technique > 150 && one_in_(11)) return TRUE;
    return FALSE;
}
static void _mod_damage(plr_attack_ptr context)
{
    if (context->info.type != PAT_WEAPON && !mut_present(MUT_DRACONIAN_METAMORPHOSIS)) return;
    if (have_flag(context->info.paf_flags, PAF_ICKY)) return;
    if (!context->dam) return;  /* MON_INVULNER() */
    if (p_ptr->cur_lite <= 0 || one_in_(7))
    {
        if (_is_critical_blow(context))
        {
            context->dam *= 5;
            context->dam_drain *= 2;
            msg_format("<color:R>You critically injured %s!</color>", context->mon_name_obj);
        }
        else if (_is_fatal_blow(context))
        {
            int maxhp = maxroll(context->race->hdice, context->race->hside);
            if (_is_unique(context->race) || context->mon->hp >= maxhp/2)
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
        me.options = CASTER_USE_HP;
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
    if (p_ptr->special_defense & NINJA_KAWARIMI)
    {
        d.name = "Kawarimi";
        d.abbrev = "Kw";
        d.color = TERM_L_BLUE;
    }
    return d;
}

plr_class_ptr ninja_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 45,  24,  36,   8,  48,  32,  70,  35 };
    skills_t xs = { 15,  10,  10,   0,   0,   0,  25,  11 };

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
    }

    return me;
}
