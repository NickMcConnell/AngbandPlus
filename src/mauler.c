#include "angband.h"

/****************************************************************
 * Timers
 ****************************************************************/
enum { _KILLING_SPREE = T_CUSTOM };
static bool _killing_spree_on(plr_tim_ptr timer)
{
    msg_print("You go on a killing spree!");
    return TRUE;
}
static void _killing_spree_off(plr_tim_ptr timer)
{
    msg_print("You have seen enough blood and suffering for now.");
}
static status_display_t _killing_spree_display(plr_tim_ptr timer)
{
    return status_display_create("Spree", "Ks", TERM_VIOLET);
}
static plr_tim_info_ptr _killing_spree(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_KILLING_SPREE, "Killing Spree");
    info->desc = "Killing monsters makes you faster.";
    info->on_f = _killing_spree_on;
    info->off_f = _killing_spree_off;
    info->status_display_f = _killing_spree_display;
    info->flags = TF_IGNORE;
    return info;
}
static void _register_timers(void)
{
    plr_tim_register(_killing_spree());
}
static void _kill_monster(mon_ptr mon)
{
    if (plr_tim_find(_KILLING_SPREE))
        plr_tim_augment(T_FAST, 10);
}

/****************************************************************
 * Helpers
 ****************************************************************/

static bool _weapon_check(void)
{
    /* Fail if any weapon is not wielded with 2 hands */
    int hand;
    for (hand = 0; hand < MAX_HANDS; hand++)
    {
        if ( plr->attack_info[hand].type == PAT_WEAPON
          && !have_flag(plr->attack_info[hand].paf_flags, PAF_TWO_HANDS) )
        {
            return FALSE;
        }
    }
    /* Fail if there is no wielded weapon */
    return equip_find_first(obj_is_weapon);
}

static int _get_toggle(void)
{
    return plr->magic_num1[0];
}

static int _set_toggle(s32b toggle)
{
    int result = plr->magic_num1[0];

    if (toggle == result) return result;

    plr->magic_num1[0] = toggle;

    plr->redraw |= PR_STATUS;
    plr->update |= PU_BONUS;
    handle_stuff();

    return result;
}

int mauler_get_toggle(void)
{
    /* exposed for prtstatus() in xtra1.c 
       this is easier than rewriting the status code so that classes can maintain it!
    */
    int result = TOGGLE_NONE;
    if (plr->pclass == CLASS_MAULER && _weapon_check())
        result = _get_toggle();
    return result;
}

/****************************************************************
 * Melee: We use global hooks for Splatter
 ****************************************************************/
enum {
    _CRUSHING_BLOW = PLR_HIT_CUSTOM,
    _SCATTER,
    _MAUL,
    _KNOCKBACK,
};
static bool _begin_weapon(plr_attack_ptr context)
{
    if (context->info.type != PAT_WEAPON) return TRUE;
    switch (context->mode)
    {
    case PLR_HIT_CRIT:
        context->info.crit.qual_add += 250*plr->lev/50;
        break;
    }
    switch (_get_toggle())
    {
    case MAULER_TOGGLE_DRAIN:
        add_flag(context->obj_flags, OF_BRAND_VAMP);
        break;
    case MAULER_TOGGLE_SHATTER:
        add_flag(context->obj_flags, OF_IMPACT);
        break;
    }
    return TRUE;
}
static void _mod_blows(plr_attack_ptr context)
{
    if (context->info.type != PAT_WEAPON) return;
    switch (context->mode)
    {
    case PLR_HIT_CRIT: {
        int blows = NUM_BLOWS(context->info.which);
        if (blows > 100)
            blows = 100 + (blows - 100) / 2;
        context->blow_ct = blows/100;
        if (randint0(100) < blows%100) context->blow_ct++;
        break; }
    case _CRUSHING_BLOW:
    case PLR_HIT_STUN:
    case _KNOCKBACK:
    case _SCATTER:
        context->blow_ct = 1;
        break;
    }
}
static void _mod_damage(plr_attack_ptr context)
{
    if (context->info.type != PAT_WEAPON) return;
    switch (context->mode)
    {
    case _CRUSHING_BLOW:
        context->dam = context->dam * NUM_BLOWS(context->info.which) / 50;
        context->dam_drain *= 2;
        break;
    }
}
static void _after_hit(plr_attack_ptr context)
{
    if (context->info.type != PAT_WEAPON) return;
    if (_get_toggle() == MAULER_TOGGLE_SPLATTER && context->stop == STOP_MON_DEAD)
        dun_burst(plr_dun(), who_create_plr(), 2, context->mon_pos, GF_BLOOD, context->dam);
    switch (context->mode)
    {
    case _KNOCKBACK:
        context->do_knockback = 8;
        break;
    case _SCATTER:
        context->do_knockback = 3;
        break;
    }
}
static void _end_weapon(plr_attack_ptr context)
{
    if (context->info.type != PAT_WEAPON) return;
}
static void _attack_init(plr_attack_ptr context)
{
    context->hooks.begin_weapon_f = _begin_weapon;
    context->hooks.mod_blows_f = _mod_blows;
    context->hooks.mod_damage_f = _mod_damage;
    context->hooks.after_hit_f = _after_hit;
    context->hooks.end_weapon_f = _end_weapon;
}
static void _blow_spell(int mode, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, plr_attack_special(mode, PAC_NO_INNATE));
        break;
    default:
        default_spell(cmd, res);
    }
}

/****************************************************************
 * Spells
 ****************************************************************/
static void _toggle_spell(int which, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, FALSE);
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

static void _block_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Block");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique, you will gain an AC bonus based on the weight of your current weapon.");
        break;
    default:
        _toggle_spell(MAULER_TOGGLE_BLOCK, cmd, res);
        break;
    }
}

static void _close_in_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Close In");
        break;
    case SPELL_DESC:
        var_set_string(res, "Close in for the kill on a nearby monster.");
        break;
    case SPELL_CAST:
    {
        bool dummy;
        rush_attack(2, &dummy);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _critical_blow_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Critical Blow");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent opponent with a devastating blow.");
        break;
    default:
        _blow_spell(PLR_HIT_CRIT, cmd, res);
    }
}

static void _crushing_blow_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Crushing Blow");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent opponent with a single powerful blow.");
        break;
    default:
        _blow_spell(_CRUSHING_BLOW, cmd, res);
    }
}

static void _detect_ferocity_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Ferocity");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects all monsters except mindless in your vicinity.");
        break;
    case SPELL_CAST:
        detect_monsters_mind(DETECT_RAD_DEFAULT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
    
static void _drain_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Drain");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique you will drain life from your foes. In addition, enemies will never fully recover from the wounds you inflict.");
        break;
    default:
        _toggle_spell(MAULER_TOGGLE_DRAIN, cmd, res);
        break;
    }
}

static void _killing_spree_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Killing Spree");
        break;
    case SPELL_DESC:
        var_set_string(res, "Engage in wanton destruction! During this time, any foe you slay will haste you, so seek to kill as many enemies as possible!");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr_tim_find(_KILLING_SPREE))
        {
            msg_print("You are already on a Killing Spree. Show some mercy!");
            return;
        }
        plr_tim_add(_KILLING_SPREE, 44);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _knockback_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Knockback");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent opponent with a single blow. If landed, your foe will be knocked back away from you.");
        break;
    default:
        _blow_spell(_KNOCKBACK, cmd, res);
    }
}

static void _maul_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Maul");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique you will maul your opponents.");
        break;
    default:
        _toggle_spell(MAULER_TOGGLE_MAUL, cmd, res);
        break;
    }
}

void _scatter_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Scatter");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack all adjacent monsters with a single strike. If landed, your enemies will be scattered away from you.");
        break;
    case SPELL_CAST:
    {
        int dir;
        for (dir = 0; dir < 8; dir++)
        {
            point_t pos = point_step(plr->pos, ddd[dir]);
            dun_cell_ptr cell = dun_cell_at(cave, pos);
            mon_ptr mon = dun_mon_at(cave, pos);
            if (mon && (mon->ml || cell_project(cell)))
            {
                plr_attack_t context = {0};
                context.mode = _SCATTER;
                plr_attack(&context, pos);
            }
        }
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _shatter_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shatter");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique, your weapon will cause earthquakes.");
        break;
    default:
        _toggle_spell(MAULER_TOGGLE_SHATTER, cmd, res);
        break;
    }
}

static void _smash_wall_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Smash");
        break;
    case SPELL_DESC:
        var_set_string(res, "Destroys adjacent targeted wall, door, tree, or trap.");
        break;
    case SPELL_CAST:
    {
        int dir;
        point_t pos;
        
        var_set_bool(res, FALSE);
        if (!get_rep_dir2(&dir)) return;
        if (dir == 5) return;

        pos = point_step(plr->pos, dir);
        
        if (!dun_pos_interior(cave, pos)) return;

        if (!dun_tunnel(cave, pos, ACTION_FORCE|ACTION_QUIET))
            plr_ball_direct(0, pos, GF_KILL_DOOR, 0);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _splatter_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Splatter");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique, monsters will explode as you kill them.");
        break;
    default:
        _toggle_spell(MAULER_TOGGLE_SPLATTER, cmd, res);
        break;
    }
}

void stunning_blow_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Stunning Blow");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent opponent with a single blow aimed to stun.");
        break;
    default:
        _blow_spell(PLR_HIT_STUN, cmd, res);
    }
}

static void _tunnel_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Tunnel");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique you may move through walls.");
        break;
    default:
        _toggle_spell(MAULER_TOGGLE_TUNNEL, cmd, res);
        break;
    }
}

/****************************************************************
 * Spell Table and Exports
 ****************************************************************/
static spell_info _spells[] = 
{
    /*lvl cst fail spell */
    {  5,  5, 30, _smash_wall_spell},
    {  8,  5, 30, _detect_ferocity_spell},
    { 12, 10,  0, _critical_blow_spell},
    { 15,  0,  0, _splatter_spell},
    { 17,  0,  0, _block_spell},
    { 21, 15, 40, _close_in_spell},
    { 23, 20,  0, _knockback_spell},
    { 30,  0,  0, _shatter_spell},
    { 32, 30, 50, _killing_spree_spell},
    { 35, 30, 50, _scatter_spell},
    { 37,  0,  0, _tunnel_spell},
    { 40, 50,  0, _crushing_blow_spell},
    { 42,  0,  0, _drain_spell},
    { 45,  0,  0, _maul_spell},
    { -1, -1, -1, NULL}
};

static int _get_spells(spell_info* spells, int max)
{
    int ct;

    if (!_weapon_check())
    {
        msg_print("Rargh! You need to wield a single weapon with both hands to properly maul stuff!");
        return 0;
    }

    ct = get_spells_aux(spells, max, _spells);
    
    if (ct == 0)
        msg_print("Rargh! Go maul something for more experience!");

    return ct;
}

static void _calc_bonuses(void)
{
    int w = equip_weight(obj_is_weapon);
    if (_weapon_check())
    {
        if (_get_toggle() == MAULER_TOGGLE_BLOCK)
        {
            int a = w/20 + (w/100)*(w/100);
            plr->to_a += a;
            plr->dis_to_a += a;
        }

        /* TODO: This should cost more energy too ... */
        if (_get_toggle() == MAULER_TOGGLE_TUNNEL)
            plr->kill_wall = TRUE;

        if (_get_toggle() == MAULER_TOGGLE_MAUL)
        {
            plr->to_a -= 20;
            plr->dis_to_a -= 20;
        }
    }
}

static void _calc_weapon_bonuses(object_type *o_ptr, plr_attack_info_ptr info)
{
    if (_weapon_check())
    {
        /* CL1: Mighty */
        if (plr->lev >= 1)
        {
            int w = o_ptr->weight;
            int h = (w - 150)/20;
            int d = (w - 150)/10;
            int crit_pct = MIN((w - 200)/20, 20);

            if (_get_toggle() != MAULER_TOGGLE_BLOCK)
            {
                info->to_h += h;
                info->dis_to_h += h;

                info->to_d += d;
                info->dis_to_d += d;
            }

            /* Destroyer */
            if (_get_toggle() == MAULER_TOGGLE_MAUL)
                crit_pct += 10;
            info->crit.freq_add += crit_pct * 10;
            info->crit.qual_add += 650 * crit_pct / 100;
        }
        /* CL25: Impact 
            20lb +1d0
            25lb +1d1
            30lb +2d1
            40lb +2d2
            50lb +3d2
            60lb +3d3

            Stagger in bonuses with level. Also, I'm debating > rather than >= for cutoffs.
        */
        if (plr->lev >= 25 )
        {
            int w = o_ptr->weight;

            if (w >= 200)
                info->to_dd++;
            if (plr->lev >= 30 && w >= 250)
                info->to_ds++;
            if (plr->lev >= 35 && w >= 300)
                info->to_dd++;
            if (plr->lev >= 40 && w >= 400)
                info->to_ds++;
            if (plr->lev >= 45 && w >= 500)
                info->to_dd++;
            if (plr->lev >= 50 && w >= 600)
                info->to_ds++;
        }

        if (_get_toggle() == MAULER_TOGGLE_MAUL)
        {
            info->to_dd += 1;
            info->to_ds += 1;
        }
    }
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "technique";
        me.options = CASTER_USE_HP;
        me.which_stat = A_STR;
        init = TRUE;
    }
    return &me;
}

static void _character_dump(doc_ptr doc)
{
    if (_weapon_check() && plr->lev >= 5)
    {
        spell_info spells[MAX_SPELLS];
        int        ct = _get_spells(spells, MAX_SPELLS);

        plr_display_spells(doc, spells, ct);
    }
}
static status_display_t _status_display(void)
{
    status_display_t d = {0};
    switch (_get_toggle())
    {
    case MAULER_TOGGLE_BLOCK:
        d.name = "Block"; d.abbrev = "Bl"; d.color = TERM_L_BLUE;
        break;
    case MAULER_TOGGLE_SHATTER:
        d.name = "Quake"; d.abbrev = "Qk"; d.color = TERM_YELLOW;
        break;
    case MAULER_TOGGLE_TUNNEL:
        d.name = "Tunnel"; d.abbrev = "Tn"; d.color = TERM_L_DARK;
        break;
    case MAULER_TOGGLE_DRAIN:
        d.name = "Drain"; d.abbrev = "Dr"; d.color = TERM_RED;
        break;
    case MAULER_TOGGLE_MAUL:
        d.name = "Maul"; d.abbrev = "Ml"; d.color = TERM_RED;
        break;
    case MAULER_TOGGLE_SPLATTER:
        d.name = "Splatter"; d.abbrev = "*"; d.color = TERM_RED;
        break;
    }
    return d;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_TWO_HANDED_SWORD, 1);
    plr_birth_obj_aux(TV_HARD_ARMOR, SV_CHAIN_MAIL, 1);
    plr_birth_obj_aux(TV_BOOTS, SV_PAIR_OF_METAL_SHOD_BOOTS, 1);
}

plr_class_ptr mauler_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  25,  35,   0,  14,   2,  70,  20 };
    skills_t xs = { 35,  45,  60,   0,   0,   0, 150,  35 };

        me = plr_class_alloc(CLASS_MAULER);
        me->name = "Mauler";
        me->desc = 
        "The Mauler favors extremely heavy weapons, and possesses powerful abilities whose "
            "effectiveness depends on the weight of the weapon. While they only gain a limited "
            "number of blows which can never be magically increased, they are capable of "
            "hitting opponents very hard to make the most of each strike. The Mauler is "
            "required to use both hands on a single weapon when wielding if they wish their "
            "talents to function properly.",

        me->stats[A_STR] =  5;
        me->stats[A_INT] = -2;
        me->stats[A_WIS] = -2;
        me->stats[A_DEX] = -1;
        me->stats[A_CON] =  3;
        me->stats[A_CHR] =  2;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 111;
        me->base_hp = 18;
        me->exp = 120;
        me->pets = 40;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_STRONG;

        me->hooks.birth = _birth;
        me->hooks.attack_init = _attack_init;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_spells = _get_spells;
        me->hooks.character_dump = _character_dump;
        me->hooks.register_timers = _register_timers;
        me->hooks.kill_monster = _kill_monster;
        me->hooks.status_display = _status_display;
    }

    return me;
}
