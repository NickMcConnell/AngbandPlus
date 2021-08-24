#include "angband.h"

#include <assert.h>

/**********************************************************************
 * Evolution
 **********************************************************************/
#define _MAX_PER_TIER 10
#define _MAX_TIERS     5

typedef struct {
    int level;
    cptr r_ids[_MAX_PER_TIER];
} _tier_t;

static _tier_t _tiers[_MAX_TIERS] = {
    {  1, { "v.fire", "v.cold", NULL } },
    { 14, { "v.water", "v.energy", NULL } },
    { 27, { "v.nexus", "v.plasma", "v.shimmering", NULL } },
    { 39, { "v.time", "v.shard", "v.death", "v.disintegrate", NULL } },
    { 50, { "v.aether", NULL } },
};

static int _count(cptr list[])
{
    int i;
    for (i = 0; ; i++)
    {
        if (!list[i]) return i;
        assert(mon_race_parse(list[i]));
    }
    /* return 0;  error: missing sentinel ... unreachable */
}

static cptr _random(cptr list[])
{
    if (spoiler_hack)
        return list[0];
    return list[randint0(_count(list))];
}

static int _find(sym_t what, cptr list[])
{
    int i;
    for (i = 0; ; i++)
    {
        cptr s = list[i];
        if (!s) return -1;
        if (sym_equals(what, s)) return i;
    }
    /* return -1;  unreachable */
}

static int _find_tier(sym_t r_id)
{
    int i;
    for (i = 0; i < _MAX_TIERS; i++)
    {
        if (_find(r_id, _tiers[i].r_ids) >= 0) return i;
    }
    return -1;
}

static int _rank(void)
{
    if (plr->current_r_idx)
        return _find_tier(plr->current_r_idx);
    return 0; /* Unborn ... */
}

static cptr _mon_name(int r_idx)
{
    if (plr_mon_race_is_("v.death")) /* This became a Death vortex, but we are being nostalgic ... */
        return "Chaos vortex";
    if (r_idx)
        return mon_race_lookup(r_idx)->name;
    return ""; /* Birth Menu */
}

static void _gain_level(int new_level)
{
    int tier = _find_tier(plr->current_r_idx);
    if (tier < 0 || tier == _MAX_TIERS - 1) return;
    if (plr->lev >= _tiers[tier + 1].level)
    {
        cptr which = _random(_tiers[tier+1].r_ids);
        plr_mon_race_evolve(which);
    }
}

/**********************************************************************
 * Birth
 **********************************************************************/
static void _birth(void)
{
    object_type forge;

    plr_mon_race_set(_random(_tiers[0].r_ids));
    msg_format("You are born a %s.", _mon_name(plr->current_r_idx));

    skills_innate_init("Engulf", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_h = 7;
    forge.to_d = 2;
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_SOFT_ARMOR, SV_LEATHER_JACK));
    plr_birth_obj(&forge);

    plr_birth_obj_aux(TV_STAFF, EFFECT_NOTHING, 1);
}

/**********************************************************************
 * Attacks
 **********************************************************************/
static int _get_effect(void)
{
    mon_race_ptr r = plr_mon_race();
    mon_spell_group_ptr g;
    int gf = GF_NONE, rolls = 1, i;

    if (plr_mon_race_is_("v.death")) return GF_CHAOS; /* XXX */
    if (!r->spells) return gf;

    g = r->spells->groups[MST_BREATH];
    if (!g) return gf;
    if (!g->count) return gf;

    /* XXX we used to have an allocation table for this ... */
    if (plr_mon_race_is_("v.aether"))
        rolls = 3;

    for (i = 0; i < rolls; i++)
    {
        mon_spell_ptr s = &g->spells[randint0(g->count)];
        if (s->id.effect > gf)
            gf = s->id.effect;
    }
    return gf;
}

static void _before_engulf(plr_attack_ptr context)
{
    if (context->info.type == PAT_INNATE && context->blow->method == RBM_ENGULF)
    {
        if (plr_mon_race_is_("v.aether"))
        {
            int i;
            for (i = 0; i < context->blow->effect_ct; i++)
                context->blow->effects[i].type = _get_effect();
        }
    }
}
static void _attack_init(plr_attack_ptr context)
{
    context->hooks.before_hit_f = _before_engulf;
}
static void _calc_innate_bonuses(mon_blow_ptr blow)
{
    if (blow->method == RBM_ENGULF)
    {
        plr->innate_attack_info.blows_calc.wgt = 200;
        plr->innate_attack_info.blows_calc.mul = 40;
        plr->innate_attack_info.blows_calc.max = 400;
        plr_calc_blows_innate(blow);
    }
}
static void _calc_innate_attacks(void)
{
    int l = plr->lev;
    int r = _rank();
    int dd = 3 + r;
    int ds = 3 + r;
    int to_d = r + l/5;
    mon_blow_ptr blow;

    /* Engulf */
    blow = mon_blow_alloc(RBM_ENGULF);
    blow->weight = 150;
    blow->power = l*3/2;
    if (plr_mon_race_is_("v.aether"))
    {
        /* Note: The effect will be randomly chosen on each hit by _before_engulf */
        mon_blow_push_effect(blow, GF_ELEC, dice_create(dd, ds, to_d));
        mon_blow_push_effect(blow, GF_FIRE, dice_create(dd, ds, to_d))->pct = 75;
        mon_blow_push_effect(blow, GF_COLD, dice_create(dd, ds, to_d))->pct = 50;
    }
    else
    {
        mon_blow_push_effect(blow, _get_effect(), dice_create(dd, ds, to_d));
        if (plr_mon_race_is_("v.shard"))
            mon_blow_push_effect(blow, RBE_CUT, dice_create(0, 0, 0))->pct = 16;
    }

    _calc_innate_bonuses(blow);
    vec_add(plr->innate_blows, blow);
}

/**********************************************************************
 * Breath
 **********************************************************************/
static int _breath_effect(void)
{
    return _get_effect();
}

static int _breath_amount(void)
{
    int pct = 10 + plr_prorata_level(15);
    return MAX(5, plr->chp * pct / 100);
}

static cptr _breath_desc(void)
{
    if (plr_mon_race_is_("v.aether"))
        return "almost anything";
    return gf_name(_breath_effect());
}

static void _breathe_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe");
        break;
    case SPELL_DESC:
        var_printf(res, "Breathes %s at your opponent.", _breath_desc());
        break;
    case SPELL_COST_EXTRA: {
        int l = plr->lev;
        int cst = l*l/150;
        var_set_int(res, cst);
        break; }
    default:
        breath_spell_innate(cmd, res, 1 + plr->lev/20, _breath_effect(), _breath_amount());
    }
}

static void _escape_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Elemental Escape");
        break;
    case SPELL_DESC:
        var_set_string(res, "Flee amidst a maelstrom of the elements.");
        break;
    case SPELL_COST_EXTRA:
        _breathe_spell(cmd, res);
        break;
    case SPELL_CAST: {
        int d = _breath_amount()/2;
        int ct = 3;
        int i;
        for (i = 0; i < ct; i++)
            plr_burst(6, _breath_effect(), d);
        teleport_player(30, 0);
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _explode_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Elemental Explosion");
        break;
    case SPELL_DESC: {
        char buf[255];
        sprintf(buf, "Generates a large ball of %s.", _breath_desc());
        var_set_string(res, buf);
        break; }
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _breath_amount()));
        break;
    case SPELL_COST_EXTRA:
        _breathe_spell(cmd, res);
        break;
    case SPELL_CAST: {
        int d = _breath_amount();
        int e = _breath_effect();
        msg_format("You explode in %s.", gf_name(e));
        plr_burst(8, e, d);
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _spin_away_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Spin Away");
        break;
    case SPELL_DESC:
        var_set_string(res, "Spin rapidly towards a random, nearby, visible location.");
        break;
    default:
        strafing_spell(cmd, res);
        break;
    }
}

static void _whirlwind_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Whirlwind");
        break;
    case SPELL_DESC:
        var_set_string(res, "Spin rapidly in place, attacking all adjacent monsters.");
        break;
    default:
        massacre_spell(cmd, res);
        break;
    }
}

static void _unleash_elements_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Unleash Elements");
        break;
    case SPELL_DESC:
        var_set_string(res, "Unleash your breath uncontrollably and at random, though with devastating effect.");
        break;
    case SPELL_COST_EXTRA:
        _breathe_spell(cmd, res);
        break;
    case SPELL_CAST: {
        int num = damroll(5, 3);
        int dam = MAX(1, _breath_amount()/3);
        int i;

        var_set_bool(res, FALSE);
        /* XXX plr_star_ball ... except we want a different gf per launch */
        for (i = 0; i < num; i++)
        {
            int attempts = 1000;
            point_t pos;
            while (attempts--)
            {
                pos = scatter(plr->pos, 4);
                /* XXX This makes long corridor casting extremely OP
                if (!dun_allow_project_at(dun, pos)) continue; */
                if (!dun_plr_at(cave, pos)) break;
            }
            if (attempts < 0) continue;
            plr_rocket(3, pos, _breath_effect(), dam);
        }
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static power_info _powers[] = {
    { A_CON, {  1,  3, 30, _breathe_spell}},
    { A_DEX, {  5,  5, 30, _spin_away_spell}},
    { A_DEX, { 12,  7, 35, _whirlwind_spell}},
    { A_CON, { 25,  7, 40, _explode_spell}},
    { A_DEX, { 30, 10, 40, _escape_spell}},
    { A_CON, { 50, 50, 65, _unleash_elements_spell}},
    {    -1, { -1, -1, -1, NULL}}
};

static int _get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _powers);
}

/**********************************************************************
 * Bonuses
 **********************************************************************/
static void _calc_bonuses(void)
{
    int r = _rank();

    plr->to_a += 5 + 5*r;
    plr->dis_to_a += 5 + 5*r;

    plr->free_act++;
    res_add(GF_CONF);
    res_add_immune(GF_BLIND);
    res_add_immune(GF_FEAR);
    plr->see_nocto = DUN_VIEW_MAX;
    res_add_immune(GF_STUN);
    plr->no_cut = TRUE;
    plr->no_eldritch = TRUE;
    plr->move_random = TRUE;
    plr->hold_life++;
    plr->levitation = TRUE;

    if (plr_mon_race_is_("v.fire"))
    {
        res_add(GF_FIRE);
        res_add(GF_FIRE);
        res_add_vuln(GF_COLD);
        plr->sh_fire = TRUE;
    }
    else if (plr_mon_race_is_("v.cold"))
    {
        res_add(GF_COLD);
        res_add(GF_COLD);
        res_add_vuln(GF_FIRE);
        plr->sh_cold = TRUE;
    }
    else if (plr_mon_race_is_("v.water"))
    {
        plr->pspeed += 1;
        res_add(GF_ACID);
        res_add(GF_ACID);
    }
    else if (plr_mon_race_is_("v.energy"))
    {
        plr->pspeed += 1;
        res_add(GF_ELEC);
        res_add(GF_ELEC);
        plr->sh_elec = TRUE;
    }
    else if (plr_mon_race_is_("v.nexus"))
    {
        plr->pspeed += 3;
        res_add(GF_NEXUS);
        res_add(GF_TELEPORT);
    }
    else if (plr_mon_race_is_("v.plasma"))
    {
        plr->pspeed += 3;
        res_add(GF_FIRE);
        res_add(GF_ELEC);
        res_add(GF_PLASMA);
        plr->sh_fire = TRUE;
        plr->sh_elec = TRUE;
    }
    else if (plr_mon_race_is_("v.shimmering"))
    {
        plr->pspeed += 3;
        res_add(GF_LIGHT);
    }
    else if (plr_mon_race_is_("v.time"))
    {
        plr->pspeed += 7;
        res_add(GF_TIME);
    }
    else if (plr_mon_race_is_("v.shard"))
    {
        plr->pspeed += 5;
        plr->skill_dig += 100;
        res_add(GF_SHARDS);
        plr->sh_shards = TRUE;
    }
    else if (plr_mon_race_is_("v.death"))
    {
        plr->pspeed += 7;
        res_add(GF_CHAOS);
    }
    else if (plr_mon_race_is_("v.disintegrate"))
    {
        plr->pspeed += 5;
        plr->kill_wall = TRUE;
    }
    else if (plr_mon_race_is_("v.aether"))
    {
        plr->pspeed += 10;
        res_add(GF_FIRE);
        res_add(GF_COLD);
        res_add(GF_ACID);
        res_add(GF_ELEC);
        res_add(GF_POIS);
        res_add(GF_LIGHT);
        res_add(GF_DARK);
        res_add(GF_CONF);
        res_add(GF_NETHER);
        res_add(GF_NEXUS);
        res_add(GF_SOUND);
        res_add(GF_SHARDS);
        res_add(GF_CHAOS);
        res_add(GF_DISEN);
        res_add(GF_TIME);
        plr->sh_cold = TRUE;
        plr->sh_fire = TRUE;
        plr->sh_elec = TRUE;
    }
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_FREE_ACT);
    add_flag(flgs, OF_IM_(GF_BLIND));
    add_flag(flgs, OF_IM_(GF_FEAR));
    add_flag(flgs, OF_RES_(GF_CONF));
    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_LEVITATION);

    if (plr_mon_race_is_("v.fire"))
    {
        add_flag(flgs, OF_RES_(GF_FIRE));
        add_flag(flgs, OF_VULN_(GF_COLD));
        add_flag(flgs, OF_AURA_FIRE);
    }
    else if (plr_mon_race_is_("v.cold"))
    {
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_VULN_(GF_FIRE));
        add_flag(flgs, OF_AURA_COLD);
    }
    else if (plr_mon_race_is_("v.water"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_ACID));
        add_flag(flgs, OF_AURA_ELEC);
    }
    else if (plr_mon_race_is_("v.energy"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_ELEC));
        add_flag(flgs, OF_AURA_ELEC);
    }
    else if (plr_mon_race_is_("v.nexus"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_NEXUS));
    }
    else if (plr_mon_race_is_("v.plasma"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_FIRE));
        add_flag(flgs, OF_RES_(GF_ELEC));
        add_flag(flgs, OF_RES_(GF_PLASMA));
        add_flag(flgs, OF_AURA_FIRE);
        add_flag(flgs, OF_AURA_ELEC);
    }
    else if (plr_mon_race_is_("v.shimmering"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_LIGHT));
    }
    else if (plr_mon_race_is_("v.time"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_TIME));
    }
    else if (plr_mon_race_is_("v.shard"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_SHARDS));
        add_flag(flgs, OF_AURA_SHARDS);
    }
    else if (plr_mon_race_is_("v.death"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_CHAOS));
    }
    else if (plr_mon_race_is_("v.disintegrate"))
    {
        add_flag(flgs, OF_SPEED);
    }
    else if (plr_mon_race_is_("v.aether"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_FIRE));
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_RES_(GF_ACID));
        add_flag(flgs, OF_RES_(GF_ELEC));
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
        add_flag(flgs, OF_RES_(GF_TIME));
        add_flag(flgs, OF_AURA_COLD);
        add_flag(flgs, OF_AURA_FIRE);
        add_flag(flgs, OF_AURA_ELEC);
    }
}

/**********************************************************************
 * Public
 **********************************************************************/
plr_race_ptr mon_vortex_get_race(void)
{
    static plr_race_ptr me = NULL;
    int           r = _rank();

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  40,   4,   5,   5,  60,   0};
    skills_t xs = { 40,  35,  75,   0,   0,   0, 125,   0};

        me = plr_race_alloc(RACE_MON_VORTEX);
        me->skills = bs;
        me->extra_skills = xs;

        me->name = "Vortex";
        me->desc = "Vortices are mindless whirlwinds of elemental forces randomly "
                    "moving about the dungeon in search of prey. They are seldom confused "
                    "and never blinded since they lack vision in the normal sense. Indeed, they "
                    "seem to be aware of their surroundings even without eyes and need not wear a "
                    "light source. They can be neither stunned, nor cut, and their mindless "
                    "nature renders them immune to fear. But, alas, they are hopeless with "
                    "magical devices, are rather weak in melee, and, while they may breathe "
                    "almost effortlessly, their breath attacks are rather weak.\n \n"
                    "Vortices do not have normal bodies. Rather they are simply a rapidly "
                    "spinning elemental mass, and as such, they may incorporate objects "
                    "into their rotating essence, gaining bonuses and protection in the process. "
                    "The number of objects they may use increases with level, and there are no "
                    "restrictions on the types of equipment they may 'wield' in each slot. "
                    "While they may 'wield' weapons this way, they cannot attack with them. "
                    "Instead, the vortex may engulf nearby monsters with their elemental essence.\n \n"
                    "Vortices evolve randomly, though all seem to end up with the evolutionary "
                    "perfection of the Aether vortex.";

        me->infra = 0;
        me->exp = 125;
        me->life = 102;
        me->base_hp = 25;
        me->shop_adjust = 120;

        me->hooks.calc_innate_attacks = _calc_innate_attacks;
        me->hooks.calc_innate_bonuses = _calc_innate_bonuses;
        me->hooks.attack_init = _attack_init;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_powers = _get_powers;
        me->hooks.get_flags = _get_flags;
        me->hooks.gain_level = _gain_level;
        me->hooks.birth = _birth;

        me->flags = RACE_IS_MONSTER | RACE_IS_NONLIVING;
        me->pseudo_class_id = CLASS_WARRIOR;
    }

    me->subname = _mon_name(plr->current_r_idx);
    me->stats[A_STR] =  0 + r;
    me->stats[A_INT] = -5;
    me->stats[A_WIS] = -5;
    me->stats[A_DEX] =  2 + r;
    me->stats[A_CON] =  1 + r/2;
    me->stats[A_CHR] =  0;
    me->equip_template = plr_equip_template();

    return me;
}
