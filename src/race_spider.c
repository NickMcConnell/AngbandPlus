#include "angband.h"

#include <assert.h>

/**********************************************************************
 * Spider: Cave Spider -> Giant Spider -> Phase Spider 
 * XXX I removed the Aranea forms, mostly because plr->current_r_idx
 * is now assumed to point to the correct mon_race. Still, there is
 * room for a Jump Spider flavor with random evolution ... XXX
 **********************************************************************/

static cptr _desc = 
    "Spiders are stealthy monsters that prefer to wait for their prey. "
    "They have poisonous attacks and can weave sticky webs to entangle "
    "their enemies. They have 8 legs, 4 of which may wear rings and the "
    "remaining may equip 2 pairs of boots. They are unable to wield "
    "weapons or bows, but may wear a helmet, an amulet, a cloak and "
    "even a suit of body armor (after slight adjustments, of course). "
    "They cannot wield a light source, but this is not an issue as spiders "
    "have grown accustomed to the dark.\n \n"
    "Spiders begin in the very weak form of the Cave Spider. Lacking "
    "any special talents, the cave spider must play carefully if it is to "
    "survive.";

/**********************************************************************
 * Spider Attacks
 **********************************************************************/
static void _calc_innate_bonuses(mon_blow_ptr blow)
{
    if (blow->method == RBM_BITE)
    {
        plr->innate_attack_info.blows_calc.wgt = 150;
        plr->innate_attack_info.blows_calc.mul = 35;
        plr->innate_attack_info.blows_calc.max = 500;
        plr_calc_blows_innate(blow);
    }
}
static dice_t _calc_dice(int dam, int pct_dice)
{
    dice_t dice = {0};
    int dice_dam = dam * pct_dice; /* scaled by 100 */
    int x; /* scaled by sqrt(100) = 10 */
    x = mysqrt(6*dice_dam);
    dice.dd = MAX(1, (x + 5)/27);
    dice.ds = MAX(5, (x + 5)/13);
    dice.base = MAX(0, dam - dice_avg_roll(dice));
    return dice;
}
static dice_t _dice(void)
{
    int l = plr->lev;
    if (plr_mon_race_is_("S.cave")) return dice_create(1, 5, 0);
    if (plr_mon_race_is_("S.giant")) return dice_create(1, 10, l/5);
    if (plr_mon_race_is_("S.phase")) return _calc_dice(10 + 25*(l - 25)/25, 35);
    return dice_create(0, 0, 1);
}
static int _weight(void)
{
    return 70;
}
static void _calc_innate_attacks(void)
{
    mon_blow_ptr blow = mon_blow_alloc(RBM_BITE);
    blow->weight = _weight();
    blow->power = plr->lev;
    mon_blow_push_effect(blow, RBE_HURT, _dice());
    if (plr->lev >= 10)
        mon_blow_push_effect(blow, GF_SLEEP, dice_create(0, 0, 0));
    _calc_innate_bonuses(blow);
    vec_add(plr->innate_blows, blow);
}
static void _calc_bonuses(void)
{
    res_add(GF_POIS);
    plr->see_nocto = DUN_VIEW_MAX;
    plr->pass_web = TRUE;
    if (plr->lev >= 10)
    {
        add_flag(plr->innate_attack_info.obj_flags, OF_BRAND_POIS);
        add_flag(plr->innate_attack_info.obj_known_flags, OF_BRAND_POIS);
    }
}

/**********************************************************************
 * Spider Powers
 **********************************************************************/
static void _detect_prey_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Prey");
        break;
    case SPELL_DESC:
        var_set_string(res, "Looks for a nearby meal.");
        break;
    case SPELL_CAST:
        detect_monsters_living(DETECT_RAD_DEFAULT, "You sense potential prey!");
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static bool _place_web(point_t pos)
{
    if (!dun_pos_interior(cave, pos)) return FALSE;
    if (dun_mon_at(cave, pos)) return FALSE;
 /* if (dun_obj_at(dun, pos)) break; cf _floor_affect(GF_WEB) */
    return dun_place_web(cave, pos);
}

static int _web_attempts(void)
{
    if (plr->lev <= 25) return 0; /* possessor spider forms (e.g. Cave Spider) */
    return 8 * (plr->lev - 25) / 25;
}

void spider_web_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Spider Web");
        break;
    case SPELL_DESC:
        var_set_string(res, "Weaves a fine, silky web which obstructs the movement of all save spiders.");
        break;
    case SPELL_CAST:
        if (plr->lev >= 50)
            plr_burst(1, GF_WEB, 0);
        else
        {
            int i, ct = _web_attempts();
            _place_web(plr->pos);
            for (i = 0; i < ct; i++)
                _place_web(point_random_step(plr->pos));
        }
        plr->update |= PU_FLOW;
        plr->redraw |= PR_MAP;
        var_set_bool(res, TRUE);
        break;
    case SPELL_COST_EXTRA:
        if (plr->lev >= 50)
            var_set_int(res, 20);
        else
            var_set_int(res, _web_attempts() * 2);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

/**********************************************************************
 * Cave Spider
 **********************************************************************/
static power_info _cave_spider_powers[] = {
    { A_DEX, {  1,  1, 30, _detect_prey_spell } },
    {    -1, { -1, -1, -1, NULL } }
};
static int _cave_spider_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _cave_spider_powers);
}
static void _cave_spider_calc_bonuses(void)
{
    plr->pspeed += 3;
    res_add(GF_DARK);
    res_add_vuln(GF_LIGHT);
    _calc_bonuses();
}
static void _cave_spider_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_RES_(GF_DARK));
    add_flag(flgs, OF_VULN_(GF_LIGHT));
}
plr_race_ptr _cave_spider_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  40,  38,   6,  20,  15,  56,  30};
    skills_t xs = { 40,  75,  50,   0,   0,   0,  90,  35};

        me = plr_race_alloc(RACE_MON_SPIDER);

        me->subname = "Cave Spider";
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 70;
        me->infra = 5;

        me->stats[A_STR] = -2;
        me->stats[A_INT] = -1;
        me->stats[A_WIS] = -1;
        me->stats[A_DEX] =  2;
        me->stats[A_CON] = -2;
        me->stats[A_CHR] = -3;
        
        me->hooks.get_powers = _cave_spider_get_powers;
        me->hooks.calc_bonuses = _cave_spider_calc_bonuses;
        me->hooks.get_flags = _cave_spider_get_flags;
    }
    return me;
}

/**********************************************************************
 * Giant Spider
 **********************************************************************/
static void _giant_spider_calc_bonuses(void)
{
    _calc_bonuses();
}
static void _giant_spider_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_POIS));
}
plr_race_ptr _giant_spider_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  40,  38,   7,  20,  15,  56,  30};
    skills_t xs = { 40,  75,  50,   0,   0,   0,  90,  35};

        me = plr_race_alloc(RACE_MON_SPIDER);

        me->subname = "Giant Spider";
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 100;
        me->infra = 5;

        me->stats[A_STR] =  0;
        me->stats[A_INT] = -1;
        me->stats[A_WIS] = -1;
        me->stats[A_DEX] =  2;
        me->stats[A_CON] =  1;
        me->stats[A_CHR] = -1;
        
        me->hooks.get_powers = _cave_spider_get_powers;
        me->hooks.calc_bonuses = _giant_spider_calc_bonuses;
        me->hooks.get_flags = _giant_spider_get_flags;
    }
    return me;
}

/**********************************************************************
 * Phase Spider
 **********************************************************************/
static void _phase_shield_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Phase Shield");
        break;
    case SPELL_DESC:
        var_set_string(res, "Teleport as you receive an attack, potentially escaping damage altogether.");
        break;
    case SPELL_CAST:
        if (!(plr->special_defense & NINJA_KAWARIMI))
        {
            msg_print("You prepare to evade any attacks.");
            plr->special_defense |= NINJA_KAWARIMI;
            plr->redraw |= PR_STATUS;
        }
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static power_info _phase_spider_powers[] = {
    { A_DEX, {  1,  1, 30, _detect_prey_spell } },
    { A_DEX, {  5,  1, 30, phase_door_spell } },
    { A_DEX, { 10,  5, 30, teleport_spell } },
    { A_DEX, { 12, 10, 60, spider_web_spell } },
    { A_DEX, { 25, 10, 30, teleport_to_spell } },
    { A_DEX, { 30, 15, 50, teleport_level_spell } },
    { A_DEX, { 35, 20, 55, teleport_other_spell } },
    { A_DEX, { 40, 25, 60, _phase_shield_spell } },
    { A_DEX, { 50, 30, 60, dimension_door_spell } },
    {    -1, { -1, -1, -1, NULL } }
};
static int _phase_spider_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _phase_spider_powers);
}
static void _phase_spider_calc_bonuses(void)
{
    plr->pspeed += 5 + (plr->lev - 25)/5;
    if (plr->lev >= 50)
        res_add_immune(GF_POIS);
    else
        res_add(GF_POIS);
    res_add(GF_NEXUS);
    res_add(GF_CONF);
    res_add_immune(GF_TELEPORT);
    plr->free_act++;
    _calc_bonuses();
}
static void _phase_spider_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SPEED);
    add_flag(flgs, OF_RES_(GF_NEXUS));
    add_flag(flgs, OF_RES_(GF_CONF));
    add_flag(flgs, OF_FREE_ACT);

    if (plr->lev >= 50)
        add_flag(flgs, OF_IM_(GF_POIS));
    else
        add_flag(flgs, OF_RES_(GF_POIS));
}
static status_display_t _phase_spider_status_display(void)
{
    status_display_t d = {0};
    if (plr->special_defense & NINJA_KAWARIMI)
    {
        d.name = "Phase Shield";
        d.abbrev = "Ps";
        d.color = TERM_L_BLUE;
    }
    return d;
}
plr_race_ptr _phase_spider_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  40,  38,  10,  25,  15,  56,  30};
    skills_t xs = { 40,  75,  50,   0,   0,   0,  90,  35};

        me = plr_race_alloc(RACE_MON_SPIDER);

        me->subname = "Phase Spider";
        me->skills = bs;
        me->extra_skills = xs;
        me->life =  90;
        me->infra =  5;

        me->stats[A_STR] =  0;
        me->stats[A_INT] =  3;
        me->stats[A_WIS] = -1;
        me->stats[A_DEX] =  3;
        me->stats[A_CON] =  1;
        me->stats[A_CHR] = -1;
        
        me->hooks.get_powers = _phase_spider_get_powers;
        me->hooks.calc_bonuses = _phase_spider_calc_bonuses;
        me->hooks.get_flags = _phase_spider_get_flags;
        me->hooks.status_display = _phase_spider_status_display;
    }
    return me;
}

static void _gain_level(int new_level)
{
    if (plr_mon_race_is_("S.cave") && new_level >= 10)
        plr_mon_race_evolve("S.giant");
    if (plr_mon_race_is_("S.giant") && new_level >= 25)
        plr_mon_race_evolve("S.phase");
}

static void _birth(void)
{
    object_type forge;

    plr_mon_race_set("S.cave");
    skills_innate_init("Bite", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_h = 7;
    forge.to_d = 2;
    forge.pval = 1;
    add_flag(forge.flags, OF_DEX);
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL));
    plr_birth_obj(&forge);

    plr_birth_food();
}

static name_desc_t _info[SPIDER_MAX] = {
    { "Phase Spider", "Phase Spiders have unsurpassed powers of teleportation and average offense." },
};

plr_race_ptr mon_spider_get_race(int psubrace)
{
    plr_race_ptr result = NULL;

    if (birth_hack && psubrace >= SPIDER_MAX)
        psubrace = 0;

    assert(0 <= psubrace && psubrace < SPIDER_MAX);

    if (plr_mon_race_is_("S.cave")) result =  _cave_spider_get_race_t();
    else if (plr_mon_race_is_("S.giant")) result =  _giant_spider_get_race_t();
    else if (plr_mon_race_is_("S.phase")) result =  _phase_spider_get_race_t();
    else result =  _cave_spider_get_race_t(); /* birth */

    result->name = "Spider";
    result->desc = _desc;
    result->exp = 200;
    result->equip_template = plr_equip_template();
    result->flags = RACE_IS_MONSTER;
    result->hooks.gain_level = _gain_level;
    result->hooks.birth = _birth;
    result->base_hp = 25;
    result->pseudo_class_id = CLASS_ROGUE;
    result->shop_adjust = 115;

    result->boss_r_idx = mon_race_parse("S.Ungoliant")->id;

    if (birth_hack || spoiler_hack)
    {
        result->subname = _info[psubrace].name;
        result->subdesc = _info[psubrace].desc;
    }

    result->hooks.calc_innate_attacks = _calc_innate_attacks;
    result->hooks.calc_innate_bonuses = _calc_innate_bonuses;

    return result;
}
