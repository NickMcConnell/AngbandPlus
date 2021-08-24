#include "angband.h"


static cptr _desc = 
    "Hydras are monsters with multiple heads. As they evolve, they grow "
    "additional heads granting the potential for extra attacks as well "
    "as the ability to wear more equipment (helmets and amulets). While "
    "not every head may wear a helmet and an amulet (This would be too "
    "powerful), each head is capable of attacking. So the 11-headed hydra "
    "can attack up to 11 times per round, provided it has enough strength "
    "and dexterity to command such a fearsome arsenal.\n \n"
    "Hydras are monsters so do not choose a normal class. Instead, they play "
    "much like warriors with a few limited abilities. All hydras regenerate quickly "
    "and 5 and 7-headed hydras resist poison and gain a few poison based attacks. "
    "9 and 11-headed hydras are creatures of flame: Their attacks burn and they are "
    "capable of breathing fire among other flame based attacks. They resist fire as "
    "well.";

static void _birth(void) 
{ 
    object_type    forge;

    plr_mon_race_set("M.2");
    skills_innate_init("Bite", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);

    object_prep(&forge, lookup_kind(TV_AMULET, 0));
    forge.name2 = EGO_JEWELRY_ELEMENTAL;
    add_flag(forge.flags, OF_RES_(GF_ACID));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_CROWN, SV_IRON_CROWN));
    forge.name2 = EGO_CROWN_MIGHT;
    forge.pval = 1;
    forge.to_a = 5;
    plr_birth_obj(&forge);

    plr_birth_food();
    plr_birth_light();
}

/**********************************************************************
 * Hydra Attacks: Each head can attack (stats permitting) for up to 11 blows!
 **********************************************************************/
static int _head_count(void)
{
    if (plr_mon_race_is_("M.2")) return 2;
    if (plr_mon_race_is_("M.4")) return 4;
    if (plr_mon_race_is_("M.5")) return 5;
    if (plr_mon_race_is_("M.7")) return 7;
    if (plr_mon_race_is_("M.9")) return 9;
    if (plr_mon_race_is_("M.11")) return 11;
    return 2; /* paranoia */
}
static int _bite_effect(void)
{
    if (plr_mon_race_is_("M.5")) return GF_POIS;
    if (plr_mon_race_is_("M.7")) return GF_POIS;
    if (plr_mon_race_is_("M.9")) return GF_FIRE;
    if (plr_mon_race_is_("M.11")) return GF_FIRE;
    return GF_NONE;
}
static void _calc_innate_bonuses(mon_blow_ptr blow)
{
    if (blow->method == RBM_BITE)
    {
        plr->innate_attack_info.blows_calc.wgt = 150;
        plr->innate_attack_info.blows_calc.mul = 45 + plr->lev/5;
        plr->innate_attack_info.blows_calc.max = 100 * _head_count();
        plr_calc_blows_innate(blow);
    }
}
static void _calc_innate_attacks(void)
{
    int l = plr->lev;
    int dd = 1 + (l+3)/14;
    int ds = 4 + l/16;
    mon_blow_ptr blow = mon_blow_alloc(RBM_BITE);

    blow->weight = 100 + l*2;
    blow->power = l*3/2;
    mon_blow_push_effect(blow, RBE_HURT, dice_create(dd, ds, 0));
    if (_bite_effect())
        mon_blow_push_effect(blow, _bite_effect(), dice_create(dd, ds, 0));
    _calc_innate_bonuses(blow);
    vec_push(plr->innate_blows, blow);
}

/**********************************************************************
 * Hydra Breath
 **********************************************************************/
static int _breath_effect(void)
{
    if (plr_mon_race_is_("M.9") || plr_mon_race_is_("M.11"))
        return GF_FIRE;
    return GF_POIS;
}

static int _breath_amount(void)
{
    int l = plr->lev;
    return MIN(375, plr->chp * (20 + l*l*l*15/125000) / 100);
}

static int _breath_cost(void)
{
    int l = plr->lev;
    return MAX(l/2 + l*l*15/2500, 1);
}

static void _breathe_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe");
        break;
    case SPELL_DESC:
        var_printf(res, "Breathes %s at your opponent.", gf_name(_breath_effect()));
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, _breath_cost());
        break;
    default:
        breath_spell_innate(cmd, res, 2, _breath_effect(), _breath_amount());
    }
}

/**********************************************************************
 * Hydra Powers
 **********************************************************************/
static power_info _fire_powers[] = {
    { A_CHR, {  1,  3, 30, scare_monster_spell} },
    { A_STR, { 20,  5, 30, fire_bolt_spell} },
    { A_CON, { 30,  0, 50, _breathe_spell} },
    { A_STR, { 45, 10, 50, fire_ball_spell} },
    { A_STR, { 45, 15, 50, plasma_bolt_spell} },
    {    -1, { -1, -1, -1, NULL}}
};

static power_info _poison_powers[] = {
    { A_CHR, {  1,  3, 30, scare_monster_spell} },
    { A_STR, { 20,  3, 30, stinking_cloud_spell} },
    { A_CON, { 30,  0, 50, _breathe_spell} },
    {    -1, { -1, -1, -1, NULL}}
};

static int _get_powers(spell_info* spells, int max) 
{
    if (plr_mon_race_is_("M.9") || plr_mon_race_is_("M.11"))
        return get_powers_aux(spells, max, _fire_powers);
    return get_powers_aux(spells, max, _poison_powers);
}

/**********************************************************************
 * Hydra Bonuses
 **********************************************************************/
static void _calc_bonuses(void) 
{
    int ac = 20 + plr->lev/10;

    plr->skill_dig += 50;
    plr->to_a += ac;
    plr->dis_to_a += ac;
    plr->regen += 100 + 4*plr->lev;

    if (plr_mon_race_is_("M.4"))
        plr->pspeed += 2;
    else if (plr_mon_race_is_("M.5"))
    {
        plr->pspeed += 3;
        res_add(GF_POIS);
    }
    else if (plr_mon_race_is_("M.7"))
    {
        plr->pspeed += 5;
        res_add(GF_POIS);
    }
    else if (plr_mon_race_is_("M.9"))
    {
        plr->pspeed += 7;
        res_add(GF_FIRE);
    }
    else if (plr_mon_race_is_("M.11"))
    {
        plr->pspeed += 10;
        res_add(GF_FIRE);
    }
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE]) 
{
    add_flag(flgs, OF_REGEN);
    if (plr_mon_race_is_("M.4"))
        add_flag(flgs, OF_SPEED);
    else if (plr_mon_race_is_("M.5"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_POIS));
    }
    else if (plr_mon_race_is_("M.7"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_POIS));
    }
    else if (plr_mon_race_is_("M.9"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_FIRE));
    }
    else if (plr_mon_race_is_("M.11"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_FIRE));
    }
}

/**********************************************************************
 * Hydra Evolution
 **********************************************************************/
static void _gain_level(int new_level) 
{
    if (plr_mon_race_is_("M.2") && new_level >= 10)
        plr_mon_race_evolve("M.4");
    if (plr_mon_race_is_("M.4") && new_level >= 20)
        plr_mon_race_evolve("M.5");
    if (plr_mon_race_is_("M.5") && new_level >= 30)
        plr_mon_race_evolve("M.7");
    if (plr_mon_race_is_("M.7") && new_level >= 37)
        plr_mon_race_evolve("M.9");
    if (plr_mon_race_is_("M.9") && new_level >= 45)
        plr_mon_race_evolve("M.11");
}

/**********************************************************************
 * Public Methods
 **********************************************************************/
plr_race_ptr mon_hydra_get_race(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[6] =  {"2-headed hydra", "4-headed hydra", "5-headed hydra", "7-headed hydra", "9-headed hydra", "11-headed hydra"};    
    int           rank = 0;

    if (plr->lev >= 10) rank++;
    if (plr->lev >= 20) rank++;
    if (plr->lev >= 30) rank++;
    if (plr->lev >= 37) rank++;
    if (plr->lev >= 45) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  21,  35,   1,  10,   7,  62,  30};
    skills_t xs = { 60,  50,  50,   0,   0,   0, 125,  35};

        me = plr_race_alloc(RACE_MON_HYDRA);

        me->name = "Hydra";
        me->desc = _desc;
        me->skills = bs;
        me->extra_skills = xs;
        me->infra = 5;
        me->exp = 130;
        me->base_hp = 45;
        me->shop_adjust = 130;
        me->pseudo_class_id = CLASS_WARRIOR;
        me->boss_r_idx = mon_race_parse("M.Lernean")->id;
        me->flags = RACE_IS_MONSTER;

        me->hooks.get_powers = _get_powers;
        me->hooks.calc_innate_attacks = _calc_innate_attacks;
        me->hooks.calc_innate_bonuses = _calc_innate_bonuses;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.gain_level = _gain_level;
        me->hooks.birth = _birth;
    }

    me->subname = NULL;
    if (!birth_hack && !spoiler_hack)
        me->subname = titles[rank];
    me->stats[A_STR] = rank;
    me->stats[A_INT] = -2;
    me->stats[A_WIS] = -2;
    me->stats[A_DEX] = (rank + 1)/2;
    me->stats[A_CON] = rank;
    me->stats[A_CHR] =  0;
    me->life = 100 + 3*rank;
    me->equip_template = plr_equip_template();

    return me;
}
