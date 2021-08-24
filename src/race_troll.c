#include "angband.h"

#include <assert.h>

/******************************************************************************
 * Troll Bite
 ******************************************************************************/
static void _calc_innate_attacks(void) 
{
    mon_race_ptr race = plr_mon_race();
    mon_blow_ptr blow = mon_blows_find(race->blows, RBM_BITE);

    if (blow)
    {
        mon_blow_ptr copy = mon_blow_copy(blow);
        copy->power = 25 + plr->lev;
        copy->blows = 50 + plr->lev;
        if (plr_mon_race_is_("T.ettin"))
            copy->blows += 50;
        vec_add(plr->innate_blows, copy);
    }
}

/******************************************************************************
 * Troll Powers
 ******************************************************************************/
static void _aklash_breathe_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Gas");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes poison at your opponent.");
        break;
    default:
        breath_spell_innate(cmd, res, 2, GF_POIS, plr->chp/4);
    }
}

static power_info _powers[] = 
{
    { A_STR, { 10, 10, 30, berserk_spell} },
    { A_CON, { 28, 20, 50, resistance_spell} },
    {    -1, { -1, -1, -1, NULL}}
};

static power_info _aklash_powers[] = 
{
    { A_CON, {  25, 10, 30, _aklash_breathe_spell} },
    {    -1, { -1, -1, -1, NULL}}
};

static power_info _storm_troll_powers[] = 
{
    { A_STR, {  40, 10, 30, lightning_ball_spell} },
    { A_STR, {  40, 10, 35, frost_ball_spell} },
    { A_STR, {  40, 15, 50, ice_bolt_spell} },
    { A_STR, {  40, 20, 60, water_bolt_spell} },
    {    -1, { -1, -1, -1, NULL}}
};

static power_info _troll_king_powers[] = 
{
    { A_DEX, { 40,  2, 30, phase_door_spell} },
    { A_CHR, { 40, 30, 50, summon_kin_spell} },
    {    -1, { -1, -1, -1, NULL}}
};

static int _get_powers(spell_info* spells, int max) 
{
    int ct = get_powers_aux(spells, max, _powers);
    if (plr_mon_race_is_("T.aklash"))
        ct += get_powers_aux(spells + ct, max - ct, _aklash_powers);
    else if (plr_mon_race_is_("T.storm"))
        ct += get_powers_aux(spells + ct, max - ct, _storm_troll_powers);
    else if (plr_mon_race_is_("T.king"))
        ct += get_powers_aux(spells + ct, max - ct, _troll_king_powers);
    return ct;
}

/******************************************************************************
 * Troll Evolution
 ******************************************************************************/
static void _birth(void) 
{ 
    object_type forge;

    plr_mon_race_set("T.forest");
    skills_innate_init("Bite", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);

    object_prep(&forge, lookup_kind(TV_HARD_ARMOR, SV_CHAIN_MAIL));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_d = 7;
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_HAFTED, SV_CLUB));
    forge.weight = 100;
    forge.dd = 3;
    forge.ds = 4;
    forge.to_d = 7;
    plr_birth_obj(&forge);

    plr_birth_food();
    plr_birth_light();
}

static void _gain_level(int new_level) 
{
    if (plr_mon_race_is_("T.forest") && new_level >= 10)
        plr_mon_race_evolve("T.stone");
    if (plr_mon_race_is_("T.stone") && new_level >= 20)
    {
        int r = randint0(3);
        if (spoiler_hack)
            r = 2;
        switch (r)
        {
        case 0:
            plr_mon_race_evolve("T.ice");
            break;
        case 1:
            plr_mon_race_evolve("T.fire");
            break;
        case 2:
            plr_mon_race_evolve("T.algroth");
            break;
        }
    }
    if (plr_mon_race_is_("T.algroth") && new_level >= 25)
        plr_mon_race_evolve("T.aklash");
    if ( ( plr_mon_race_is_("T.fire")
        || plr_mon_race_is_("T.ice")
        || plr_mon_race_is_("T.aklash") )
      && new_level >= 30 )
    {
        plr_mon_race_evolve("T.olog");
    }
    if (plr_mon_race_is_("T.olog") && new_level >= 40)
    {
        switch (plr->psubrace)
        {
        case TROLL_ETTIN:
            plr_mon_race_evolve("T.ettin");
            break;
        case TROLL_STORM:
            plr_mon_race_evolve("T.storm");
            break;
        case TROLL_SPIRIT:
            plr_mon_race_evolve("T.spirit");
            break;
        case TROLL_KING:
            plr_mon_race_evolve("T.king");
            break;
        }
    }
}

/******************************************************************************
 * Troll Bonuses
 ******************************************************************************/
static void _calc_bonuses(void) 
{
    int to_a = plr_prorata_level_aux(25, 1, 2, 2);

    plr->to_a += to_a;
    plr->dis_to_a += to_a;

    plr->regen += 100 + 8*plr->lev;
    if (plr_mon_race_is_("T.forest"))
    {
        res_add_vuln(GF_LIGHT);
    }
    else if (plr_mon_race_is_("T.stone"))
    {
        res_add_vuln(GF_LIGHT);
    }
    else if (plr_mon_race_is_("T.ice"))
    {
        res_add_vuln(GF_LIGHT);
        res_add_vuln(GF_FIRE);
        res_add(GF_COLD);
        res_add(GF_COLD);
    }
    else if (plr_mon_race_is_("T.fire"))
    {
        res_add_vuln(GF_LIGHT);
        res_add_vuln(GF_COLD);
        res_add(GF_FIRE);
        res_add(GF_FIRE);
    }
    else if (plr_mon_race_is_("T.algroth"))
    {
    }
    else if (plr_mon_race_is_("T.aklash"))
    {
        res_add(GF_POIS);
        res_add(GF_ACID);
    }
    else if (plr_mon_race_is_("T.olog"))
    {
        res_add(GF_POIS);
    }
    else if (plr_mon_race_is_("T.ettin"))
    {
        plr->free_act++;
        res_add(GF_POIS);
        res_add(GF_CONF);
    }
    else if (plr_mon_race_is_("T.spirit"))
    {
        plr->pass_wall = TRUE;
        plr->no_passwall_dam = TRUE;
        plr->free_act++;
        plr->see_inv++;
        plr->levitation = TRUE;
        res_add(GF_COLD);
        res_add(GF_ELEC);
        res_add(GF_POIS);
        res_add(GF_CONF);
    }
    else if (plr_mon_race_is_("T.storm"))
    {
        plr->pspeed += 5;
        res_add(GF_COLD);
        res_add(GF_ELEC);
        res_add(GF_ACID);
    }
    else if (plr_mon_race_is_("T.king"))
    {
        plr->pspeed += 7;
        plr->free_act++;
        res_add(GF_POIS);
        res_add(GF_CONF);
    }
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE]) 
{
    add_flag(flgs, OF_REGEN);
    if (plr_mon_race_is_("T.forest"))
    {
        add_flag(flgs, OF_VULN_(GF_LIGHT));
    }
    else if (plr_mon_race_is_("T.stone"))
    {
        add_flag(flgs, OF_VULN_(GF_LIGHT));
    }
    else if (plr_mon_race_is_("T.ice"))
    {
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_BRAND_COLD);
        add_flag(flgs, OF_VULN_(GF_LIGHT));
        add_flag(flgs, OF_VULN_(GF_FIRE));
    }
    else if (plr_mon_race_is_("T.fire"))
    {
        add_flag(flgs, OF_RES_(GF_FIRE));
        add_flag(flgs, OF_BRAND_FIRE);
        add_flag(flgs, OF_VULN_(GF_LIGHT));
        add_flag(flgs, OF_VULN_(GF_COLD));
    }
    else if (plr_mon_race_is_("T.algroth"))
    {
        add_flag(flgs, OF_BRAND_POIS);
    }
    else if (plr_mon_race_is_("T.aklash"))
    {
        add_flag(flgs, OF_RES_(GF_POIS));
        add_flag(flgs, OF_RES_(GF_ACID));
    }
    else if (plr_mon_race_is_("T.olog"))
    {
        add_flag(flgs, OF_RES_(GF_POIS));
    }
    else if (plr_mon_race_is_("T.ettin"))
    {
        add_flag(flgs, OF_FREE_ACT);
        add_flag(flgs, OF_RES_(GF_POIS));
        add_flag(flgs, OF_RES_(GF_CONF));
    }
    else if (plr_mon_race_is_("T.spirit"))
    {
        add_flag(flgs, OF_FREE_ACT);
        add_flag(flgs, OF_SEE_INVIS);
        add_flag(flgs, OF_LEVITATION);
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_RES_(GF_ELEC));
        add_flag(flgs, OF_RES_(GF_POIS));
        add_flag(flgs, OF_RES_(GF_CONF));
    }
    else if (plr_mon_race_is_("T.storm"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_RES_(GF_ELEC));
        add_flag(flgs, OF_RES_(GF_ACID));
        add_flag(flgs, OF_BRAND_ELEC);
    }
    else if (plr_mon_race_is_("T.king"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_FREE_ACT);
        add_flag(flgs, OF_RES_(GF_POIS));
        add_flag(flgs, OF_RES_(GF_CONF));
    }
}

static void _calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    if (plr_mon_race_is_("T.ice"))
    {
        add_flag(info->obj_flags, OF_BRAND_COLD);
        add_flag(info->obj_known_flags, OF_BRAND_COLD);
    }
    else if (plr_mon_race_is_("T.fire"))
    {
        add_flag(info->obj_flags, OF_BRAND_FIRE);
        add_flag(info->obj_known_flags, OF_BRAND_FIRE);
    }
    else if (plr_mon_race_is_("T.algroth"))
    {
        add_flag(info->obj_flags, OF_BRAND_POIS);
        add_flag(info->obj_known_flags, OF_BRAND_POIS);
    }
    else if (plr_mon_race_is_("T.storm"))
    {
        add_flag(info->obj_flags, OF_BRAND_ELEC);
        add_flag(info->obj_known_flags, OF_BRAND_ELEC);
    }
    else if (plr_mon_race_is_("T.king"))
    {
        info->to_d += 10;
        info->dis_to_d += 10;
    }
}

static name_desc_t _info[TROLL_MAX] = {
    { "Ettin", "Ettins are large, two-headed trolls. They lack much in the way of "
                "powers and abilities, but make up for this with the ability to "
                "wield an extra helmet." },
    { "Storm Troll", "Storm Trolls are fast trolls with elemental powers. They may call "
                        "forth elemental balls and bolts. Their weapons are wreathed in "
                        "electricity as their fury rains down on all they meet." },
    { "Spirit Troll", "Spirit trolls may pass through walls on their quest to demolish "
                        "all that oppose them." },
    { "Troll King", "Troll Kings are lords of their kind, fast and extremely deadly in "
                        "melee. They may blink themselves out of harms way." },
};

/******************************************************************************
 * Troll Public API
 ******************************************************************************/
plr_race_ptr mon_troll_get_race(int psubrace)
{
    static plr_race_ptr me = NULL;

    if (birth_hack && psubrace >= TROLL_MAX)
        psubrace = 0;

    assert(0 <= psubrace && psubrace < TROLL_MAX);

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  30,   1,  13,   7,  65,  30 };
    skills_t xs = { 35,  35,  50,   0,   0,   0, 140,  50 };

        me = plr_race_alloc(RACE_MON_TROLL);
        me->skills = bs;
        me->extra_skills = xs;

        me->name = "Troll";
        me->desc =     
            "Trolls are disgusting creatures: Big, strong and stupid. They make excellent warriors "
            "but are hopeless with magical devices. Trolls have incredible powers of regeneration.";

        me->infra = 5;
        me->exp = 150;
        me->base_hp = 50;
        me->shop_adjust = 135;

        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.calc_innate_attacks = _calc_innate_attacks;
        me->hooks.get_powers = _get_powers;
        me->hooks.get_flags = _get_flags;
        me->hooks.gain_level = _gain_level;
        me->hooks.birth = _birth;

        me->flags = RACE_IS_MONSTER;
        me->boss_r_idx = mon_race_parse("T.Ulik")->id;
        me->pseudo_class_id = CLASS_WARRIOR;
    }

    me->subid = psubrace;
    me->subname = mon_name(plr->current_r_idx);
    me->stats[A_STR] =  3 + plr->lev/12;
    me->stats[A_INT] = -5;
    me->stats[A_WIS] = -5;
    me->stats[A_DEX] = -2 + plr->lev/20;
    me->stats[A_CON] =  3 + plr->lev/13;
    me->stats[A_CHR] =  0;
    me->life = 100 + (plr->lev/10)*4;

    if (birth_hack || spoiler_hack)
    {
        me->subname = _info[psubrace].name;
        me->subdesc = _info[psubrace].desc;
    }

    me->equip_template = plr_equip_template();
    return me;
}
