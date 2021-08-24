#include "angband.h"

/**********************************************************************
 * Jelly: black ooze -> gelatinous cube -> acidic cytoplasm -> shoggoth
 **********************************************************************/

static cptr _desc = 
  "Jellies are acidic creatures, quietly patrolling their surroundings in search "
  "of objects to absorb. They are quite stealthy for most of their lives. They "
  "don't wear equipment on their bodies the way normal races do. Instead, they "
  "are capable of absorbing objects into their gelatinous frames. In fact, each "
  "equipment slot (except their pseudopod) is capable of holding any type of "
  "equipment whatsoever, and the number of slots increases with experience. "
  "For attacking, jellies are capable of forming a single pseudopod which they "
  "may either use for grasping a normal melee weapon, or leave empty to use their "
  "innate acid based attacks.\n \n"
  "Jellies are monsters and cannot pick a normal player class. Rather than relying on sight, "
  "jellies use a little understood sense of their surroundings which allows them to "
  "see in the dark with neither light nor infravision. So while jellies can equip a "
  "torch or lantern, there is little reason for them to do so.\n \n"
  "Jellies feed off the objects they find gaining nutrition in proportion to the "
  "weight of the object absorbed. They can eat rations of food if desired, but these "
  "are so light as to offer very little nutritional value. Jellies prefer to eat heavy "
  "armors and weapons. For convenience, destroying an object ('k' or the autodestroyer) "
  "automatically eats the object, and jellies never become bloated, their appetite being "
  "insatiable.\n \n"
  "Like all monster races, jellies begin as weak monster of their race and evolve to "
  "better forms as they gain experience. Jellies begin life as a small puddle of black "
  "ooze with very little carrying capacity for equipment. However, they quickly evolve "
  "into a Gelatinous Cube and then into an Acidic Cytoplasm. Jellies take one additional "
  "powerful evolutionary step late in life.";

static void _divide_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Divide");
        break;
    case SPELL_DESC:
        var_set_string(res, "Replicate yourself.");
        break;
    case SPELL_CAST:
    {
        summon_named_creature(who_create_plr(), plr->pos, plr_mon_race(), PM_FORCE_PET);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
static power_info _jelly_powers[] = {
    { A_CON, { 1, 10, 0, _divide_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _jelly_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _jelly_powers);
}
static void _jelly_calc_innate_bonuses(mon_blow_ptr blow)
{
    if (blow->method != RBM_TOUCH) return;
    plr->innate_attack_info.blows_calc.wgt = 200;
    plr->innate_attack_info.blows_calc.mul = 50;
    plr->innate_attack_info.blows_calc.max = 600;
    plr_calc_blows_innate(blow);
}
static void _jelly_calc_innate_attacks(void)
{
    int l = plr->lev;
    mon_blow_ptr blow = mon_blow_alloc(RBM_TOUCH);

    blow->name = "Pseudopod";
    blow->msg = "You shoot acid.";
    blow->power = l*3/2;
    mon_blow_push_effect(blow, GF_ACID, dice_create(2 + l/10, 6 + l/12, l/5 + l*l/250));
    _jelly_calc_innate_bonuses(blow);
    vec_add(plr->innate_blows, blow);
}

static void _black_ooze_calc_bonuses(void)
{
    res_add(GF_ACID);
    res_add(GF_POIS);
    res_add_immune(GF_BLIND);
    plr->see_nocto = DUN_VIEW_MAX;
    res_add_immune(GF_STUN);
    plr->no_cut = TRUE;
}
static void _black_ooze_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_ACID));
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_IM_(GF_BLIND));
}
plr_race_ptr _black_ooze_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  37,   8,  14,   7,  70,  30};
    skills_t xs = { 60,  35,  55,   0,   0,   0, 150,  35};

        me = plr_race_alloc(RACE_MON_JELLY);
        me->skills = bs;
        me->extra_skills = xs;

        me->subname = "Black Ooze";

        me->stats[A_STR] =  1;
        me->stats[A_INT] = -5;
        me->stats[A_WIS] = -5;
        me->stats[A_DEX] =  1;
        me->stats[A_CON] =  1;
        me->stats[A_CHR] = -2;

        me->life = 90;
        me->infra = 0;

        me->hooks.calc_bonuses = _black_ooze_calc_bonuses;
        me->hooks.get_flags = _black_ooze_get_flags;
    }
    return me;
}

static void _gelatinous_cube_calc_bonuses(void)
{
    plr->to_a += plr->lev/3;
    plr->dis_to_a += plr->lev/3;
    res_add(GF_ACID);
    res_add(GF_FIRE);
    res_add(GF_COLD);
    res_add(GF_ELEC);
    _black_ooze_calc_bonuses();
}
static void _gelatinous_cube_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_ACID));
    add_flag(flgs, OF_RES_(GF_FIRE));
    add_flag(flgs, OF_RES_(GF_COLD));
    add_flag(flgs, OF_RES_(GF_ELEC));
    _black_ooze_get_flags(flgs);
}
plr_race_ptr _gelatinous_cube_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  37,   8,  14,   7,  70,  30};
    skills_t xs = { 60,  35,  55,   0,   0,   0, 150,  35};

        me = plr_race_alloc(RACE_MON_JELLY);
        me->skills = bs;
        me->extra_skills = xs;

        me->subname = "Gelatinous Cube";

        me->stats[A_STR] =  2;
        me->stats[A_INT] = -10;
        me->stats[A_WIS] = -10;
        me->stats[A_DEX] =  2;
        me->stats[A_CON] =  2;
        me->stats[A_CHR] = -2;
        
        me->life = 100;
        me->infra = 0;

        me->hooks.calc_bonuses = _gelatinous_cube_calc_bonuses;
        me->hooks.get_flags = _gelatinous_cube_get_flags;
    }
    return me;
}

static void _acidic_cytoplasm_calc_bonuses(void)
{
    plr->pspeed += 3;
    plr->free_act++;
    plr->to_a += plr->lev/3;
    plr->dis_to_a += plr->lev/3;
    res_add_immune(GF_ACID);
    res_add(GF_CONF);
    res_add_immune(GF_FEAR);

    _gelatinous_cube_calc_bonuses();
}
static void _calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    add_flag(info->obj_flags, OF_BRAND_ACID);
    add_flag(info->obj_known_flags, OF_BRAND_ACID);
}
static void _acidic_cytoplasm_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SPEED);
    add_flag(flgs, OF_FREE_ACT);
    add_flag(flgs, OF_RES_(GF_CONF));
    add_flag(flgs, OF_BRAND_ACID);

    add_flag(flgs, OF_IM_(GF_ACID));
    add_flag(flgs, OF_IM_(GF_FEAR));

    _gelatinous_cube_get_flags(flgs);
}

plr_race_ptr _acidic_cytoplasm_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  37,   7,  14,   7,  70,  30};
    skills_t xs = { 60,  35,  55,   0,   0,   0, 150,  35};

        me = plr_race_alloc(RACE_MON_JELLY);
        me->skills = bs;
        me->extra_skills = xs;

        me->subname = "Acidic Cytoplasm";

        me->stats[A_STR] =  3;
        me->stats[A_INT] = -7;
        me->stats[A_WIS] = -7;
        me->stats[A_DEX] =  2;
        me->stats[A_CON] =  3;
        me->stats[A_CHR] = -1;
        
        me->life = 105;
        me->infra = 0;

        me->hooks.calc_bonuses = _acidic_cytoplasm_calc_bonuses;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.get_flags = _acidic_cytoplasm_get_flags;
    }
    return me;
}

static void _shoggoth_calc_bonuses(void)
{
    plr->pspeed += 2;
    plr->regen += 100;
    plr->to_a += plr->lev/3;
    plr->dis_to_a += plr->lev/3;
    plr->no_eldritch = TRUE;

    res_add(GF_TELEPORT);

    _acidic_cytoplasm_calc_bonuses();
}
static void _shoggoth_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SPEED);
    add_flag(flgs, OF_REGEN);
    _acidic_cytoplasm_get_flags(flgs);
}
plr_race_ptr _shoggoth_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  37,   2,  14,   7,  70,  30};
    skills_t xs = { 60,  35,  55,   0,   0,   0, 150,  35};

        me = plr_race_alloc(RACE_MON_JELLY);
        me->skills = bs;
        me->extra_skills = xs;

        me->subname = "Shoggoth";

        me->stats[A_STR] =  5;
        me->stats[A_INT] = -5;
        me->stats[A_WIS] = -5;
        me->stats[A_DEX] =  2;
        me->stats[A_CON] =  4;
        me->stats[A_CHR] =  0;
        
        me->life = 108;
        me->infra = 0;

        me->hooks.calc_bonuses = _shoggoth_calc_bonuses;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.get_flags = _shoggoth_get_flags;
    }
    return me;
}

static void _gain_level(int new_level) 
{
    if (plr_mon_race_is_("j.ooze.black") && new_level >= 10)
        plr_mon_race_evolve("j.cube");
    else if ( plr_mon_race_is_("j.cube") && new_level >= 25)
        plr_mon_race_evolve("j.cytoplasm");
    else if ( plr_mon_race_is_("j.cytoplasm") && new_level >= 40)
        plr_mon_race_evolve("j.shoggoth");
}


static void _birth(void)
{
    object_type forge;

    plr_mon_race_set("j.ooze.black");
    skills_innate_init("Pseudopod", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.pval = 1;
    forge.to_d = 3;
    add_flag(forge.flags, OF_STR);
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL));
    plr_birth_obj(&forge);
}

plr_race_ptr mon_jelly_get_race(void)
{
    plr_race_ptr result = NULL;

    if (plr_mon_race_is_("j.ooze.black"))
        result = _black_ooze_get_race_t();
    else if (plr_mon_race_is_("j.cube"))
        result = _gelatinous_cube_get_race_t();
    else if (plr_mon_race_is_("j.cytoplasm"))
        result = _acidic_cytoplasm_get_race_t();
    else if (plr_mon_race_is_("j.shoggoth"))
        result = _shoggoth_get_race_t();
    else /* Birth code? Startup? Restart from Old Savefile? */
        result = _black_ooze_get_race_t();

    result->name = "Jelly";
    result->desc = _desc;
    result->exp = 150;
    result->base_hp = 40;
    result->shop_adjust = 125;
    result->hooks.gain_level = _gain_level;
    result->hooks.get_powers = _jelly_get_powers;
    result->hooks.calc_innate_attacks = _jelly_calc_innate_attacks;
    result->hooks.calc_innate_bonuses = _jelly_calc_innate_bonuses;
    result->hooks.birth = _birth;
    result->hooks.destroy_object = jelly_eat_object;
    result->equip_template = plr_equip_template();
    result->pseudo_class_id = CLASS_WARRIOR;
    result->boss_r_idx = mon_race_parse("j.Ubbo-Sathla")->id;
    result->flags = RACE_IS_MONSTER /* | RACE_IS_ILLITERATE */;

    if (birth_hack || spoiler_hack)
    {
        result->subname = NULL;
        result->subdesc = NULL;
    }

    return result;
}

bool jelly_eat_object(object_type *o_ptr)
{
    char o_name[MAX_NLEN];
    object_type copy = *o_ptr;
    copy.number = 1;
    object_desc(o_name, &copy, OD_COLOR_CODED);
    set_food(MIN(PY_FOOD_FULL - 1, plr->food + o_ptr->weight * 50));
    msg_format("You assimilate %s into your gelatinous frame.", o_name);
    /* TODO: Consider giving timed benefits based on what is absorbed.
       For example, TR_RES_FIRE might give temp fire resistance and 
       TR_SPEED might give temporary haste.
    */
    return TRUE;
}

