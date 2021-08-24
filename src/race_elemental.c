#include "angband.h"

#include <assert.h>

static cptr _desc = 
    "Elementals are mindless creatures animated from a single "
    "elemental form. As a species, elementals can never be confused or cut, "
    "and they are immune to the eldritch horror (being virtually mindless). "
    "They rarely feel fear and are resistant to poison. In addition, they "
    "gain specific powers and abilities depending on their form.\n \n"
    "For example, Earth Elementals are slow but gain bonuses to AC due "
    "to their tough skins. They are resistant to shards and may even turn "
    "their skins to stone. At home in elemental earth, they may travel "
    "freely through rocky confines. However, being made of earth, their "
    "potions frequently turn to mud!\n \n"
    "Air Elementals are shockingly fast, but perhaps that is just the "
    "crackle of their electrified bodies? They may hurl bolts and balls "
    "electricity at their enemies and may even imbue their weapons with "
    "deadly lightning. However, being surrounded by lightning, rings, "
    "amulets, wands and rods are quickly destroyed!\n \n"
    "Fire Elementals are somewhat fast (They definitely run circles around "
    "their earthen brethren) and are cloaked in fire. Of course, they may attack "
    "with hell's fury but need to be on the lookout for cold wielding foes. "
    "However, being surrounded by fire, scrolls and staves are rapidly burned "
    "to ash!\n \n"
    "Finally, there are the Water Elementals, creatures able to conjure deadly "
    "water bolts. They are immune to stunning. Their attacks can be quite corrosive, "
    "but, alas, sometimes their armor corrodes as well!";

static void _calc_bonuses(void) 
{
    res_add(GF_CONF);
    res_add(GF_FEAR);
    plr->no_cut = TRUE;
    plr->no_eldritch = TRUE;
    plr->levitation = TRUE;
    plr->slow_digest = TRUE;

    if (plr->lev >= 5)
        res_add(GF_POIS);
    if (plr->lev >= 10)
        plr->see_inv++;
    if (plr->lev >= 15)
        plr->free_act++;
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE]) 
{
    add_flag(flgs, OF_RES_(GF_CONF));
    add_flag(flgs, OF_RES_(GF_FEAR));
    add_flag(flgs, OF_LEVITATION);
    add_flag(flgs, OF_SLOW_DIGEST);
    if (plr->lev >= 5)
        add_flag(flgs, OF_RES_(GF_POIS));
    if (plr->lev >= 10)
        add_flag(flgs, OF_SEE_INVIS);
    if (plr->lev >= 15)
        add_flag(flgs, OF_FREE_ACT);
}

static bool _elemental_travel(int type)
{
    int  rng = plr->lev / 2 + 10;
    point_t pos = target_pos(rng);
    dun_cell_ptr cell;

    if (!dun_pos_interior(cave, pos)) return FALSE;

    cell = dun_cell_at(cave, pos);
    if (cell->type != type)
    {
        msg_print("Failed! You are out of your element!");
        teleport_player((plr->lev + 2) * 2, TELEPORT_PASSIVE);
    }
    else if (one_in_(7))
    {
        msg_print("You failed to travel correctly!");
        teleport_player((plr->lev + 2) * 2, TELEPORT_PASSIVE);
    }
    else
    {
        /* Note: teleport_player_to requires FF_TELEPORTABLE, which won't work for walls */
        if (type == FEAT_WALL && !(cell->flags & CELL_PERM))
            move_player_effect(pos, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
        else
            teleport_player_to(pos, 0);
    }
    return TRUE;
}

static bool _elemental_healing(int type)
{
    int dir, ct = 0;
    dun_cell_ptr cell = dun_cell_at(cave, plr->pos);

    if (cell->type != type)
    {
        msg_print("Failed! You are out of your element!");
        return FALSE;
    }

    for (dir = 0; dir < 8; dir++)
    {
        point_t p = point_step(plr->pos, dir);
        dun_cell_ptr c = dun_cell_at(cave, p);
        if (c->type == type) ct++; /* note: we count the boundary walls for the earth elemental */
    }

    if (ct < 4)
    {
        msg_print("Failed! You need to be surrounded by your element!");
        return FALSE;
    }

    msg_print("You bask in your element and slowly feel your life returning ... ");
    hp_player(100 + plr->lev * 3);
    return TRUE;
}

static void _elemental_rage_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Elemental Rage");
        break;
    default:
        berserk_spell(cmd, res);
    }
}

static void _destroy_aux(obj_ptr obj, cptr fmt)
{
    char o_name[MAX_NLEN];

    object_desc(o_name, obj, OD_OMIT_PREFIX | OD_NAME_ONLY | OD_COLOR_CODED | OD_SINGULAR);
    msg_format(fmt, o_name);

    stats_on_p_destroy(obj, 1);
    obj->number--;
    obj_release(obj, 0);
}

static void _elemental_pack_destroy(object_p p, cptr destroy_fmt, int chance)
{
    int inven_ct = 0;
    int equip_ct = 0;
    slot_t slot;
    for (slot = 1; slot <= pack_max(); slot++)
    {
        obj_ptr obj = pack_obj(slot);

        if (!obj) continue;
        if (!p(obj)) continue;
        
        if (randint0(1000) >= MAX(1, chance)) continue;
        _destroy_aux(obj, destroy_fmt);
        ++inven_ct;
   }
    for (slot = 1; slot <= equip_max(); slot++)
    {
        obj_ptr obj = equip_obj(slot);

        if (!obj) continue;
        if (!p(obj)) continue;
        
        if (randint0(1000) >= MAX(1, chance/3)) continue;
        _destroy_aux(obj, destroy_fmt);
        ++equip_ct;
    }

    if (equip_ct)
    {
        plr->update |= PU_BONUS | PU_TORCH | PU_MANA;
        plr->redraw |= PR_EQUIPPY;
        plr->window |= PW_EQUIP;
    }
    if (inven_ct)
        plr->window |= PW_INVEN;

    if (equip_ct + inven_ct)
        disturb(1, 0);
}

/**********************************************************************
 *             25
 * Earth Spirt -> Earth Elemental
 **********************************************************************/
static void _earth_birth(void) 
{ 
    object_type forge;

    plr_mon_race_set("E.earth spirit");
    
    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_d = 6;
    forge.pval = 3;
    add_flag(forge.flags, OF_STR);
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_HAFTED, SV_CLUB));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_HARD_ARMOR, SV_CHAIN_MAIL));
    plr_birth_obj(&forge);

    plr_birth_light();
}

static void _earth_gain_level(int new_level) 
{
    if (plr_mon_race_is_("E.earth spirit") && new_level >= 25)
        plr_mon_race_evolve("E.earth");
}

static void _shard_bolt_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shard Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a bolt of shards at chosen target.");
        break;
    default:
        bolt_spell(cmd, res, GF_SHARDS, 1 + plr->lev/3, 8);
    }
}

static void _earthen_healing_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Earthen Healing");
        break;
    case SPELL_DESC:
        var_set_string(res, "If you are surrounded by rock, you may heal yourself at the cost of several acts.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_heal(0, 0, 100 + plr->lev * 3));
        break;
    case SPELL_CAST:
        var_set_bool(res, _elemental_healing(FEAT_WALL));
        break;
    case SPELL_ENERGY:
        var_set_int(res, 300);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _earthen_portal_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Earthen Portal");
        break;
    case SPELL_DESC:
        var_set_string(res, "Move instantaneously to chosen rocky locale.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _elemental_travel(FEAT_WALL));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _shard_ball_spell(int cmd, var_ptr res)
{
    int dam = plr_prorata_level(300);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shard Ball");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a ball of shards at chosen target");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, dam / 6);
        break;
    default:
        ball_spell(cmd, res, 2, GF_SHARDS, dam);
    }
}

static void _wall_of_earth_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Wall of Earth");
        break;
    case SPELL_DESC:
        var_set_string(res, "Create walls on all open, surrounding squares.");
        break;
    case SPELL_CAST:
        wall_stone();
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static power_info _earth_powers[] = 
{
    { A_STR, {  1,  1, 35, eat_rock_spell}},
    { A_STR, {  7,  5, 35, _shard_bolt_spell}},
    { A_CON, { 10, 10, 40, stone_skin_spell}},
    { A_CON, { 15, 10, 40, sense_surroundings_spell}},
    { A_STR, { 20, 15, 40, earthquake_spell}},
    { A_STR, { 25,  0, 50, _shard_ball_spell}},
    { A_CON, { 30, 50, 60, _wall_of_earth_spell}},
    { A_CON, { 35,  0, 65, _earthen_healing_spell}},
    { A_DEX, { 42, 75, 70, _earthen_portal_spell}},
    {    -1, { -1, -1, -1, NULL} }
};

static int _earth_get_powers(spell_info* spells, int max) 
{
    return get_powers_aux(spells, max, _earth_powers);
}

static void _earth_calc_bonuses(void) 
{
    int to_a = plr_prorata_level(50);

    plr->to_a += to_a;
    plr->dis_to_a += to_a;

    res_add(GF_FIRE);
    res_add(GF_COLD);
    res_add(GF_ELEC);
    res_add(GF_SHARDS);
    plr->pass_wall = TRUE;
    plr->no_passwall_dam = TRUE;
    plr->regen += 100;

    plr->pspeed--;
    if (plr->lev >= 25)
        plr->pspeed--;

    _calc_bonuses();
}

static void _earth_get_flags(u32b flgs[OF_ARRAY_SIZE]) 
{
    add_flag(flgs, OF_RES_(GF_FIRE));
    add_flag(flgs, OF_RES_(GF_COLD));
    add_flag(flgs, OF_RES_(GF_ELEC));
    add_flag(flgs, OF_RES_(GF_SHARDS));
    add_flag(flgs, OF_DEC_SPEED);
    add_flag(flgs, OF_REGEN);

    _get_flags(flgs);
}

static bool _earth_p(object_type *o_ptr)
{
    if (obj_is_art(o_ptr)) return FALSE;
    if (o_ptr->tval != TV_POTION) return FALSE;
    return TRUE;
}

static void _earth_process_world(void)
{
    int chance = 40 - plr->lev/2;
    _elemental_pack_destroy(_earth_p, "Your %s turns to mud.", chance);
}

static plr_race_ptr _earth_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[2] =  {"Earth Spirit", "Earth Elemental"};
    int           rank = 0;

    if (plr->lev >= 25) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  18,  40,   5,  25,  16,  70,  25};
    skills_t xs = { 40,  40,  60,   0,   0,   0, 150,  35};

        me = plr_race_alloc_aux(RACE_MON_ELEMENTAL, ELEMENTAL_EARTH);
        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 5;
        me->exp = 170;

        me->hooks.birth = _earth_birth;
        me->hooks.get_powers = _earth_get_powers;
        me->hooks.calc_bonuses = _earth_calc_bonuses;
        me->hooks.get_flags = _earth_get_flags;
        me->hooks.gain_level = _earth_gain_level;
        me->hooks.process_world = _earth_process_world;

        me->boss_r_idx = mon_race_parse("E.Quaker")->id;
    }

    me->subname = titles[rank];
    me->stats[A_STR] =  2 + 3*rank;
    me->stats[A_INT] = -5;
    me->stats[A_WIS] = -5;
    me->stats[A_DEX] = -2 - 2*rank;
    me->stats[A_CON] =  2 + 4*rank;
    me->stats[A_CHR] =  0;
    me->life = 105 + 15*rank;

    return me;
}

/**********************************************************************
 *           25
 * Air Spirt -> Air Elemental
 **********************************************************************/
static void _air_birth(void) 
{ 
    object_type forge;
    
    plr_mon_race_set("E.air spirit");

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_JEWELRY_ELEMENTAL;
    forge.to_a = 15;
    add_flag(forge.flags, OF_RES_(GF_ELEC));
    add_flag(forge.flags, OF_AURA_ELEC);
    add_flag(forge.flags, OF_IGNORE_ELEC);
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_SWORD, SV_LONG_SWORD));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_HARD_ARMOR, SV_CHAIN_MAIL));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_STAFF, SV_ANY));
    if (device_init_fixed(&forge, EFFECT_NOTHING))
        plr_birth_obj(&forge);
    plr_birth_light();

}

static void _air_gain_level(int new_level) 
{
    if (plr_mon_race_is_("E.air spirit") && new_level >= 25)
        plr_mon_race_evolve("E.air");
}

static void _confusing_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Confusing Strike");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent opponent with a confusing blow.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_attack_special(PLR_HIT_CONFUSE, PAC_NO_INNATE));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _lightning_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Lightning Strike");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent opponent with a shocking blow.");
        break;
    default:
        lightning_eagle_spell(cmd, res);
        break;
    }
}

static void _lightning_storm_spell(int cmd, var_ptr res)
{
    int dam = plr_prorata_level(350);

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Lightning Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a huge ball of electricity.");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, dam / 10);
        break;
    default:
        ball_spell(cmd, res, 4, GF_ELEC, dam);
    }
}

static void _whirlwind_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Whirlwind");
        break;
    default:
        massacre_spell(cmd, res);
    }
}

static void _sky_gate_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Sky Gate");
        break;
    case SPELL_DESC:
        var_set_string(res, "Move instantaneously to chosen open location.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _elemental_travel(FEAT_FLOOR));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static power_info _air_powers[] = 
{
    { A_STR, {  2,  3, 25, lightning_bolt_spell}},
    { A_DEX, {  5,  3, 25, phase_door_spell}},
    { A_DEX, { 10,  5, 35, teleport_spell}},
    { A_DEX, { 15,  7,  0, _confusing_strike_spell}},
    { A_STR, { 17, 10, 45, lightning_ball_spell}},
    { A_DEX, { 25, 20, 50, haste_self_spell}},
    { A_STR, { 32, 25,  0, _lightning_strike_spell}},
    { A_DEX, { 35, 40, 55, _whirlwind_spell}},
    { A_STR, { 37,  0, 55, _lightning_storm_spell}},
    { A_DEX, { 42, 40, 60, _sky_gate_spell}},
    {    -1, { -1, -1, -1, NULL} }
};

static int _air_get_powers(spell_info* spells, int max) 
{
    return get_powers_aux(spells, max, _air_powers);
}

static void _air_calc_bonuses(void) 
{
    res_add(GF_ELEC);

    plr->pspeed += 2;
    if (plr->lev >= 25)
    {
        plr->pspeed += 3;
        plr->pspeed += (plr->lev - 25) / 5; /* up to +10 speed */
        res_add(GF_ELEC);
        res_add(GF_ACID);
        res_add(GF_FIRE);
        res_add(GF_COLD);
        plr->sh_elec = TRUE;
    }
    if (plr->lev >= 50)
        res_add_immune(GF_ELEC);

    _calc_bonuses();
}

static void _air_get_flags(u32b flgs[OF_ARRAY_SIZE]) 
{
    add_flag(flgs, OF_RES_(GF_ELEC));
    add_flag(flgs, OF_SPEED);

    if (plr->lev >= 25)
    {
        add_flag(flgs, OF_RES_(GF_ACID));
        add_flag(flgs, OF_RES_(GF_FIRE));
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_AURA_ELEC);
    }

    if (plr->lev >= 50)
        add_flag(flgs, OF_IM_(GF_ELEC));

    _get_flags(flgs);
}

static bool _air_p(object_type *o_ptr)
{
    if (obj_is_art(o_ptr)) return FALSE;
    if ( o_ptr->tval != TV_RING 
      && o_ptr->tval != TV_AMULET 
      && o_ptr->tval != TV_WAND 
      && o_ptr->tval != TV_ROD) 
    {
        return FALSE;
    }
    if (obj_has_flag(o_ptr, OF_IGNORE_ELEC)) return FALSE;
    return TRUE;
}

static void _air_process_world(void)
{
    _elemental_pack_destroy(_air_p, "Your shocking touch destroys your %s.", 40);
}

static plr_race_ptr _air_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[2] =  {"Air Spirit", "Air Elemental"};
    int           rank = 0;

    if (plr->lev >= 25) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  25,  35,   6,  25,  16,  55,  35};
    skills_t xs = { 40,  50,  45,   0,   0,   0, 100,  75};

        me = plr_race_alloc_aux(RACE_MON_ELEMENTAL, ELEMENTAL_AIR);
        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 5;
        me->exp = 225;

        me->hooks.birth = _air_birth;
        me->hooks.get_powers = _air_get_powers;
        me->hooks.calc_bonuses = _air_calc_bonuses;
        me->hooks.get_flags = _air_get_flags;
        me->hooks.gain_level = _air_gain_level;
        me->hooks.process_world = _air_process_world;

        me->boss_r_idx = mon_race_parse("E.Ariel")->id;
    }

    me->subname = titles[rank];
    me->stats[A_STR] =  0 + 2*rank;
    me->stats[A_INT] = -4;
    me->stats[A_WIS] = -4;
    me->stats[A_DEX] =  3 + 3*rank;
    me->stats[A_CON] =  0 + 2*rank;
    me->stats[A_CHR] =  0;
    me->life = 90 + 10*rank;

    return me;
}

/**********************************************************************
 *             25
 * Water Spirt -> Water Elemental
 **********************************************************************/
static void _water_birth(void) 
{ 
    object_type forge;
    
    plr_mon_race_set("E.water spirit");

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_JEWELRY_ELEMENTAL;
    forge.to_a = 15;
    add_flag(forge.flags, OF_RES_(GF_ACID));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_d = 5;
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_POLEARM, SV_TRIDENT));
    plr_birth_obj(&forge);

    plr_birth_obj_aux(TV_POTION, SV_POTION_WATER, rand_range(15, 23));
    plr_birth_light();
}

static void _water_gain_level(int new_level) 
{
    if (plr_mon_race_is_("E.water spirit") && new_level >= 25)
        plr_mon_race_evolve("E.water");
}

static void _acid_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Acid Strike");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent opponent with a corrosive blow.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_attack_special(PLR_HIT_ACID, PAC_NO_INNATE));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _water_ball_spell(int cmd, var_ptr res)
{
    int dam = plr_prorata_level(300);

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Water Ball");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a huge ball of water.");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, dam / 10);
        break;
    default:
        ball_spell(cmd, res, 4, GF_WATER2, dam);
    }
}

static void _water_gate_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Water Gate");
        break;
    case SPELL_DESC:
        var_set_string(res, "Move instantaneously to chosen watery location.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _elemental_travel(FEAT_WATER));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _water_healing_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Healing Bath");
        break;
    case SPELL_DESC:
        var_set_string(res, "If you are surrounded by water, you may heal yourself at the cost of several acts.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_heal(0, 0, 100 + plr->lev * 3));
        break;
    case SPELL_CAST:
        var_set_bool(res, _elemental_healing(FEAT_WATER));
        break;
    case SPELL_ENERGY:
        var_set_int(res, 200);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static power_info _water_powers[] = 
{
    { A_DEX, {  5,  5,  0, _acid_strike_spell}},
    { A_STR, { 12,  7, 35, acid_bolt_spell}},
    { A_STR, { 17, 10, 40, _elemental_rage_spell}},
    { A_STR, { 23, 15, 65, water_bolt_spell}},
    { A_STR, { 32,  0, 65, _water_ball_spell}},
    { A_CON, { 35,  0, 65, _water_healing_spell}},
    { A_DEX, { 40, 50, 75, _water_gate_spell}},
    {    -1, { -1, -1, -1, NULL} }
};

static int _water_get_powers(spell_info* spells, int max) 
{
    return get_powers_aux(spells, max, _water_powers);
}

static void _water_calc_bonuses(void) 
{
    res_add(GF_ACID);
    res_add(GF_WATER);
    res_add_immune(GF_STUN);

    if (plr->lev >= 25)
    {
        plr->pspeed += 3;
        plr->melt_armor = TRUE;
        res_add(GF_ACID);
        res_add(GF_WATER);
    }

    if (plr->lev >= 50)
        res_add_immune(GF_ACID);

    _calc_bonuses();
}

static void _water_get_flags(u32b flgs[OF_ARRAY_SIZE]) 
{
    add_flag(flgs, OF_RES_(GF_ACID));
    add_flag(flgs, OF_RES_(GF_WATER));
    add_flag(flgs, OF_IM_(GF_STUN));

    if (plr->lev >= 25)
        add_flag(flgs, OF_SPEED);

    if (plr->lev >= 50)
        add_flag(flgs, OF_IM_(GF_ACID));

    _get_flags(flgs);
}

static void _water_damage(obj_ptr obj)
{
    u32b flgs[OF_ARRAY_SIZE];
    char o_name[MAX_NLEN];
    int  chance = 15;

    if (!obj_is_armor(obj)) return;
    if (randint0(1000) >= chance) return;
    if (obj->ac + obj->to_a <= 0) return;

    obj_flags(obj, flgs);
    if (have_flag(flgs, OF_IM_(GF_ACID))) return;
    if (have_flag(flgs, OF_RES_(GF_ACID))) return;
    if (have_flag(flgs, OF_IGNORE_ACID) && !one_in_(10)) return;
    if (obj_is_art(obj) && !one_in_(2)) return;

    object_desc(o_name, obj, OD_OMIT_PREFIX | OD_NAME_ONLY | OD_COLOR_CODED);
    msg_format("<color:R>Your watery touch corrodes your %s!</color>", o_name);
    obj->to_a--;

    if (obj->loc.where == INV_EQUIP)
    {
        plr->update |= PU_BONUS;
        plr->window |= PW_EQUIP;
    }
    else if (obj->loc.where == INV_PACK)
        plr->window |= PW_INVEN;
    if (disturb_minor)
        disturb(1, 0);
}

static void _water_process_world(void)
{
    equip_for_each(_water_damage);
    pack_for_each(_water_damage);
}

static plr_race_ptr _water_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[2] =  {"Water Spirit", "Water Elemental"};
    int           rank = 0;

    if (plr->lev >= 25) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  25,  35,   5,  25,  16,  65,  35};
    skills_t xs = { 40,  50,  45,   0,   0,   0, 100,  75};

        me = plr_race_alloc_aux(RACE_MON_ELEMENTAL, ELEMENTAL_WATER);
        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 5;
        me->exp = 200;

        me->hooks.birth = _water_birth;
        me->hooks.get_powers = _water_get_powers;
        me->hooks.calc_bonuses = _water_calc_bonuses;
        me->hooks.get_flags = _water_get_flags;
        me->hooks.gain_level = _water_gain_level;
        me->hooks.process_world = _water_process_world;

        me->boss_r_idx = mon_race_parse("E.Moire")->id;
    }

    me->subname = titles[rank];
    me->stats[A_STR] =  2 + 2*rank;
    me->stats[A_INT] = -4;
    me->stats[A_WIS] = -4;
    me->stats[A_DEX] =  1 + rank;
    me->stats[A_CON] =  2 + 2*rank;
    me->stats[A_CHR] =  1;
    me->life = 100 + 10*rank;

    return me;
}

/**********************************************************************
 *            25                40
 * Fire Spirt -> Fire Elemental -> Magma Elemental
 **********************************************************************/
static void _fire_birth(void) 
{ 
    object_type forge;
    
    plr_mon_race_set("E.fire spirit");

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_JEWELRY_ELEMENTAL;
    forge.to_a = 15;
    add_flag(forge.flags, OF_RES_(GF_FIRE));
    add_flag(forge.flags, OF_AURA_FIRE);
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_HAFTED, SV_WHIP));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_HARD_ARMOR, SV_CHAIN_MAIL));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_FLASK, SV_ANY));
    apply_magic(&forge, 1, AM_NO_FIXED_ART);
    forge.number = (byte)rand_range(7, 12);
    plr_birth_obj(&forge);

    plr_birth_light();
}

static void _fire_gain_level(int new_level) 
{
    if (plr_mon_race_is_("E.fire spirit") && new_level >= 25)
        plr_mon_race_evolve("E.fire");
    if (plr_mon_race_is_("E.fire") && new_level >= 40)
        plr_mon_race_evolve("E.magma");
}

static void _fire_whip_spell(int cmd, var_ptr res)
{
    dice_t dice = dice_create(3 + plr->lev/7, 6, 0);
    int range = 2 + plr->lev/6;

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Fire Whip");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a short beam of fire.");
        break;
    case SPELL_INFO:
        var_printf(res, "dam %dd%d (rng %d)", dice.dd, dice.ds, range);
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_cast_beam_aux(GF_FIRE, dice, range));
        break;
    default:
        default_spell(cmd, res);
    }
}

static void _fire_storm_spell(int cmd, var_ptr res)
{
    int dam = plr_prorata_level(400);

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Fire Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a huge ball of fire.");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, dam / 10);
        break;
    default:
        ball_spell(cmd, res, 4, GF_FIRE, dam);
    }
}

static void _flaming_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Flaming Strike");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent opponent with a fiery blow.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_attack_special(PLR_HIT_FIRE, PAC_NO_INNATE));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _fire_door_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Fire Door");
        break;
    case SPELL_DESC:
        var_set_string(res, "Move instantaneously to chosen fiery location.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _elemental_travel(FEAT_LAVA));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _fire_healing_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Healing Flames");
        break;
    case SPELL_DESC:
        var_set_string(res, "If you are surrounded by lava, you may heal yourself at the cost of several acts.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_heal(0, 0, 100 + plr->lev * 3));
        break;
    case SPELL_CAST:
        var_set_bool(res, _elemental_healing(FEAT_LAVA));
        break;
    case SPELL_ENERGY:
        var_set_int(res, 250);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static power_info _fire_powers[] = 
{
    { A_STR, {  2,  3, 25, _fire_whip_spell}},
    { A_STR, {  7,  5, 35, fire_bolt_spell}},
    { A_STR, { 12,  7,  0, _flaming_strike_spell}},
    { A_DEX, { 15, 10, 40, hide_in_flame_spell}},
    { A_STR, { 20, 12, 45, fire_ball_spell}},
    { A_STR, { 22, 15, 45, plasma_bolt_spell}},
    { A_DEX, { 25, 20, 55, flow_of_lava_spell}},
    { A_STR, { 27, 25, 65, plasma_ball_spell}},
    { A_CON, { 35,  0, 65, _fire_healing_spell}},
    { A_STR, { 37,  0, 55, _fire_storm_spell}},
    { A_DEX, { 42, 50, 60, _fire_door_spell}},
    {    -1, { -1, -1, -1, NULL} }
};

static int _fire_get_powers(spell_info* spells, int max) 
{
    return get_powers_aux(spells, max, _fire_powers);
}

static void _fire_calc_bonuses(void) 
{
    res_add(GF_FIRE);
    res_add_vuln(GF_COLD);
    plr->sh_fire = TRUE;

    if (plr->lev >= 25)
    {
        plr->pspeed += 2;
        res_add(GF_FIRE);
    }

    if (plr->lev >= 40)
    {
        plr->pspeed += 3;
        plr->pass_wall = TRUE;
        plr->no_passwall_dam = TRUE;
        res_add(GF_FIRE);
        res_add(GF_ELEC);
    }

    if (plr->lev >= 50)
        res_add_immune(GF_FIRE);

    _calc_bonuses();
}

static void _fire_get_flags(u32b flgs[OF_ARRAY_SIZE]) 
{
    add_flag(flgs, OF_VULN_(GF_COLD));
    add_flag(flgs, OF_RES_(GF_FIRE));
    add_flag(flgs, OF_AURA_FIRE);
    add_flag(flgs, OF_LIGHT); /* cf calc_torch */

    if (plr->lev >= 25)
        add_flag(flgs, OF_SPEED);

    if (plr->lev >= 40)
        add_flag(flgs, OF_RES_(GF_ELEC));

    if (plr->lev >= 50)
        add_flag(flgs, OF_IM_(GF_FIRE));

    _get_flags(flgs);
}

static bool _fire_p(object_type *o_ptr)
{
    if (obj_is_art(o_ptr)) return FALSE;
    if (o_ptr->tval != TV_SCROLL && o_ptr->tval != TV_STAFF) return FALSE;
    if (obj_has_flag(o_ptr, OF_IGNORE_FIRE)) return FALSE;
    return TRUE;
}

static void _fire_process_world(void)
{
    _elemental_pack_destroy(_fire_p, "Your fiery touch burns your %s.", 40);
}

static plr_race_ptr _fire_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[3] =  {"Fire Spirit", "Fire Elemental", "Magma Elemental"};
    int           rank = 0;

    if (plr->lev >= 25) rank++;
    if (plr->lev >= 40) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  25,  35,   4,  25,  16,  65,  35};
    skills_t xs = { 40,  50,  45,   0,   0,   0, 125,  75};

        me = plr_race_alloc_aux(RACE_MON_ELEMENTAL, ELEMENTAL_FIRE);

        me->skills = bs;
        me->extra_skills = xs;
        me->infra = 5;
        me->exp = 200;
        me->boss_r_idx = mon_race_parse("E.Loge")->id;

        me->hooks.birth = _fire_birth;
        me->hooks.get_powers = _fire_get_powers;
        me->hooks.calc_bonuses = _fire_calc_bonuses;
        me->hooks.get_flags = _fire_get_flags;
        me->hooks.gain_level = _fire_gain_level;
        me->hooks.process_world = _fire_process_world;
    }

    me->subname = titles[rank];
    me->stats[A_STR] =  1 + 2*rank;
    me->stats[A_INT] = -4;
    me->stats[A_WIS] = -4;
    me->stats[A_DEX] =  0 + rank;
    me->stats[A_CON] =  1 + rank;
    me->stats[A_CHR] =  0;
    me->life = 100 + 5*rank;

    return me;
}

static name_desc_t _info[ELEMENTAL_MAX] = {
    { "Earth Elemental",
        "Earth Elementals are creatures of rock: Strong, tough and slow. "
        "They may move freely through the earth and are capable of conjuring "
        "sharp clods of earth to hurl at their foes. Their skin is very tough, "
        "and they can even turn their bodies to stone. However, being made of "
        "earth, their potions frequently turn to mud." },
    { "Air Elemental",
        "Air Elementals are creatures of electricity. They are incredibly fast, "
        "blinking in and out of sight as they shower their enemies with confusing "
        "and shocking blows. Electricity crackles menacingly about their nimble frames, "
        "tending to destroy rings, amulets, wands and rods." },
    { "Water Elemental",
        "Water Elementals are creatures of water, able to modify this ubiquitous "
        "liquid into a deadly and often corrosive weapon of destruction. Fear their "
        "rage! They cannot be stunned. Their corrosive nature erodes any armor that "
        "gets too close." },
    { "Fire Elemental",
        "Fire Elementals are creatures of flame. They have a vast arsenal of "
        "flaming attacks with which to singe the fiercest of foes. However, they "
        "must beware of cold based attacks! Being wreathed in flames, scrolls and "
        "staves are quickly burned to ash." },
};

/**********************************************************************
 * Public
 **********************************************************************/
plr_race_ptr mon_elemental_get_race(int psubrace)
{
    plr_race_ptr result = NULL;

    if (birth_hack && psubrace >= ELEMENTAL_MAX)
        psubrace = 0;

    assert(0 <= psubrace && psubrace < ELEMENTAL_MAX);

    switch (psubrace)
    {
    case ELEMENTAL_EARTH:
        result = _earth_get_race_t();
        break;
    case ELEMENTAL_AIR:
        result = _air_get_race_t();
        break;
    case ELEMENTAL_WATER:
        result = _water_get_race_t();
        break;
    case ELEMENTAL_FIRE:
        result = _fire_get_race_t();
        break;
    default: /* ?? */
        result = _earth_get_race_t();
    }

    result->name = "Elemental";
    result->desc = _desc;
    result->flags = RACE_IS_MONSTER | RACE_IS_NONLIVING;
    result->base_hp = 30;
    result->pseudo_class_id = CLASS_WARRIOR;
    result->shop_adjust = 120;

    if (birth_hack || spoiler_hack)
    {
        result->subname = _info[psubrace].name;
        result->subdesc = _info[psubrace].desc;
    }

    return result;
}


