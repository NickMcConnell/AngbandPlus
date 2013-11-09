#include "angband.h"

static cptr _desc = 
    "Elementals are mindless creatures animated from a single "
    "elemental form. As a species, elementals can never be confused or cut, "
    "and they are immune to the eldritch horror (being virtually mindless). "
    "They rarely feel fear and are resistant to poison. In addition, they "
    "gain specific powers and abilities depending on their form.\n \n"
    "For example, Earth Elementals are slow but gain bonuses to AC due "
    "to their tough skins. They are resistant to shards and may even turn "
    "their skins to stone. At home in elemental earth, they may travel "
    "freely through rocky confines.\n \n"
    "Air Elementals are shockingly fast, but perhaps that is just the "
    "crackle of their electrified bodies? They may hurl bolts and balls "
    "electricity at their enemies and may even imbue their weapons with "
    "deadly lightning.\n \n"
    "Fire Elementals are somewhat fast (They definitely run circles around "
    "their earthen brethren) and are cloaked in fire. Of course, they may attack "
    "with hell's fury but need to be on the lookout for cold wielding foes.\n \n"
    "Finally, there are the Water Elementals, creatures able to conjure deadly "
    "water bolts. They are immune to stunning.";

static void _calc_bonuses(void) 
{
    res_add(RES_CONF);
    res_add(RES_FEAR);
    p_ptr->no_cut = TRUE;
    p_ptr->no_eldritch = TRUE;
    p_ptr->levitation = TRUE;

    if (p_ptr->lev >= 5)
        res_add(RES_POIS);
    if (p_ptr->lev >= 10)
        p_ptr->see_inv = TRUE;
    if (p_ptr->lev >= 15)
        p_ptr->free_act = TRUE;
}

static void _get_flags(u32b flgs[TR_FLAG_SIZE]) 
{
    add_flag(flgs, TR_RES_CONF);
    add_flag(flgs, TR_RES_FEAR);
    add_flag(flgs, TR_LEVITATION);
    if (p_ptr->lev >= 5)
        add_flag(flgs, TR_RES_POIS);
    if (p_ptr->lev >= 10)
        add_flag(flgs, TR_SEE_INVIS);
    if (p_ptr->lev >= 15)
        add_flag(flgs, TR_FREE_ACT);
}

static bool _do_blow(int type)
{
    int x, y;
    int dir;
    int m_idx = 0;

    /* For ergonomics sake, use currently targeted monster.  This allows
       a macro of \e*tmaa or similar to pick an adjacent foe, while
       \emaa*t won't work, since get_rep_dir2() won't allow a target. */
    if (use_old_target && target_okay())
    {
        y = target_row;
        x = target_col;
        m_idx = cave[y][x].m_idx;
        if (m_idx)
        {
            if (m_list[m_idx].cdis > 1)
                m_idx = 0;
            else
                dir = 5;
        }
    }

    if (!m_idx)
    {
        if (!get_rep_dir2(&dir)) return FALSE;
        if (dir == 5) return FALSE;

        y = py + ddy[dir];
        x = px + ddx[dir];
        m_idx = cave[y][x].m_idx;

        if (!m_idx)
        {
            msg_print("There is no monster there.");
            return FALSE;
        }
    }

    if (m_idx)
        py_attack(y, x, type);

    return TRUE;
}

static bool _elemental_travel(int flag)
{
    int  rng = p_ptr->lev / 2 + 10;
    int  x, y;

    if (!tgt_pt(&x, &y, rng)) return FALSE;
    if (!in_bounds(y, x)) return FALSE;

    if (!cave_have_flag_bold(y, x, flag))
    {
        msg_print("Failed! You are out of your element!");
        teleport_player((p_ptr->lev + 2) * 2, TELEPORT_PASSIVE);
    }
    else if (one_in_(7))
    {
        msg_print("You failed to travel correctly!");
        teleport_player((p_ptr->lev + 2) * 2, TELEPORT_PASSIVE);
    }
    else
        teleport_player_to(y, x, 0);

    return TRUE;
}

static void _elemental_rage_spell(int cmd, variant *res)
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

/**********************************************************************
 *             25
 * Earth Spirt -> Earth Elemental
 **********************************************************************/
static void _earth_birth(void) 
{ 
    object_type forge;
    
    object_prep(&forge, lookup_kind(TV_RING, SV_RING_STR));
    forge.pval = 3;
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_RING, SV_RING_DAMAGE));
    forge.to_d = 6;
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_HAFTED, SV_CLUB));
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_HARD_ARMOR, SV_CHAIN_MAIL));
    add_outfit(&forge);

    p_ptr->current_r_idx = MON_EARTH_SPIRIT; 
}

static void _earth_gain_level(int new_level) 
{
    if (p_ptr->current_r_idx == MON_EARTH_SPIRIT && new_level >= 25)
    {
        p_ptr->current_r_idx = MON_EARTH_ELEMENTAL;
        msg_print("You have evolved into an Earth Elemental.");
        p_ptr->redraw |= PR_MAP;
    }
}

static void _shard_bolt_spell(int cmd, variant *res)
{
    int dd = 1 + p_ptr->lev / 3;
    int ds = 8;

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shard Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a bolt of shards at chosen target.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(dd, ds, 0));
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        if (!get_aim_dir(&dir)) return;
        fire_bolt(GF_SHARDS, dir, damroll(dd, ds));
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _earthen_portal_spell(int cmd, variant *res)
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
        var_set_bool(res, _elemental_travel(FF_WALL));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _shard_ball_spell(int cmd, variant *res)
{
    int l = p_ptr->lev;
    int dam = l*2 + l*l/25 + l*l*l/1250;

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shard Ball");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a ball of shards at chosent target");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, dam));
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        if (!get_aim_dir(&dir)) return;
        fire_ball(GF_SHARDS, dir, dam, 2);
        var_set_bool(res, TRUE);
        break;
    }
    case SPELL_COST_EXTRA:
        var_set_int(res, dam / 6);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _wall_of_earth_spell(int cmd, variant *res)
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
    { A_STR, {  1,  1, 35, stone_to_mud_spell}},
    { A_STR, {  7,  5, 35, _shard_bolt_spell}},
    { A_CON, { 10, 10, 40, stone_skin_spell}},
    { A_CON, { 15, 10, 40, sense_surroundings_spell}},
    { A_STR, { 20, 15, 40, earthquake_spell}},
    { A_STR, { 25,  0, 50, _shard_ball_spell}},
    { A_CON, { 35, 50, 60, _wall_of_earth_spell}},
    { A_DEX, { 42, 75, 70, _earthen_portal_spell}},
    {    -1, { -1, -1, -1, NULL} }
};

static int _earth_get_powers(spell_info* spells, int max) 
{
    return get_powers_aux(spells, max, _earth_powers);
}

static void _earth_calc_bonuses(void) 
{
    int l = p_ptr->lev;
    int to_a = l/3 + l*l/150 + l*l*l/7500;

    p_ptr->to_a += to_a;
    p_ptr->dis_to_a += to_a;

    res_add(RES_FIRE);
    res_add(RES_COLD);
    res_add(RES_ELEC);
    res_add(RES_SHARDS);
    p_ptr->pass_wall = TRUE;
    p_ptr->no_passwall_dam = TRUE;
    p_ptr->regenerate = TRUE;

    p_ptr->pspeed--;
    if (p_ptr->lev >= 25)
        p_ptr->pspeed--;

    _calc_bonuses();
}

static void _earth_get_flags(u32b flgs[TR_FLAG_SIZE]) 
{
    add_flag(flgs, TR_RES_FIRE);
    add_flag(flgs, TR_RES_COLD);
    add_flag(flgs, TR_RES_ELEC);
    add_flag(flgs, TR_RES_SHARDS);
    add_flag(flgs, TR_SPEED);
    add_flag(flgs, TR_REGEN);

    _get_flags(flgs);
}

static race_t *_earth_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[2] =  {"Earth Spirit", "Earth Elemental"};
    int           rank = 0;

    if (p_ptr->lev >= 25) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  18,  40,   5,  25,  16,  70,  25};
    skills_t xs = {  8,   8,  12,   0,   0,   0,  30,   7};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 170;

        me.birth = _earth_birth;
        me.get_powers = _earth_get_powers;
        me.calc_bonuses = _earth_calc_bonuses;
        me.get_flags = _earth_get_flags;
        me.gain_level = _earth_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  2 + 3*rank;
    me.stats[A_INT] = -5;
    me.stats[A_WIS] = -5;
    me.stats[A_DEX] = -2 - 2*rank;
    me.stats[A_CON] =  2 + 4*rank;
    me.stats[A_CHR] =  0;
    me.life = 105 + 15*rank;
    me.boss_r_idx = MON_QUAKER;

    return &me;
}

/**********************************************************************
 *           25
 * Air Spirt -> Air Elemental
 **********************************************************************/
static void _air_birth(void) 
{ 
    object_type forge;
    
    object_prep(&forge, lookup_kind(TV_RING, SV_RING_DEX));
    forge.pval = 3;
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_SWORD, SV_LONG_SWORD));
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_HARD_ARMOR, SV_CHAIN_MAIL));
    add_outfit(&forge);

    p_ptr->current_r_idx = MON_AIR_SPIRIT; 
}

static void _air_gain_level(int new_level) 
{
    if (p_ptr->current_r_idx == MON_AIR_SPIRIT && new_level >= 25)
    {
        p_ptr->current_r_idx = MON_AIR_ELEMENTAL;
        msg_print("You have evolved into an Air Elemental.");
        p_ptr->redraw |= PR_MAP;
    }
}

static void _confusing_strike_spell(int cmd, variant *res)
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
        var_set_bool(res, _do_blow(HISSATSU_CONF));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _lightning_strike_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Lightning Strike");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent opponent with a shocking blow.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _do_blow(HISSATSU_ELEC));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void _lightning_storm_spell(int cmd, variant *res)
{
    int l = p_ptr->lev;
    int dam = l*2 + l*l/25 + l*l*l/1250;

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Lightning Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a huge ball of electricity.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, dam));
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        if (!get_aim_dir(&dir)) return;
        fire_ball(GF_ELEC, dir, dam, 4);
        var_set_bool(res, TRUE);
        break;
    }
    case SPELL_COST_EXTRA:
        var_set_int(res, dam / 10);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _whirlwind_spell(int cmd, variant *res)
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

static void _sky_gate_spell(int cmd, variant *res)
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
        var_set_bool(res, _elemental_travel(FF_FLOOR));
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
    res_add(RES_ELEC);

    p_ptr->pspeed += 2 + p_ptr->lev / 10;
    if (p_ptr->lev >= 25)
    {
        res_add(RES_ELEC);
        res_add(RES_ACID);
        res_add(RES_FIRE);
        res_add(RES_COLD);
        p_ptr->sh_elec = TRUE;
    }
    if (p_ptr->lev >= 50)
        res_add_immune(RES_ELEC);

    _calc_bonuses();
}

static void _air_get_flags(u32b flgs[TR_FLAG_SIZE]) 
{
    add_flag(flgs, TR_RES_ELEC);
    add_flag(flgs, TR_SPEED);

    if (p_ptr->lev >= 25)
    {
        add_flag(flgs, TR_RES_ACID);
        add_flag(flgs, TR_RES_FIRE);
        add_flag(flgs, TR_RES_COLD);
        add_flag(flgs, TR_SH_ELEC);
    }

    _get_flags(flgs);
}

static void _air_get_immunities(u32b flgs[TR_FLAG_SIZE])
{
    if (p_ptr->lev >= 50)
        add_flag(flgs, TR_RES_ELEC);
}

static race_t *_air_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[2] =  {"Air Spirit", "Air Elemental"};
    int           rank = 0;

    if (p_ptr->lev >= 25) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  25,  35,   6,  25,  16,  55,  35};
    skills_t xs = {  8,  10,   9,   0,   0,   0,  20,  15};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 225;

        me.birth = _air_birth;
        me.get_powers = _air_get_powers;
        me.calc_bonuses = _air_calc_bonuses;
        me.get_flags = _air_get_flags;
        me.get_immunities = _air_get_immunities;
        me.gain_level = _air_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  0 + 2*rank;
    me.stats[A_INT] = -4;
    me.stats[A_WIS] = -4;
    me.stats[A_DEX] =  3 + 3*rank;
    me.stats[A_CON] =  0 + 2*rank;
    me.stats[A_CHR] =  0;
    me.life = 90 + 10*rank;
    me.boss_r_idx = MON_ARIEL;

    return &me;
}

/**********************************************************************
 *             25
 * Water Spirt -> Water Elemental
 **********************************************************************/
static void _water_birth(void) 
{ 
    object_type forge;
    
    object_prep(&forge, lookup_kind(TV_RING, SV_RING_DAMAGE));
    forge.to_d = 4;
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_POLEARM, SV_TRIDENT));
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_HARD_ARMOR, SV_CHAIN_MAIL));
    add_outfit(&forge);

    p_ptr->current_r_idx = MON_WATER_SPIRIT; 
}

static void _water_gain_level(int new_level) 
{
    if (p_ptr->current_r_idx == MON_WATER_SPIRIT && new_level >= 25)
    {
        p_ptr->current_r_idx = MON_WATER_ELEMENTAL;
        msg_print("You have evolved into a Water Elemental.");
        p_ptr->redraw |= PR_MAP;
    }
}

static void _acid_strike_spell(int cmd, variant *res)
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
        var_set_bool(res, _do_blow(PY_ATTACK_ACID));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _water_ball_spell(int cmd, variant *res)
{
    int l = p_ptr->lev;
    int dam = l*2 + l*l/25 + l*l*l/1250;

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Water Ball");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a huge ball of water.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, dam));
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        if (!get_aim_dir(&dir)) return;
        fire_ball(GF_WATER, dir, dam, 4);
        if (p_ptr->lev >= 35)
            fire_ball_hide(GF_WATER_FLOW, dir, 1, 4);
        var_set_bool(res, TRUE);
        break;
    }
    case SPELL_COST_EXTRA:
        var_set_int(res, dam / 10);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _water_gate_spell(int cmd, variant *res)
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
        var_set_bool(res, _elemental_travel(FF_WATER));
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
    { A_DEX, { 40, 50, 75, _water_gate_spell}},
    {    -1, { -1, -1, -1, NULL} }
};

static int _water_get_powers(spell_info* spells, int max) 
{
    return get_powers_aux(spells, max, _water_powers);
}

static void _water_calc_bonuses(void) 
{
    res_add(RES_ACID);
    p_ptr->no_stun = TRUE;

    if (p_ptr->lev >= 25)
    {
        p_ptr->pspeed += 3;
        p_ptr->melt_armor = TRUE;
    }

    if (p_ptr->lev >= 35)
        res_add_immune(RES_ACID);

    _calc_bonuses();
}

static void _water_get_flags(u32b flgs[TR_FLAG_SIZE]) 
{
    add_flag(flgs, TR_RES_ACID);

    if (p_ptr->lev >= 25)
        add_flag(flgs, TR_SPEED);

    _get_flags(flgs);
}

static void _water_get_immunities(u32b flgs[TR_FLAG_SIZE])
{
    if (p_ptr->lev >= 35)
        add_flag(flgs, TR_RES_ACID);
}

static race_t *_water_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[2] =  {"Water Spirit", "Water Elemental"};
    int           rank = 0;

    if (p_ptr->lev >= 25) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  25,  35,   6,  25,  16,  65,  35};
    skills_t xs = {  8,  10,   9,   0,   0,   0,  20,  15};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 200;

        me.birth = _water_birth;
        me.get_powers = _water_get_powers;
        me.get_immunities = _water_get_immunities;
        me.calc_bonuses = _water_calc_bonuses;
        me.get_flags = _water_get_flags;
        me.gain_level = _water_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  2 + 2*rank;
    me.stats[A_INT] = -4;
    me.stats[A_WIS] = -4;
    me.stats[A_DEX] =  1 + rank;
    me.stats[A_CON] =  2 + 2*rank;
    me.stats[A_CHR] =  1;
    me.life = 100 + 10*rank;
    me.boss_r_idx = MON_MOIRE;

    return &me;
}

/**********************************************************************
 *            25                40
 * Fire Spirt -> Fire Elemental -> Magma Elemental
 **********************************************************************/
static void _fire_birth(void) 
{ 
    object_type forge;
    
    object_prep(&forge, lookup_kind(TV_RING, SV_RING_DAMAGE));
    forge.to_d = 5;
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_HAFTED, SV_WHIP));
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_HARD_ARMOR, SV_CHAIN_MAIL));
    add_outfit(&forge);

    p_ptr->current_r_idx = MON_FIRE_SPIRIT; 
}

static void _fire_gain_level(int new_level) 
{
    if (p_ptr->current_r_idx == MON_FIRE_SPIRIT && new_level >= 25)
    {
        p_ptr->current_r_idx = MON_FIRE_ELEMENTAL;
        msg_print("You have evolved into a Fire Elemental.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_FIRE_ELEMENTAL && new_level >= 40)
    {
        p_ptr->current_r_idx = MON_MAGMA_ELEMENTAL;
        msg_print("You have evolved into a Magma Elemental.");
        p_ptr->redraw |= PR_MAP;
    }
}

static void _fire_whip_spell(int cmd, variant *res)
{
    int dd = 3 + p_ptr->lev / 7;
    int ds = 6;
    int range = 2 + p_ptr->lev / 6;

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Fire Whip");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a short beam of fire.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("dam %dd%d (rng %d)", dd, ds, range));
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        project_length = range;
        if (!get_aim_dir(&dir)) return;
        fire_beam(GF_FIRE, dir, damroll(dd, ds));
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _fire_storm_spell(int cmd, variant *res)
{
    int l = p_ptr->lev;
    int dam = l*2 + l*l/25 + l*l*l/1250;

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Fire Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a huge ball of fire.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, dam));
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        if (!get_aim_dir(&dir)) return;
        fire_ball(GF_FIRE, dir, dam, 4);
        var_set_bool(res, TRUE);
        break;
    }
    case SPELL_COST_EXTRA:
        var_set_int(res, dam / 10);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _flaming_strike_spell(int cmd, variant *res)
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
        var_set_bool(res, _do_blow(HISSATSU_FIRE));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _fire_door_spell(int cmd, variant *res)
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
        var_set_bool(res, _elemental_travel(FF_LAVA));
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
    res_add(RES_FIRE);
    res_add_vuln(RES_COLD);
    p_ptr->sh_fire = TRUE;

    if (p_ptr->lev >= 25)
        p_ptr->pspeed += 2;

    if (p_ptr->lev >= 40)
    {
        p_ptr->pspeed += 3;
        p_ptr->pass_wall = TRUE;
        p_ptr->no_passwall_dam = TRUE;
        res_add(RES_ELEC);
    }

    if (p_ptr->lev >= 25)
        res_add_immune(RES_FIRE);

    _calc_bonuses();
}

static void _fire_get_flags(u32b flgs[TR_FLAG_SIZE]) 
{
    add_flag(flgs, TR_RES_FIRE);
    add_flag(flgs, TR_SH_FIRE);

    if (p_ptr->lev >= 25)
        add_flag(flgs, TR_SPEED);

    if (p_ptr->lev >= 40)
        add_flag(flgs, TR_RES_ELEC);

    _get_flags(flgs);
}

static void _fire_get_immunities(u32b flgs[TR_FLAG_SIZE])
{
    if (p_ptr->lev >= 25)
        add_flag(flgs, TR_RES_FIRE);
}

static void _fire_get_vulnerabilities(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_COLD);
}

static race_t *_fire_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[3] =  {"Fire Spirit", "Fire Elemental", "Magma Elemental"};
    int           rank = 0;

    if (p_ptr->lev >= 25) rank++;
    if (p_ptr->lev >= 40) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  25,  35,   6,  25,  16,  65,  35};
    skills_t xs = {  8,  10,   9,   0,   0,   0,  25,  15};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 200;

        me.birth = _fire_birth;
        me.get_powers = _fire_get_powers;
        me.calc_bonuses = _fire_calc_bonuses;
        me.get_flags = _fire_get_flags;
        me.get_immunities = _fire_get_immunities;
        me.get_vulnerabilities = _fire_get_vulnerabilities;
        me.gain_level = _fire_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  1 + 2*rank;
    me.stats[A_INT] = -4;
    me.stats[A_WIS] = -4;
    me.stats[A_DEX] =  0 + rank;
    me.stats[A_CON] =  1 + rank;
    me.stats[A_CHR] =  0;
    me.life = 100 + 5*rank;
    me.boss_r_idx = MON_LOGE;

    return &me;
}

/**********************************************************************
 * Public
 **********************************************************************/
race_t *mon_elemental_get_race_t(int psubrace)
{
    race_t *result = NULL;

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
    result->flags = RACE_IS_MONSTER;
    /* TODO: Nonliving makes sense, but then what will elementals eat?
    result->flags = RACE_IS_MONSTER | RACE_IS_NONLIVING;*/
    result->base_hp = 30;

    return result;
}


