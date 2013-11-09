#include "angband.h"

static cptr _desc = 
    "Giants are humanoids of immense stature. There are several types of giants. "
    "Fire, Frost and Storm giants are elemental giants and gain extra resistance, "
    "elementals slays and even elemental attacks of their respective element. Titans "
    "are powerful immortal beings of legend. Their attacks often confuse their foes and they "
    "rarely fight alone.\n \n"
    "Giants are monsters so cannot choose a normal class. Instead, they must rely on their "
    "superior physical stature to pummel their opponents with mighty blows. Against a distant "
    "foe, giants are capable of hurling large boulders with devastating effect.\n \n" 
    "Giants use the same equipment slots as normal player races and have no innate attacks.";

static void _birth(void) 
{ 
    object_type    forge;

    p_ptr->current_r_idx = MON_HILL_GIANT;
    
    object_prep(&forge, lookup_kind(TV_HARD_ARMOR, SV_CHAIN_MAIL));
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_SWORD, SV_EXECUTIONERS_SWORD));
    add_outfit(&forge);
}

/******************************************************************************
 *                 20             30            40
 * Hru: Hill Giant -> Stone Giant -> Rock Giant -> Hru
 ******************************************************************************/
static power_info _hru_powers[] = {
    { A_STR, {  5,  0, 50, throw_boulder_spell} },
    { A_STR, { 30,  5, 25, stone_to_mud_spell} },
    { A_STR, { 35, 10, 45, earthquake_spell} },
    { A_STR, { 40, 15, 50, stone_skin_spell} },
    {    -1, { -1, -1, -1, NULL}}
};
static int _hru_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _hru_powers);
}
static void _hru_calc_bonuses(void) {
    if (p_ptr->lev >= 30)
    {
        p_ptr->kill_wall = TRUE;
        p_ptr->to_a += 10;
        p_ptr->dis_to_a += 10;
    }
    if (p_ptr->lev >= 40)
    {
        p_ptr->to_a += 15;
        p_ptr->dis_to_a += 15;
    }
}
static void _hru_get_flags(u32b flgs[TR_FLAG_SIZE]) {
}
static void _hru_gain_level(int new_level) {
    if (p_ptr->current_r_idx == MON_HILL_GIANT && new_level >= 20)
    {
        p_ptr->current_r_idx = MON_STONE_GIANT;
        msg_print("You have evolved into a Stone Giant.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_STONE_GIANT && new_level >= 30)
    {
        p_ptr->current_r_idx = MON_ROCK_GIANT;
        msg_print("You have evolved into a Rock Giant.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_ROCK_GIANT && new_level >= 40)
    {
        p_ptr->current_r_idx = MON_HRU;
        msg_print("You have evolved into a Hru.");
        p_ptr->redraw |= PR_MAP;
    }
}
static race_t *_hru_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[4] =  {"Hill Giant", "Stone Giant", "Rock Giant", "Hru"};    
    int           rank = 0;

    if (p_ptr->lev >= 20) rank++;
    if (p_ptr->lev >= 30) rank++;
    if (p_ptr->lev >= 40) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  30,  -1,  13,   7,  75,  55};
    skills_t xs = { 10,   7,  10,   0,   0,   0,  34,  31};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 225;

        me.birth = _birth;
        me.get_powers = _hru_get_powers;
        me.calc_bonuses = _hru_calc_bonuses;
        me.get_flags = _hru_get_flags;
        me.gain_level = _hru_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  3 + 2*rank;
    me.stats[A_INT] = -3 + rank;
    me.stats[A_WIS] = -6 + rank;
    me.stats[A_DEX] = -2 + rank;
    me.stats[A_CON] =  3 + 7*rank/4;
    me.stats[A_CHR] =  0 + rank;
    me.life = 105 + 7*rank;
    me.boss_r_idx = MON_ATLAS;

    return &me;
}

/******************************************************************************
 *                        20             30            40
 * Fire Giant: Hill Giant -> Stone Giant -> Fire Giant -> Elder Fire Giant
 ******************************************************************************/
static void _breathe_plasma_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Plasma");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes Plasma at your opponent.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, p_ptr->chp*3/10));
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, p_ptr->lev);
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        if (get_aim_dir(&dir))
        {
            msg_print("You breathe plasma...");
            fire_ball(GF_PLASMA, dir, p_ptr->chp*3/10, -3);
            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
static power_info _fire_powers[] = {
    { A_STR, {  5,  0, 50, throw_boulder_spell} },
    { A_STR, { 30,  5, 25, fire_bolt_spell} },
    { A_STR, { 32, 10, 50, fire_ball_spell} },
    { A_STR, { 35, 15, 60, plasma_bolt_spell} },
    { A_CON, { 40,  0, 65, breathe_fire_II_spell} },
    { A_CON, { 42, 10, 70, _breathe_plasma_spell} },
    {    -1, { -1, -1, -1, NULL}}
};
static int _fire_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _fire_powers);
}
static void _fire_calc_bonuses(void) {
    if (p_ptr->lev >= 30)
    {
        res_add(RES_FIRE);
        p_ptr->sh_fire = TRUE;
    }
    if (p_ptr->lev >= 40)
        res_add(RES_FIRE);
}
static void _fire_get_flags(u32b flgs[TR_FLAG_SIZE]) {
    if (p_ptr->lev >= 30)
    {
        add_flag(flgs, TR_RES_FIRE);
        add_flag(flgs, TR_SH_FIRE);
    }
    if (p_ptr->lev >= 40)
        add_flag(flgs, TR_BRAND_FIRE);
}
static void _fire_calc_weapon_bonuses(object_type *o_ptr, weapon_info_t *info_ptr)
{
    if (p_ptr->lev >= 40)
        add_flag(info_ptr->flags, TR_BRAND_FIRE);
}
static void _fire_gain_level(int new_level) {
    if (p_ptr->current_r_idx == MON_HILL_GIANT && new_level >= 20)
    {
        p_ptr->current_r_idx = MON_STONE_GIANT;
        msg_print("You have evolved into a Stone Giant.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_STONE_GIANT && new_level >= 30)
    {
        p_ptr->current_r_idx = MON_FIRE_GIANT;
        msg_print("You have evolved into a Fire Giant.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_FIRE_GIANT && new_level >= 40)
    {
        p_ptr->current_r_idx = MON_ELDER_FIRE_GIANT;
        msg_print("You have evolved into an Elder Fire Giant.");
        p_ptr->redraw |= PR_MAP;
    }
}
static race_t *_fire_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[4] =  {"Hill Giant", "Stone Giant", "Fire Giant", "Elder Fire Giant"};    
    int           rank = 0;

    if (p_ptr->lev >= 20) rank++;
    if (p_ptr->lev >= 30) rank++;
    if (p_ptr->lev >= 40) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  30,  -1,  13,   7,  75,  55};
    skills_t xs = { 10,   7,  10,   0,   0,   0,  34,  31};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 200;

        me.birth = _birth;
        me.get_powers = _fire_get_powers;
        me.calc_bonuses = _fire_calc_bonuses;
        me.calc_weapon_bonuses = _fire_calc_weapon_bonuses;
        me.get_flags = _fire_get_flags;
        me.gain_level = _fire_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  3 + 7*rank/4;
    me.stats[A_INT] = -3 + rank;
    me.stats[A_WIS] = -6 + rank;
    me.stats[A_DEX] = -2 + rank;
    me.stats[A_CON] =  3 + 7*rank/4;
    me.stats[A_CHR] =  0 + rank;
    me.life = 105 + 7*rank;
    me.boss_r_idx = MON_SURTUR;

    return &me;
}

/******************************************************************************
 *                         20             30             40
 * Frost Giant: Hill Giant -> Stone Giant -> Frost Giant -> Ice Giant
 ******************************************************************************/
static void _ice_storm_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Ice Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generate a huge ball of ice on chosen target.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, 6*p_ptr->lev));
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        if (!get_aim_dir(&dir)) return;
        fire_ball(GF_ICE, dir, 6*p_ptr->lev, 5);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
static power_info _frost_powers[] = {
    { A_STR, {  5,  0, 50, throw_boulder_spell} },
    { A_STR, { 30,  3, 25, frost_bolt_spell} },
    { A_STR, { 32,  9, 50, frost_ball_spell} },
    { A_STR, { 35, 15, 60, ice_bolt_spell} },
    { A_STR, { 40, 40, 60, _ice_storm_spell} },
    {    -1, { -1, -1, -1, NULL}}
};
static int _frost_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _frost_powers);
}
static void _frost_calc_bonuses(void) {
    if (p_ptr->lev >= 30)
    {
        res_add(RES_COLD);
        p_ptr->sh_cold = TRUE;
    }
    if (p_ptr->lev >= 40)
        res_add(RES_COLD);
}
static void _frost_get_flags(u32b flgs[TR_FLAG_SIZE]) {
    if (p_ptr->lev >= 30)
    {
        add_flag(flgs, TR_RES_COLD);
        add_flag(flgs, TR_SH_COLD);
    }
    if (p_ptr->lev >= 40)
        add_flag(flgs, TR_BRAND_COLD);
}
static void _frost_calc_weapon_bonuses(object_type *o_ptr, weapon_info_t *info_ptr)
{
    if (p_ptr->lev >= 40)
        add_flag(info_ptr->flags, TR_BRAND_COLD);
}
static void _frost_gain_level(int new_level) {
    if (p_ptr->current_r_idx == MON_HILL_GIANT && new_level >= 20)
    {
        p_ptr->current_r_idx = MON_STONE_GIANT;
        msg_print("You have evolved into a Stone Giant.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_STONE_GIANT && new_level >= 30)
    {
        p_ptr->current_r_idx = MON_FROST_GIANT;
        msg_print("You have evolved into a Frost Giant.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_FROST_GIANT && new_level >= 40)
    {
        p_ptr->current_r_idx = MON_ICE_GIANT;
        msg_print("You have evolved into an Ice Giant.");
        p_ptr->redraw |= PR_MAP;
    }
}
static race_t *_frost_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[4] =  {"Hill Giant", "Stone Giant", "Frost Giant", "Ice Giant"};    
    int           rank = 0;

    if (p_ptr->lev >= 20) rank++;
    if (p_ptr->lev >= 30) rank++;
    if (p_ptr->lev >= 40) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  30,  -1,  13,   7,  75,  55};
    skills_t xs = { 10,   7,  10,   0,   0,   0,  34,  31};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 200;

        me.birth = _birth;
        me.get_powers = _frost_get_powers;
        me.calc_bonuses = _frost_calc_bonuses;
        me.calc_weapon_bonuses = _frost_calc_weapon_bonuses;
        me.get_flags = _frost_get_flags;
        me.gain_level = _frost_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  3 + 7*rank/4;
    me.stats[A_INT] = -3 + rank;
    me.stats[A_WIS] = -6 + rank;
    me.stats[A_DEX] = -2 + rank;
    me.stats[A_CON] =  3 + 7*rank/4;
    me.stats[A_CHR] =  0 + rank;
    me.life = 105 + 7*rank;
    me.boss_r_idx = MON_YMIR;

    return &me;
}

/*******************************************************************************************
 *                         20             30             40             45
 * Storm Giant: Hill Giant -> Stone Giant -> Cloud Giant -> Storm Giant -> Elder Storm Giant
 *******************************************************************************************/
static void _breathe_storm_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes Storm at your opponent.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, p_ptr->chp*3/10));
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, p_ptr->lev);
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        if (get_aim_dir(&dir))
        {
            msg_print("You breathe storm...");
            fire_ball(GF_STORM, dir, p_ptr->chp*3/10, -3);
            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _lightning_storm_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Lightning Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generate a huge ball of lightning on chosen target.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, 7*p_ptr->lev));
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        if (!get_aim_dir(&dir)) return;
        fire_ball(GF_ELEC, dir, 7*p_ptr->lev, 5);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
static power_info _storm_powers[] = {
    { A_STR, {  5,  0, 50, throw_boulder_spell} },
    { A_STR, { 30,  3, 25, lightning_bolt_spell} },
    { A_STR, { 35, 10, 50, lightning_ball_spell} },
    { A_DEX, { 40,  2, 10, phase_door_spell} },
    { A_DEX, { 40, 10, 50, teleport_to_spell} },
    { A_STR, { 40, 40, 60, _lightning_storm_spell} },
    { A_CON, { 45, 10, 70, _breathe_storm_spell} },
    {    -1, { -1, -1, -1, NULL} }
};
static int _storm_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _storm_powers);
}
static void _storm_calc_bonuses(void) {
    if (p_ptr->lev >= 30)
        res_add(RES_ELEC);

    if (p_ptr->lev >= 40)
    {
        p_ptr->sh_elec = TRUE;
        res_add(RES_ELEC);
    }
}
static void _storm_get_flags(u32b flgs[TR_FLAG_SIZE]) {
    if (p_ptr->lev >= 30)
        add_flag(flgs, TR_RES_ELEC);
    if (p_ptr->lev >= 40)
    {
        add_flag(flgs, TR_SH_ELEC);
        add_flag(flgs, TR_BRAND_ELEC);
    }
}
static void _storm_calc_weapon_bonuses(object_type *o_ptr, weapon_info_t *info_ptr)
{
    if (p_ptr->lev >= 40)
        add_flag(info_ptr->flags, TR_BRAND_ELEC);
}
static void _storm_gain_level(int new_level) {
    if (p_ptr->current_r_idx == MON_HILL_GIANT && new_level >= 20)
    {
        p_ptr->current_r_idx = MON_STONE_GIANT;
        msg_print("You have evolved into a Stone Giant.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_STONE_GIANT && new_level >= 30)
    {
        p_ptr->current_r_idx = MON_CLOUD_GIANT;
        msg_print("You have evolved into a Cloud Giant.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_CLOUD_GIANT && new_level >= 40)
    {
        p_ptr->current_r_idx = MON_STORM_GIANT;
        msg_print("You have evolved into a Storm Giant.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_STORM_GIANT && new_level >= 45)
    {
        p_ptr->current_r_idx = MON_ELDER_STORM_GIANT;
        msg_print("You have evolved into an Elder Storm Giant.");
        p_ptr->redraw |= PR_MAP;
    }
}
static race_t *_storm_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[5] =  {"Hill Giant", "Stone Giant", "Cloud Giant", "Storm Giant", "Elder Storm Giant"};    
    int           rank = 0;

    if (p_ptr->lev >= 20) rank++;
    if (p_ptr->lev >= 30) rank++;
    if (p_ptr->lev >= 40) rank++;
    if (p_ptr->lev >= 45) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  30,  -1,  13,   7,  70,  55};
    skills_t xs = { 10,   7,  10,   0,   0,   0,  30,  30};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 250;

        me.birth = _birth;
        me.get_powers = _storm_get_powers;
        me.calc_bonuses = _storm_calc_bonuses;
        me.calc_weapon_bonuses = _storm_calc_weapon_bonuses;
        me.get_flags = _storm_get_flags;
        me.gain_level = _storm_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  3 + rank;
    me.stats[A_INT] = -3 + rank;
    me.stats[A_WIS] = -6 + rank;
    me.stats[A_DEX] = -2 + rank;
    me.stats[A_CON] =  3 + rank;
    me.stats[A_CHR] =  0 + rank;
    me.life = 105 + 3*rank;
    me.boss_r_idx = MON_TYPHOEUS;

    return &me;
}

/******************************************************************************
 *                   20             30              40
 * Titan: Hill Giant -> Stone Giant -> Lesser Titan -> Greater Titan
 ******************************************************************************/
static power_info _titan_powers[] = {
    { A_STR, {  5,   0, 50, throw_boulder_spell} },
    { A_CHR, { 30,  30, 25, summon_monsters_spell} },
    { A_DEX, { 35,  10, 50, teleport_to_spell} },
    { A_CHR, { 40,  30, 50, summon_kin_spell} },
    { A_CON, { 45, 100, 95, healing_I_spell} }, /* N.B. Casting costs are paid with hp! */
    {    -1, { -1,  -1, -1, NULL}}
};
static int _titan_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _titan_powers);
}
static void _titan_calc_bonuses(void) {
    if (p_ptr->lev >= 30)
    {
        res_add(RES_CHAOS);
        p_ptr->pspeed += 2;
    }
    if (p_ptr->lev >= 40)
        p_ptr->pspeed += 3;
}
static void _titan_get_flags(u32b flgs[TR_FLAG_SIZE]) {
    if (p_ptr->lev >= 30)
    {
        add_flag(flgs, TR_RES_CHAOS);
        add_flag(flgs, TR_SPEED);
    }
}
static void _titan_gain_level(int new_level) {
    if (p_ptr->current_r_idx == MON_HILL_GIANT && new_level >= 20)
    {
        p_ptr->current_r_idx = MON_STONE_GIANT;
        msg_print("You have evolved into a Stone Giant.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_STONE_GIANT && new_level >= 30)
    {
        p_ptr->current_r_idx = MON_LESSER_TITAN;
        msg_print("You have evolved into a Lesser Titan.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_LESSER_TITAN && new_level >= 40)
    {
        p_ptr->current_r_idx = MON_GREATER_TITAN;
        msg_print("You have evolved into a Greater Titan.");
        p_ptr->redraw |= PR_MAP;
    }
}
static race_t *_titan_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[4] =  {"Hill Giant", "Stone Giant", "Lesser Titan", "Greater Titan"};    
    int           rank = 0;

    if (p_ptr->lev >= 20) rank++;
    if (p_ptr->lev >= 30) rank++;
    if (p_ptr->lev >= 40) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  20,  32,  -1,  15,  10,  75,  50};
    skills_t xs = { 11,   8,  10,   0,   0,   0,  35,  30};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 300;

        me.birth = _birth;
        me.get_powers = _titan_get_powers;
        me.calc_bonuses = _titan_calc_bonuses;
        me.get_flags = _titan_get_flags;
        me.gain_level = _titan_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  3 + rank;
    me.stats[A_INT] =  0 + rank;
    me.stats[A_WIS] =  0 + rank;
    me.stats[A_DEX] = -2 + rank;
    me.stats[A_CON] =  3 + rank;
    me.stats[A_CHR] =  3 + rank;
    me.life = 105 + 5*rank;
    me.boss_r_idx = MON_KRONOS;

    return &me;
}

/**********************************************************************
 * Public
 **********************************************************************/
race_t *mon_giant_get_race_t(int psubrace)
{
    race_t *result = NULL;

    switch (psubrace)
    {
    case GIANT_FIRE:
        result = _fire_get_race_t();
        break;
    case GIANT_FROST:
        result = _frost_get_race_t();
        break;
    case GIANT_STORM:
        result = _storm_get_race_t();
        break;
    case GIANT_TITAN:
        result = _titan_get_race_t();
        break;
    case GIANT_HRU:
        result = _hru_get_race_t();
        break;
    default: /* Birth Menus */
        result = _fire_get_race_t();
    }

    result->name = "Giant";
    result->desc = _desc;
    result->flags = RACE_IS_MONSTER;
    result->base_hp = 46;

    return result;
}

