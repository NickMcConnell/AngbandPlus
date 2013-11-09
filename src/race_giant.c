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
 *                        20             30
 * Fire Giant: Hill Giant -> Stone Giant -> Fire Giant
 ******************************************************************************/
static power_info _fire_powers[] = {
    { A_STR, {  5,  0, 50, throw_boulder_spell} },
    { A_STR, { 30,  5, 25, fire_bolt_spell} },
    { A_STR, { 32, 10, 50, fire_ball_spell} },
    { A_STR, { 35, 15, 60, plasma_bolt_spell} },
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
    if (p_ptr->lev >= 45)
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
}
static race_t *_fire_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[3] =  {"Hill Giant", "Stone Giant", "Fire Giant"};    
    int           rank = 0;

    if (p_ptr->lev >= 20) rank++;
    if (p_ptr->lev >= 30) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  30,  -1,  13,   7,  75,  55};
    skills_t xs = { 10,   7,  10,   0,   0,   0,  34,  31};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 250;

        me.birth = _birth;
        me.get_powers = _fire_get_powers;
        me.calc_bonuses = _fire_calc_bonuses;
        me.calc_weapon_bonuses = _fire_calc_weapon_bonuses;
        me.get_flags = _fire_get_flags;
        me.gain_level = _fire_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  3 + 2*rank;
    me.stats[A_INT] = -3 + rank;
    me.stats[A_WIS] = -6 + rank;
    me.stats[A_DEX] = -2 + rank;
    me.stats[A_CON] =  3 + 2*rank;
    me.stats[A_CHR] =  0 + rank;
    me.life = 105 + 10*rank;
    me.boss_r_idx = MON_SURTUR;

    return &me;
}

/******************************************************************************
 *                         20             30
 * Frost Giant: Hill Giant -> Stone Giant -> Frost Giant
 ******************************************************************************/
static power_info _frost_powers[] = {
    { A_STR, {  5,  0, 50, throw_boulder_spell} },
    { A_STR, { 30,  3, 25, frost_bolt_spell} },
    { A_STR, { 32,  9, 50, frost_ball_spell} },
    { A_STR, { 35, 15, 60, ice_bolt_spell} },
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
    if (p_ptr->lev >= 45)
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
}
static race_t *_frost_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[3] =  {"Hill Giant", "Stone Giant", "Frost Giant"};    
    int           rank = 0;

    if (p_ptr->lev >= 20) rank++;
    if (p_ptr->lev >= 30) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  30,  -1,  13,   7,  75,  55};
    skills_t xs = { 10,   7,  10,   0,   0,   0,  34,  31};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 250;

        me.birth = _birth;
        me.get_powers = _frost_get_powers;
        me.calc_bonuses = _frost_calc_bonuses;
        me.calc_weapon_bonuses = _frost_calc_weapon_bonuses;
        me.get_flags = _frost_get_flags;
        me.gain_level = _frost_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  3 + 2*rank;
    me.stats[A_INT] = -3 + rank;
    me.stats[A_WIS] = -6 + rank;
    me.stats[A_DEX] = -2 + rank;
    me.stats[A_CON] =  3 + 2*rank;
    me.stats[A_CHR] =  0 + rank;
    me.life = 105 + 10*rank;
    me.boss_r_idx = MON_YMIR;

    return &me;
}

/******************************************************************************
 *                         20             30             40
 * Storm Giant: Hill Giant -> Stone Giant -> Cloud Giant -> Storm Giant
 ******************************************************************************/
static power_info _storm_powers[] = {
    { A_STR, {  5,  0, 50, throw_boulder_spell} },
    { A_STR, { 30,  3, 25, lightning_bolt_spell} },
    { A_STR, { 35, 10, 50, lightning_ball_spell} },
    { A_DEX, { 40,  2, 10, phase_door_spell} },
    { A_DEX, { 40, 10, 50, teleport_to_spell} },
    {    -1, { -1, -1, -1, NULL} }
};
static int _storm_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _storm_powers);
}
static void _storm_calc_bonuses(void) {
    if (p_ptr->lev >= 30)
        res_add(RES_ELEC);

    if (p_ptr->lev >= 40)
        p_ptr->sh_elec = TRUE;

    if (p_ptr->lev >= 45)
        res_add(RES_ELEC);
}
static void _storm_get_flags(u32b flgs[TR_FLAG_SIZE]) {
    if (p_ptr->lev >= 30)
        add_flag(flgs, TR_RES_ELEC);
    if (p_ptr->lev >= 40)
        add_flag(flgs, TR_SH_ELEC);
    if (p_ptr->lev >= 40)
        add_flag(flgs, TR_BRAND_ELEC);
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
}
static race_t *_storm_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[4] =  {"Hill Giant", "Stone Giant", "Cloud Giant", "Storm Giant"};    
    int           rank = 0;

    if (p_ptr->lev >= 20) rank++;
    if (p_ptr->lev >= 30) rank++;
    if (p_ptr->lev >= 40) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  30,  -1,  13,   7,  70,  55};
    skills_t xs = { 10,   7,  10,   0,   0,   0,  30,  30};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 300;

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
    me.life = 105 + 4*rank;
    me.boss_r_idx = MON_ATLAS; /* TODO */

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
        me.exp = 350;

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
    default: /* Birth Menus */
        result = _fire_get_race_t();
    }

    result->name = "Giant";
    result->desc = _desc;
    result->flags = RACE_IS_MONSTER;
    result->base_hp = 46;

    return result;
}

