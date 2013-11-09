#include "angband.h"

static cptr _desc = 
    "Dragons are powerful winged serpents. They are the strongest fighters "
    "with razor sharp claws and a bone crushing bite. Each dragon has a unique "
    "type of breath that becomes more deadly as the dragon grows and matures.\n \n"
    "Due to their non-humanoid bodies, dragons are unable to wear armor, gloves "
    "or boots. However, being creatures of magic, they are able to wear 6 rings. "
    "They can also wear a helmet, a light source, a cloak and an amulet. Because "
    "of these equipment restrictions, dragons may have a difficult time covering "
    "all resistances despite the fact that each dragon has one or more innate "
    "resistances (or even immunities).\n \n"
    "Dragons begin life in a weak form, being very young. As their bodies mature, "
    "their scales grow tough and their claws sharp. Their breath grows more "
    "deadly and they frequently gain additional magical powers and resistances. "
    "All dragons can fly, but younger dragons are not so quick as their elders.";

/**********************************************************************
 * Dragon Equipment
 **********************************************************************/
static equip_template_t _equip_template = {10, { 
    {EQUIP_SLOT_RING, "Ring", 0},
    {EQUIP_SLOT_RING, "Ring", 0},
    {EQUIP_SLOT_RING, "Ring", 0},
    {EQUIP_SLOT_RING, "Ring", 0},
    {EQUIP_SLOT_RING, "Ring", 0},
    {EQUIP_SLOT_RING, "Ring", 0},
    {EQUIP_SLOT_AMULET, "Amulet", 0},
    {EQUIP_SLOT_LITE, "Light", 0},
    {EQUIP_SLOT_CLOAK, "Cloak", 0},
    {EQUIP_SLOT_HELMET, "Helm", 0},
}};

static void _dragon_birth(void) 
{ 
    object_type    forge;
    
    object_prep(&forge, lookup_kind(TV_RING, SV_RING_STR));
    forge.pval = 1;
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_RING, SV_RING_DAMAGE));
    forge.to_d = 3;
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_RING, SV_RING_ACCURACY));
    forge.to_h = 3;
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_RING, SV_RING_DEX));
    forge.pval = 1;
    add_outfit(&forge);
}

/**********************************************************************
 * Dragon Breath
 **********************************************************************/
static int _breath_effect(void)
{
    switch (p_ptr->psubrace)
    {
    case DRAGON_RED: return GF_FIRE;
    case DRAGON_WHITE: return GF_COLD;
    case DRAGON_BLUE: return GF_ELEC;
    case DRAGON_BLACK: return GF_ACID;
    case DRAGON_GREEN: return GF_POIS;
    case DRAGON_BRONZE: return GF_CONFUSION;
    case DRAGON_GOLD: return GF_SOUND;
    case DRAGON_NETHER: 
        if (p_ptr->lev >= 45)
        {
            switch (randint0(4))
            {
            case 0: return GF_NETHER;
            case 1: return GF_NEXUS;
            case 2: return GF_DISENCHANT;
            }
        }
        return GF_NETHER;
    case DRAGON_LAW: return one_in_(2) ? GF_SOUND : GF_SHARDS;
    case DRAGON_CHAOS: return one_in_(2) ? GF_CHAOS : GF_DISENCHANT;
    case DRAGON_ETHEREAL: 
        switch (randint0(2+p_ptr->lev/40))
        {
        case 0: return GF_LITE;
        case 1: return GF_DARK;
        case 2: return GF_CONFUSION;
        }
    case DRAGON_CRYSTAL: return GF_SHARDS;
    case DRAGON_BALANCE:
        switch (randint0(4))
        {
        case 0: return GF_SOUND;
        case 1: return GF_SHARDS;
        case 2: return GF_CHAOS;
        case 3: return GF_DISENCHANT;
        }
    }
    return 0;
}
cptr gf_name(int which)
{
    switch (which)
    {
    case GF_FIRE: return "fire";
    case GF_ACID: return "acid";
    case GF_COLD: return "cold";
    case GF_ELEC: return "lightning";
    case GF_POIS: return "poison";
    case GF_LITE: return "light";
    case GF_DARK: return "dark";
    case GF_CONFUSION: return "confusion";
    case GF_NETHER: return "nether";
    case GF_NEXUS: return "nexus";
    case GF_SOUND: return "sound";
    case GF_SHARDS: return "shards";
    case GF_CHAOS: return "chaos";
    case GF_DISENCHANT: return "disenchantment";
    case GF_TIME: return "time";
    case GF_MANA: return "mana";
    case GF_GRAVITY: return "gravity";
    case GF_INERT: return "inerta";
    case GF_PLASMA: return "plasma";
    case GF_FORCE: return "force";
    case GF_NUKE: return "nuke";
    }
    return "something";
}

static int _breath_amount(void)
{
    int l = p_ptr->lev;
    switch (p_ptr->psubrace)
    {
    case DRAGON_RED:
    case DRAGON_WHITE:
    case DRAGON_BLUE:
    case DRAGON_BLACK:
    case DRAGON_GREEN:
        return MAX(1, MIN(900, p_ptr->chp * (25 + l*l*l/2500) / 100));

    case DRAGON_LAW:
    case DRAGON_CHAOS:
    case DRAGON_CRYSTAL:
    case DRAGON_BRONZE:
    case DRAGON_GOLD:
        return MAX(1, MIN(450, p_ptr->chp * (20 + l*l*l*30/125000) / 100));

    case DRAGON_BALANCE:
        return MAX(1, MIN(400, p_ptr->chp * (20 + l*l*l*25/125000) / 100));

    case DRAGON_NETHER:
    case DRAGON_ETHEREAL:
        return MAX(1, MIN(375, p_ptr->chp * (20 + l*l*l*15/125000) / 100));
    }
    return 0;
}

static int _breath_cost(void)
{
    int l = p_ptr->lev;
    return MAX(l/2 + l*l*15/2500, 1);
}

static cptr _breath_desc(void)
{
    switch (p_ptr->psubrace)
    {
    case DRAGON_RED: return "fire";
    case DRAGON_WHITE: return "cold";
    case DRAGON_BLUE: return "lightning";
    case DRAGON_BLACK: return "acid";
    case DRAGON_GREEN: return "poison";
    case DRAGON_BRONZE: return "confusion";
    case DRAGON_GOLD: return "sound";
    case DRAGON_NETHER: 
        if (p_ptr->lev >= 40) return "nether, nexus or disenchantment";
        return "nether";
    case DRAGON_LAW: return "sound or shards";
    case DRAGON_CHAOS: return "chaos or disenchantment";
    case DRAGON_ETHEREAL: return "light, dark or confusion";
    case DRAGON_CRYSTAL: return "shards";
    case DRAGON_BALANCE: return "sound, shards, chaos or disenchantment";
    }
    return 0;
}


static void _breathe_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe");
        break;
    case SPELL_DESC:
        var_set_string(res, format("Breathes %s at your opponent.", _breath_desc()));
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _breath_amount()));
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, _breath_cost());
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        if (get_aim_dir(&dir))
        {
            int e = _breath_effect();
            msg_format("You breathe %s", gf_name(e));
            fire_ball(e, dir, _breath_amount(), -1 - (p_ptr->lev / 20));
            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

/**********************************************************************
 * Dragon Melee
 **********************************************************************/
static int _attack_level(void)
{
    int l = p_ptr->lev;
    switch (p_ptr->psubrace)
    {
    case DRAGON_STEEL:
        l = MAX(1, l * 130 / 100);
        break;

    case DRAGON_RED:
    case DRAGON_WHITE:
        l = MAX(1, l * 110 / 100);
        break;

    case DRAGON_BLACK:
    case DRAGON_GREEN:
        break;

    case DRAGON_BLUE:
        l = MAX(1, l * 95 / 100);
        break;

    case DRAGON_ETHEREAL:
    case DRAGON_CRYSTAL:
    case DRAGON_BRONZE:
    case DRAGON_GOLD:
        l = MAX(1, l * 90 / 100);
        break;

    case DRAGON_LAW:
    case DRAGON_CHAOS:
        l = MAX(1, l * 85 / 100);
        break;

    case DRAGON_NETHER:
    case DRAGON_BALANCE:
        l = MAX(1, l * 80 / 100);
        break;
    }
    return l;
}

static int _bite_effect(void)
{
    switch (p_ptr->psubrace)
    {
    case DRAGON_RED: return GF_FIRE;
    case DRAGON_WHITE: return GF_COLD;
    case DRAGON_BLUE: return GF_ELEC;
    case DRAGON_BLACK: return GF_ACID;
    case DRAGON_GREEN: return GF_POIS;
    case DRAGON_BRONZE: return GF_OLD_CONF;
    }
    return 0;
}

static void _calc_innate_attacks(void)
{
    int l = _attack_level();
    int to_d = l/10 + l*l/500 + l*l*l/25000;
    int to_h = l/5 + l*l/250 + l*l*l/12500;

    /* Claws */
    {
        innate_attack_t    a = {0};

        a.dd = 1 + l / 15;
        a.ds = 3 + l / 5;
        a.to_h += to_h;
        a.to_d += to_d;

        a.weight = 100 + 3 * l;
        calc_innate_blows(&a, 4);
        a.msg = "You claw %s.";
        a.name = "Claw";

        p_ptr->innate_attacks[p_ptr->innate_attack_ct++] = a;
    }
    /* Bite */
    {
        innate_attack_t    a = {0};

        a.dd = 1 + l / 9;
        a.ds = 4 + l / 3;
        a.to_h += to_h;
        a.to_d += to_d;

        a.weight = 200 + 6 * l;

        if (p_ptr->lev >= 30)
            a.effect[1] = _bite_effect();

        if (p_ptr->lev >= 40)
            calc_innate_blows(&a, 2);
        else
            a.blows = 1;
        a.msg = "You bite %s.";
        a.name = "Bite";
        p_ptr->innate_attacks[p_ptr->innate_attack_ct++] = a;
    }
}

/**********************************************************************
 * Misc Spells
 **********************************************************************/
static void _treasure_seeking_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Treasure Seeking");
        break;
    case SPELL_DESC:
        var_set_string(res, "Locate nearby magical objects and treasures.");
        break;
    case SPELL_CAST:
        detect_treasure(DETECT_RAD_DEFAULT);
        detect_objects_gold(DETECT_RAD_DEFAULT);
        detect_objects_magic(DETECT_RAD_DEFAULT);    
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

/**********************************************************************
 * Elemental Dragon (Red, White, Blue, Black, Green)
 *   Baby -> Young -> Mature -> Ancient -> Great Foo Wyrm
 **********************************************************************/
 typedef struct {
    int  r_idx[5];
    cptr r_name[5];
    int  which_res;
} _elemental_info_t;

static _elemental_info_t _elemental_info[5] = { /* relies on #define DRAGON_RED 0 ... */
    { {167, 563, 589, 644, 756},
      {"Baby Red Dragon", "Young Red Dragon", "Mature Red Dragon", "Ancient Red Dragon", "Great Hell Wyrm"},
      RES_FIRE },
    { {164, 460, 549, 617, 741},
      {"Baby White Dragon", "Young White Dragon", "Mature White Dragon", "Ancient White Dragon", "Great Ice Wyrm"},
      RES_COLD},
    { {163, 459, 560, 601, 728},
      {"Baby Blue Dragon", "Young Blue Dragon", "Mature Blue Dragon", "Ancient Blue Dragon", "Great Storm Wyrm"},
      RES_ELEC},
    { {166, 546, 592, 624, 1066},
      {"Baby Black Dragon", "Young Black Dragon", "Mature Black Dragon", "Ancient Black Dragon", "Great Bile Wyrm"},
      RES_ACID},
    { {165, 461, 561, 618, 890},
      {"Baby Green Dragon", "Young Green Dragon", "Mature Green Dragon", "Ancient Green Dragon", "Great Venom Wyrm"},
      RES_POIS},
};

static power_info _elemental_powers[] = {
    { A_CON, {  1,  0, 30, _breathe_spell}},
    { A_INT, {  5,  5, 50, _treasure_seeking_spell}},
    { A_CHR, { 10, 10, 50, scare_spell}},
    { A_CHR, { 20, 10, 50, confuse_spell}}, 
    {    -1, { -1, -1, -1, NULL} }
};
static int _elemental_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _elemental_powers);
}
static void _elemental_calc_bonuses(void) {
    int l = p_ptr->lev;
    int to_a = l/2 + l*l/100 + l*l*l/5000;
    int ac = 15 + (l/10)*5;
    int res = _elemental_info[p_ptr->psubrace].which_res;
    
    p_ptr->skill_dig += 100;

    p_ptr->ac += ac;
    p_ptr->dis_ac += ac;

    p_ptr->to_a += to_a;
    p_ptr->dis_to_a += to_a;

    res_add(res);
    p_ptr->levitation = TRUE;
    
    if (p_ptr->lev >= 20)
    {
        p_ptr->free_act = TRUE;
        p_ptr->see_inv = TRUE;
    }
    if (p_ptr->lev >= 30)
    {
        p_ptr->pspeed += 3;
        res_add(res);
        res_add(RES_CONF);
        res_add(RES_FEAR);
    }
    if (p_ptr->lev >= 40)
    {
        p_ptr->pspeed += 2;
        res_add_immune(res);
        res_add(RES_BLIND);
        switch (res)
        {
        case RES_FIRE: p_ptr->sh_fire = TRUE; break;
        case RES_COLD: p_ptr->sh_cold = TRUE; break;
        case RES_ELEC: p_ptr->sh_elec = TRUE; break;
        }
    }
}
static void _elemental_get_flags(u32b flgs[TR_FLAG_SIZE]) {
    int res = _elemental_info[p_ptr->psubrace].which_res;
    add_flag(flgs, res_get_object_flag(res));
    add_flag(flgs, TR_LEVITATION);
    if (p_ptr->lev >= 20)
    {
        add_flag(flgs, TR_FREE_ACT);
        add_flag(flgs, TR_SEE_INVIS);
    }
    if (p_ptr->lev >= 30)
    {
        add_flag(flgs, TR_SPEED);
        add_flag(flgs, TR_RES_CONF);
        add_flag(flgs, TR_RES_FEAR);
    }
    if (p_ptr->lev >= 40)
    {
        add_flag(flgs, TR_RES_BLIND);
        switch (res)
        {
        case RES_FIRE: add_flag(flgs, TR_SH_FIRE); break;
        case RES_COLD: add_flag(flgs, TR_SH_COLD); break;
        case RES_ELEC: add_flag(flgs, TR_SH_ELEC); break;
        }
    }
}
static void _elemental_get_immunities(u32b flgs[TR_FLAG_SIZE]) {
    int res = _elemental_info[p_ptr->psubrace].which_res;
    if (p_ptr->lev >= 40)
        add_flag(flgs, res_get_object_flag(res));
}
static void _elemental_birth(void) { 
    p_ptr->current_r_idx = _elemental_info[p_ptr->psubrace].r_idx[0]; 
    _dragon_birth();
}
static void _elemental_gain_level(int new_level) {
    if (p_ptr->current_r_idx == _elemental_info[p_ptr->psubrace].r_idx[0] && new_level >= 10)
    {
        p_ptr->current_r_idx = _elemental_info[p_ptr->psubrace].r_idx[1];
        msg_format("You have evolved into a %s.", _elemental_info[p_ptr->psubrace].r_name[1]);
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == _elemental_info[p_ptr->psubrace].r_idx[1] && new_level >= 20)
    {
        p_ptr->current_r_idx = _elemental_info[p_ptr->psubrace].r_idx[2];
        msg_format("You have evolved into a %s.", _elemental_info[p_ptr->psubrace].r_name[2]);
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == _elemental_info[p_ptr->psubrace].r_idx[2] && new_level >= 30)
    {
        p_ptr->current_r_idx = _elemental_info[p_ptr->psubrace].r_idx[3];
        msg_format("You have evolved into an %s.", _elemental_info[p_ptr->psubrace].r_name[3]);
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == _elemental_info[p_ptr->psubrace].r_idx[3] && new_level >= 40)
    {
        p_ptr->current_r_idx = _elemental_info[p_ptr->psubrace].r_idx[4];
        msg_format("You have evolved into a %s.", _elemental_info[p_ptr->psubrace].r_name[4]);
        p_ptr->redraw |= PR_MAP;
    }
}
static race_t *_elemental_get_race_t(int subrace)
{
    static race_t me = {0};
    static bool   init = FALSE;
    int           rank = 0;

    if (p_ptr->lev >= 10) rank++;
    if (p_ptr->lev >= 20) rank++;
    if (p_ptr->lev >= 30) rank++;
    if (p_ptr->lev >= 40) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  35,  38,   2,  25,  26,  70,  30};
    skills_t xs = {  8,   9,  10,   0,   0,   0,  20,   7};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 250;

        me.birth = _elemental_birth;
        me.get_powers = _elemental_get_powers;
        me.calc_bonuses = _elemental_calc_bonuses;
        me.get_flags = _elemental_get_flags;
        me.get_immunities = _elemental_get_immunities;
        me.gain_level = _elemental_gain_level;
        init = TRUE;
    }

    me.subname = _elemental_info[subrace].r_name[rank];
    me.stats[A_STR] =  1 + rank;
    me.stats[A_INT] = -1 + rank;
    me.stats[A_WIS] = -6 + rank;
    me.stats[A_DEX] = -2 + rank;
    me.stats[A_CON] =  0 + rank;
    me.stats[A_CHR] = -1 + rank;
    me.life = 100 + 5*rank;

    return &me;
}

/**********************************************************************
 * Nether: Shadow Drake -> Death Drake -> Spectral Wyrm
 **********************************************************************/
static power_info _nether_powers[] = {
    { A_CON, {  1,  0, 30, _breathe_spell}},
    { A_INT, {  5,  5, 50, _treasure_seeking_spell}},
    { A_CHR, { 10, 10, 50, scare_spell}},
    { A_CHR, { 10, 10, 50, confuse_spell}}, 
    { A_CHR, { 10, 10, 50, slow_spell}}, 
    { A_DEX, { 45,  5, 30, phase_door_spell}}, 
    { A_CHR, { 45, 90, 90, summon_hi_dragon_spell}}, 
    {    -1, { -1, -1, -1, NULL} }
};
static int _nether_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _nether_powers);
}
static void _nether_calc_bonuses(void) {
    int l = p_ptr->lev;
    int to_a = l/2 + l*l/100 + l*l*l/5000;
    int ac = 15 + (l/10)*2;
    
    p_ptr->ac += ac;
    p_ptr->dis_ac += ac;

    p_ptr->to_a += to_a;
    p_ptr->dis_to_a += to_a;

    res_add(RES_NETHER);
    p_ptr->levitation = TRUE;
    
    if (p_ptr->lev >= 20)
    {
        p_ptr->free_act = TRUE;
        p_ptr->see_inv = TRUE;
    }
    if (p_ptr->lev >= 30)
    {
        p_ptr->pspeed += 3;
        res_add(RES_COLD);
        res_add(RES_CONF);
        res_add(RES_FEAR);
        res_add(RES_TELEPORT);
        p_ptr->pass_wall = TRUE;
        p_ptr->no_passwall_dam = TRUE;
    }
    if (p_ptr->lev >= 45)
    {
        p_ptr->align -= 200;
        p_ptr->pspeed += 2;
        p_ptr->sh_cold = TRUE;
        res_add(RES_POIS);
        res_add_immune(RES_NETHER);
        res_add(RES_NEXUS);
        res_add(RES_DISEN);
        res_add(RES_TELEPORT);
    }
}
static void _nether_get_flags(u32b flgs[TR_FLAG_SIZE]) {
    add_flag(flgs, TR_RES_NETHER);
    add_flag(flgs, TR_LEVITATION);
    if (p_ptr->lev >= 20)
    {
        add_flag(flgs, TR_FREE_ACT);
        add_flag(flgs, TR_SEE_INVIS);
    }
    if (p_ptr->lev >= 30)
    {
        add_flag(flgs, TR_SPEED);
        add_flag(flgs, TR_RES_COLD);
        add_flag(flgs, TR_RES_CONF);
        add_flag(flgs, TR_RES_FEAR);
    }
    if (p_ptr->lev >= 45)
    {
        add_flag(flgs, TR_SH_COLD);
        add_flag(flgs, TR_RES_POIS);
        add_flag(flgs, TR_RES_NEXUS);
        add_flag(flgs, TR_RES_DISEN);
    }
}
static void _nether_get_immunities(u32b flgs[TR_FLAG_SIZE]) {
    if (p_ptr->lev >= 45)
        add_flag(flgs, TR_RES_NETHER);
}
static void _nether_birth(void) { 
    p_ptr->current_r_idx = MON_SHADOW_DRAKE; 
    _dragon_birth();
}

static void _nether_gain_level(int new_level) {
    if (p_ptr->current_r_idx == MON_SHADOW_DRAKE && new_level >= 30)
    {
        p_ptr->current_r_idx = MON_DEATH_DRAKE;
        msg_print("You have evolved into a Death Drake.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_DEATH_DRAKE && new_level >= 45)
    {
        p_ptr->current_r_idx = MON_SPECTRAL_WYRM;
        msg_print("You have evolved into a Spectral Wyrm.");
        p_ptr->redraw |= PR_MAP;
    }
}

static race_t *_nether_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[3] =  {"Shadow Drake", "Death Drake", "Spectral Wyrm"};    
    int           rank = 0;

    if (p_ptr->lev >= 30) rank++;
    if (p_ptr->lev >= 45) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  35,  38,   4,  25,  26,  50,  30};
    skills_t xs = {  8,  10,  11,   0,   0,   0,  15,   7};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 350;

        me.birth = _nether_birth;
        me.get_powers = _nether_get_powers;
        me.calc_bonuses = _nether_calc_bonuses;
        me.get_flags = _nether_get_flags;
        me.get_immunities = _nether_get_immunities;
        me.gain_level = _nether_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  0 + 2*rank;
    me.stats[A_INT] = -1 + 2*rank;
    me.stats[A_WIS] = -6 + rank;
    me.stats[A_DEX] = -2 + rank;
    me.stats[A_CON] = -1 + rank;
    me.stats[A_CHR] = -1 + 3*rank;
    me.life = 90 + 5*rank;

    return &me;
}

/**********************************************************************
 * Law: Law Drake -> Great Wyrm of Law
 **********************************************************************/
static power_info _law_powers[] = {
    { A_CON, {  1,  0, 30, _breathe_spell}},
    { A_INT, {  5,  5, 50, _treasure_seeking_spell}},
    { A_CHR, { 10, 10, 50, scare_spell}},
    { A_CHR, { 10, 10, 50, confuse_spell}}, 
    { A_CHR, { 10, 10, 50, slow_spell}}, 
    { A_CHR, { 40, 70, 65, summon_dragon_spell}}, 
    {    -1, { -1, -1, -1, NULL} }
};
static int _law_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _law_powers);
}
static void _law_calc_bonuses(void) {
    int l = p_ptr->lev;
    int to_a = l/2 + l*l/100 + l*l*l/5000;
    int ac = 15 + (l/10)*2;
    
    p_ptr->ac += ac;
    p_ptr->dis_ac += ac;

    p_ptr->to_a += to_a;
    p_ptr->dis_to_a += to_a;

    res_add(RES_SOUND);
    res_add(RES_SHARDS);
    p_ptr->levitation = TRUE;
    
    if (p_ptr->lev >= 20)
    {
        p_ptr->free_act = TRUE;
        p_ptr->see_inv = TRUE;
    }
    if (p_ptr->lev >= 30)
    {
        res_add(RES_CONF);
        res_add(RES_FEAR);
    }
    if (p_ptr->lev >= 40)
    {
        p_ptr->align += 200;
        p_ptr->pspeed += 5;
        res_add(RES_SOUND);
        res_add(RES_SHARDS);
    }
}
static void _law_get_flags(u32b flgs[TR_FLAG_SIZE]) {
    add_flag(flgs, TR_RES_SOUND);
    add_flag(flgs, TR_RES_SHARDS);
    add_flag(flgs, TR_LEVITATION);
    if (p_ptr->lev >= 20)
    {
        add_flag(flgs, TR_FREE_ACT);
        add_flag(flgs, TR_SEE_INVIS);
    }
    if (p_ptr->lev >= 30)
    {
        add_flag(flgs, TR_RES_CONF);
        add_flag(flgs, TR_RES_FEAR);
    }
    if (p_ptr->lev >= 40)
    {
        add_flag(flgs, TR_SPEED);
    }
}
static void _law_birth(void) { 
    p_ptr->current_r_idx = MON_LAW_DRAKE; 
    _dragon_birth();
}
static void _law_gain_level(int new_level) {
    if (p_ptr->current_r_idx == MON_LAW_DRAKE && new_level >= 40)
    {
        p_ptr->current_r_idx = MON_GREAT_WYRM_OF_LAW;
        msg_print("You have evolved into a Great Wyrm of Law.");
        p_ptr->redraw |= PR_MAP;
    }
}
static race_t *_law_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[2] =  {"Law Drake", "Great Wyrm of Law"};    
    int           rank = 0;

    if (p_ptr->lev >= 40) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  40,  40,   2,  25,  26,  55,  30};
    skills_t xs = {  8,  11,  11,   0,   0,   0,  15,   7};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 300;

        me.birth = _law_birth;
        me.get_powers = _law_get_powers;
        me.calc_bonuses = _law_calc_bonuses;
        me.get_flags = _law_get_flags;
        me.gain_level = _law_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  0 + 5*rank;
    me.stats[A_INT] = -1 + 5*rank;
    me.stats[A_WIS] = -6 + 2*rank;
    me.stats[A_DEX] = -2 + 3*rank;
    me.stats[A_CON] = -1 + 4*rank;
    me.stats[A_CHR] = -1 + 5*rank;
    me.life = 100 + 10*rank;

    return &me;
}

/**********************************************************************
 * Chaos: Chaos Drake -> Great Wyrm of Chaos
 **********************************************************************/
static power_info _chaos_powers[] = {
    { A_CON, {  1,  0, 30, _breathe_spell}},
    { A_INT, {  5,  5, 50, _treasure_seeking_spell}},
    { A_CHR, { 10, 10, 50, scare_spell}},
    { A_CHR, { 10, 10, 50, confuse_spell}}, 
    { A_CHR, { 10, 10, 50, slow_spell}}, 
    { A_CHR, { 40, 70, 65, summon_dragon_spell}}, 
    {    -1, { -1, -1, -1, NULL} }
};
static int _chaos_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _chaos_powers);
}
static void _chaos_calc_bonuses(void) {
    int l = p_ptr->lev;
    int to_a = l/2 + l*l/100 + l*l*l/5000;
    int ac = 15 + (l/10)*2;
    
    p_ptr->ac += ac;
    p_ptr->dis_ac += ac;

    p_ptr->to_a += to_a;
    p_ptr->dis_to_a += to_a;

    res_add(RES_CHAOS);
    res_add(RES_DISEN);
    p_ptr->levitation = TRUE;
    
    if (p_ptr->lev >= 20)
    {
        p_ptr->free_act = TRUE;
        p_ptr->see_inv = TRUE;
    }
    if (p_ptr->lev >= 30)
    {
        res_add(RES_CONF);
        res_add(RES_FEAR);
    }
    if (p_ptr->lev >= 40)
    {
        p_ptr->align -= 200;
        p_ptr->pspeed += 5;
        res_add(RES_CHAOS);
        res_add(RES_DISEN);
    }
}
static void _chaos_get_flags(u32b flgs[TR_FLAG_SIZE]) {
    add_flag(flgs, TR_RES_CHAOS);
    add_flag(flgs, TR_RES_DISEN);
    add_flag(flgs, TR_LEVITATION);
    if (p_ptr->lev >= 20)
    {
        add_flag(flgs, TR_FREE_ACT);
        add_flag(flgs, TR_SEE_INVIS);
    }
    if (p_ptr->lev >= 30)
    {
        add_flag(flgs, TR_RES_CONF);
        add_flag(flgs, TR_RES_FEAR);
    }
    if (p_ptr->lev >= 40)
    {
        add_flag(flgs, TR_SPEED);
    }
}
static void _chaos_birth(void) { 
    p_ptr->current_r_idx = MON_CHAOS_DRAKE; 
    _dragon_birth();
}
static void _chaos_gain_level(int new_level) {
    if (p_ptr->current_r_idx == MON_CHAOS_DRAKE && new_level >= 40)
    {
        p_ptr->current_r_idx = MON_GREAT_WYRM_OF_CHAOS;
        msg_print("You have evolved into a Great Wyrm of Chaos.");
        p_ptr->redraw |= PR_MAP;
    }
}
static race_t *_chaos_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[2] =  {"Chaos Drake", "Great Wyrm of Chaos"};    
    int           rank = 0;

    if (p_ptr->lev >= 40) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  40,  40,   2,  25,  26,  55,  30};
    skills_t xs = {  8,  11,  11,   0,   0,   0,  15,   7};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 300;

        me.birth = _chaos_birth;
        me.get_powers = _chaos_get_powers;
        me.calc_bonuses = _chaos_calc_bonuses;
        me.get_flags = _chaos_get_flags;
        me.gain_level = _chaos_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  0 + 5*rank;
    me.stats[A_INT] = -1 + 5*rank;
    me.stats[A_WIS] = -6 + 2*rank;
    me.stats[A_DEX] = -2 + 3*rank;
    me.stats[A_CON] = -1 + 4*rank;
    me.stats[A_CHR] = -1 + 5*rank;
    me.life = 100 + 10*rank;

    return &me;
}

/**********************************************************************
 * Balance: Balance Drake -> Great Wyrm of Balance
 **********************************************************************/
static power_info _balance_powers[] = {
    { A_CON, {  1,  0, 30, _breathe_spell}},
    { A_INT, {  5,  5, 50, _treasure_seeking_spell}},
    { A_CHR, { 10, 10, 50, scare_spell}},
    { A_CHR, { 10, 10, 50, confuse_spell}}, 
    { A_CHR, { 10, 10, 50, slow_spell}}, 
    { A_CHR, { 40, 70, 65, summon_dragon_spell}}, 
    { A_CHR, { 50, 90, 80, summon_hi_dragon_spell}}, 
    {    -1, { -1, -1, -1, NULL} }
};
static int _balance_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _balance_powers);
}
static void _balance_calc_bonuses(void) {
    int l = p_ptr->lev;
    int to_a = l/2 + l*l/100 + l*l*l/5000;
    int ac = 10 + (l/10)*2;
    
    p_ptr->ac += ac;
    p_ptr->dis_ac += ac;

    p_ptr->to_a += to_a;
    p_ptr->dis_to_a += to_a;

    res_add(RES_SOUND);
    res_add(RES_SHARDS);
    res_add(RES_CHAOS);
    res_add(RES_DISEN);
    p_ptr->levitation = TRUE;
    
    if (p_ptr->lev >= 20)
    {
        p_ptr->free_act = TRUE;
        p_ptr->see_inv = TRUE;
    }
    if (p_ptr->lev >= 30)
    {
        res_add(RES_CONF);
        res_add(RES_FEAR);
    }
    if (p_ptr->lev >= 40)
    {
        p_ptr->pspeed += 5;
    }
}
static void _balance_get_flags(u32b flgs[TR_FLAG_SIZE]) {
    add_flag(flgs, TR_RES_SOUND);
    add_flag(flgs, TR_RES_SHARDS);
    add_flag(flgs, TR_RES_CHAOS);
    add_flag(flgs, TR_RES_DISEN);
    add_flag(flgs, TR_LEVITATION);
    if (p_ptr->lev >= 20)
    {
        add_flag(flgs, TR_FREE_ACT);
        add_flag(flgs, TR_SEE_INVIS);
    }
    if (p_ptr->lev >= 30)
    {
        add_flag(flgs, TR_RES_CONF);
        add_flag(flgs, TR_RES_FEAR);
    }
    if (p_ptr->lev >= 40)
    {
        add_flag(flgs, TR_SPEED);
    }
}
static void _balance_birth(void) { 
    p_ptr->current_r_idx = MON_BALANCE_DRAKE; 
    _dragon_birth();
}
static void _balance_gain_level(int new_level) {
    if (p_ptr->current_r_idx == MON_BALANCE_DRAKE && new_level >= 40)
    {
        p_ptr->current_r_idx = MON_GREAT_WYRM_OF_BALANCE;
        msg_print("You have evolved into a Great Wyrm of Balance.");
        p_ptr->redraw |= PR_MAP;
    }
}
static race_t *_balance_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[2] =  {"Balance Drake", "Great Wyrm of Balance"};    
    int           rank = 0;

    if (p_ptr->lev >= 40) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  35,  35,   2,  25,  26,  50,  30};
    skills_t xs = {  8,  10,  10,   0,   0,   0,  15,   7};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 350;

        me.birth = _balance_birth;
        me.get_powers = _balance_get_powers;
        me.calc_bonuses = _balance_calc_bonuses;
        me.get_flags = _balance_get_flags;
        me.gain_level = _balance_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  0 + 4*rank;
    me.stats[A_INT] = -1 + 4*rank;
    me.stats[A_WIS] = -6 + 2*rank;
    me.stats[A_DEX] = -2 + 3*rank;
    me.stats[A_CON] = -1 + 3*rank;
    me.stats[A_CHR] = -1 + 5*rank;
    me.life = 95 + 10*rank;

    return &me;
}

/**********************************************************************
 * Ethereal: Pseudo Dragon -> Ethereal Drake -> Ethereal Dragon
 **********************************************************************/
static power_info _ethereal_powers[] = {
    { A_CON, {  1,  0, 30, _breathe_spell}},
    { A_INT, {  5,  5, 50, _treasure_seeking_spell}},
    { A_CHR, { 10, 10, 50, scare_spell}},
    { A_CHR, { 10, 10, 50, confuse_spell}}, 
    { A_CHR, { 10, 10, 50, slow_spell}}, 
    {    -1, { -1, -1, -1, NULL} }
};
static int _ethereal_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _ethereal_powers);
}
static void _ethereal_calc_bonuses(void) {
    int l = p_ptr->lev;
    int to_a = l/2 + l*l/100 + l*l*l/5000;
    int ac = 15 + (l/10)*2;
    
    p_ptr->ac += ac;
    p_ptr->dis_ac += ac;

    p_ptr->to_a += to_a;
    p_ptr->dis_to_a += to_a;

    res_add(RES_LITE);
    res_add(RES_DARK);
    p_ptr->levitation = TRUE;
    
    if (p_ptr->lev >= 20)
    {
        p_ptr->pass_wall = TRUE;
        p_ptr->no_passwall_dam = TRUE;
        p_ptr->free_act = TRUE;
        p_ptr->see_inv = TRUE;
    }
    if (p_ptr->lev >= 30)
    {
        res_add(RES_CONF);
        res_add(RES_FEAR);
    }
    if (p_ptr->lev >= 40)
    {
        p_ptr->pspeed += 5;
        res_add(RES_LITE);
        res_add(RES_DARK);
        res_add(RES_CONF);
    }
}
static void _ethereal_get_flags(u32b flgs[TR_FLAG_SIZE]) {
    add_flag(flgs, TR_RES_LITE);
    add_flag(flgs, TR_RES_DARK);
    add_flag(flgs, TR_LEVITATION);
    if (p_ptr->lev >= 20)
    {
        add_flag(flgs, TR_FREE_ACT);
        add_flag(flgs, TR_SEE_INVIS);
    }
    if (p_ptr->lev >= 30)
    {
        add_flag(flgs, TR_RES_CONF);
        add_flag(flgs, TR_RES_FEAR);
    }
    if (p_ptr->lev >= 40)
    {
        add_flag(flgs, TR_SPEED);
    }
}
static void _ethereal_birth(void) { 
    p_ptr->current_r_idx = MON_PSEUDO_DRAGON; 
    _dragon_birth();
}
static void _ethereal_gain_level(int new_level) {
    if (p_ptr->current_r_idx == MON_PSEUDO_DRAGON && new_level >= 20)
    {
        p_ptr->current_r_idx = MON_ETHEREAL_DRAKE;
        msg_print("You have evolved into an Ethereal Drake.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_ETHEREAL_DRAKE && new_level >= 40)
    {
        p_ptr->current_r_idx = MON_ETHEREAL_DRAGON;
        msg_print("You have evolved into an Ethereal Dragon.");
        p_ptr->redraw |= PR_MAP;
    }
}
static race_t *_ethereal_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[3] =  {"Pseudo Dragon", "Ethereal Drake", "Ethereal Dragon"};    
    int           rank = 0;

    if (p_ptr->lev >= 20) rank++;
    if (p_ptr->lev >= 40) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  35,  37,   4,  25,  26,  52,  30};
    skills_t xs = {  8,  10,  11,   0,   0,   0,  15,   7};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 250;

        me.birth = _ethereal_birth;
        me.get_powers = _ethereal_get_powers;
        me.calc_bonuses = _ethereal_calc_bonuses;
        me.get_flags = _ethereal_get_flags;
        me.gain_level = _ethereal_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  0 + 2*rank;
    me.stats[A_INT] = -1 + 2*rank;
    me.stats[A_WIS] = -6 + rank;
    me.stats[A_DEX] = -2 + 2*rank;
    me.stats[A_CON] = -1 + 2*rank;
    me.stats[A_CHR] = -1 + 2*rank;
    me.life = 95 + 7*rank;

    return &me;
}

/**********************************************************************
 * Crystal: Crystal Drake -> Great Crystal Drake
 **********************************************************************/
static power_info _crystal_powers[] = {
    { A_CON, {  1,  0, 30, _breathe_spell}},
    { A_INT, {  5,  5, 50, _treasure_seeking_spell}},
    { A_CHR, { 10, 10, 50, scare_spell}},
    { A_CHR, { 10, 10, 50, confuse_spell}}, 
    { A_CHR, { 10, 10, 50, slow_spell}}, 
    {    -1, { -1, -1, -1, NULL} }
};
static int _crystal_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _crystal_powers);
}
static void _crystal_calc_bonuses(void) {
    int l = p_ptr->lev;
    int to_a = l/2 + l*l/50 + l*l*l/2500;
    int ac = 15 + (l/10)*2;
    
    p_ptr->ac += ac;
    p_ptr->dis_ac += ac;

    p_ptr->to_a += to_a;
    p_ptr->dis_to_a += to_a;

    res_add(RES_COLD);
    res_add(RES_SHARDS);
    p_ptr->levitation = TRUE;
    if (p_ptr->lev >= 10)
    {
        p_ptr->pspeed++;
    }    
    if (p_ptr->lev >= 20)
    {
        p_ptr->pspeed++;
        p_ptr->free_act = TRUE;
        p_ptr->see_inv = TRUE;
    }
    if (p_ptr->lev >= 30)
    {
        p_ptr->pspeed++;
        res_add(RES_CONF);
        res_add(RES_FEAR);
    }
    if (p_ptr->lev >= 40)
    {
        p_ptr->pspeed += 2;
        res_add(RES_SHARDS);
        p_ptr->reflect = TRUE;
    }
}
static void _crystal_get_flags(u32b flgs[TR_FLAG_SIZE]) {
    add_flag(flgs, TR_RES_COLD);
    add_flag(flgs, TR_RES_SHARDS);
    add_flag(flgs, TR_LEVITATION);
    if (p_ptr->lev >= 10)
    {
        add_flag(flgs, TR_SPEED);
    }
    if (p_ptr->lev >= 20)
    {
        add_flag(flgs, TR_FREE_ACT);
        add_flag(flgs, TR_SEE_INVIS);
    }
    if (p_ptr->lev >= 30)
    {
        add_flag(flgs, TR_RES_CONF);
        add_flag(flgs, TR_RES_FEAR);
    }
    if (p_ptr->lev >= 40)
    {
        add_flag(flgs, TR_REFLECT);
    }
}
static void _crystal_birth(void) { 
    p_ptr->current_r_idx = MON_CRYSTAL_DRAKE; 
    _dragon_birth();
}
static void _crystal_gain_level(int new_level) {
    if (p_ptr->current_r_idx == MON_CRYSTAL_DRAKE && new_level >= 40)
    {
        p_ptr->current_r_idx = MON_GREAT_CRYSTAL_DRAKE;
        msg_print("You have evolved into a Great Crystal Drake.");
        p_ptr->redraw |= PR_MAP;
    }
}
static race_t *_crystal_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[2] =  {"Crystal Drake", "Great Crystal Drake"};    
    int           rank = 0;

    if (p_ptr->lev >= 40) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  35,  40,   1,  25,  26,  70,  30};
    skills_t xs = {  8,   7,  12,   0,   0,   0,  22,   7};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 275;

        me.birth = _crystal_birth;
        me.get_powers = _crystal_get_powers;
        me.calc_bonuses = _crystal_calc_bonuses;
        me.get_flags = _crystal_get_flags;
        me.gain_level = _crystal_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  1 + 5*rank;
    me.stats[A_INT] = -1 + 5*rank;
    me.stats[A_WIS] = -6 + 2*rank;
    me.stats[A_DEX] =  0 + 3*rank;
    me.stats[A_CON] =  0 + 4*rank;
    me.stats[A_CHR] =  0 + 3*rank;
    me.life = 100 + 15*rank;

    return &me;
}

/**********************************************************************
 * Bronze: Young -> Mature -> Ancient
 **********************************************************************/
static power_info _bronze_powers[] = {
    { A_CON, {  1,  0, 30, _breathe_spell}},
    { A_INT, {  5,  5, 50, _treasure_seeking_spell}},
    { A_CHR, { 10, 10, 50, scare_spell}},
    { A_CHR, { 10, 10, 50, confuse_spell}}, 
    {    -1, { -1, -1, -1, NULL} }
};
static int _bronze_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _bronze_powers);
}
static void _bronze_calc_bonuses(void) {
    int l = p_ptr->lev;
    int to_a = l/2 + l*l/100 + l*l*l/5000;
    int ac = 15 + (l/10)*2;
    
    p_ptr->ac += ac;
    p_ptr->dis_ac += ac;

    p_ptr->to_a += to_a;
    p_ptr->dis_to_a += to_a;

    res_add(RES_CONF);
    p_ptr->levitation = TRUE;
    
    if (p_ptr->lev >= 20)
    {
        p_ptr->free_act = TRUE;
        p_ptr->see_inv = TRUE;
    }
    if (p_ptr->lev >= 30)
    {
        res_add(RES_CONF);
        res_add(RES_FEAR);
        p_ptr->pspeed += 3;
    }
    if (p_ptr->lev >= 40)
    {
        p_ptr->pspeed += 2;
    }
}
static void _bronze_get_flags(u32b flgs[TR_FLAG_SIZE]) {
    add_flag(flgs, TR_RES_CONF);
    add_flag(flgs, TR_LEVITATION);
    if (p_ptr->lev >= 20)
    {
        add_flag(flgs, TR_FREE_ACT);
        add_flag(flgs, TR_SEE_INVIS);
    }
    if (p_ptr->lev >= 30)
    {
        add_flag(flgs, TR_RES_FEAR);
        add_flag(flgs, TR_SPEED);
    }
}
static void _bronze_birth(void) { 
    p_ptr->current_r_idx = MON_YOUNG_BRONZE_DRAGON; 
    _dragon_birth();
}
static void _bronze_gain_level(int new_level) {
    if (p_ptr->current_r_idx == MON_YOUNG_BRONZE_DRAGON && new_level >= 20)
    {
        p_ptr->current_r_idx = MON_MATURE_BRONZE_DRAGON;
        msg_print("You have evolved into a Mature Bronze Dragon.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_MATURE_BRONZE_DRAGON && new_level >= 30)
    {
        p_ptr->current_r_idx = MON_ANCIENT_BRONZE_DRAGON;
        msg_print("You have evolved into an Ancient Bronze Dragon.");
        p_ptr->redraw |= PR_MAP;
    }
}
static race_t *_bronze_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[3] =  {"Young Bronze Dragon", "Mature Bronze Dragon", "Ancient Bronze Dragon"};    
    int           rank = 0;

    if (p_ptr->lev >= 20) rank++;
    if (p_ptr->lev >= 30) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  35,  38,   3,  25,  26,  55,  30};
    skills_t xs = {  8,  10,  11,   0,   0,   0,  15,   7};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 250;

        me.birth = _bronze_birth;
        me.get_powers = _bronze_get_powers;
        me.calc_bonuses = _bronze_calc_bonuses;
        me.get_flags = _bronze_get_flags;
        me.gain_level = _bronze_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  0 + 2*rank;
    me.stats[A_INT] = -1 + 2*rank;
    me.stats[A_WIS] = -6 + rank;
    me.stats[A_DEX] = -2 + 2*rank;
    me.stats[A_CON] = -1 + 2*rank;
    me.stats[A_CHR] = -1 + 2*rank;
    me.life = 100 + 5*rank;

    return &me;
}

/**********************************************************************
 * Gold: Young -> Mature -> Ancient
 **********************************************************************/
static power_info _gold_powers[] = {
    { A_CON, {  1,  0, 30, _breathe_spell}},
    { A_INT, {  5,  5, 50, _treasure_seeking_spell}},
    { A_CHR, { 10, 10, 50, scare_spell}},
    { A_CHR, { 10, 10, 50, confuse_spell}}, 
    {    -1, { -1, -1, -1, NULL} }
};
static int _gold_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _gold_powers);
}
static void _gold_calc_bonuses(void) {
    int l = p_ptr->lev;
    int to_a = l/2 + l*l/100 + l*l*l/5000;
    int ac = 15 + (l/10)*2;
    
    p_ptr->ac += ac;
    p_ptr->dis_ac += ac;

    p_ptr->to_a += to_a;
    p_ptr->dis_to_a += to_a;

    res_add(RES_SOUND);
    p_ptr->levitation = TRUE;
    
    if (p_ptr->lev >= 20)
    {
        p_ptr->free_act = TRUE;
        p_ptr->see_inv = TRUE;
    }
    if (p_ptr->lev >= 30)
    {
        res_add(RES_CONF);
        res_add(RES_FEAR);
        p_ptr->pspeed += 3;
    }
    if (p_ptr->lev >= 40)
    {
        res_add(RES_SOUND);
        p_ptr->pspeed += 2;
    }
}
static void _gold_get_flags(u32b flgs[TR_FLAG_SIZE]) {
    add_flag(flgs, TR_RES_SOUND);
    add_flag(flgs, TR_LEVITATION);
    if (p_ptr->lev >= 20)
    {
        add_flag(flgs, TR_FREE_ACT);
        add_flag(flgs, TR_SEE_INVIS);
    }
    if (p_ptr->lev >= 30)
    {
        add_flag(flgs, TR_RES_CONF);
        add_flag(flgs, TR_RES_FEAR);
        add_flag(flgs, TR_SPEED);
    }
}
static void _gold_birth(void) { 
    p_ptr->current_r_idx = MON_YOUNG_GOLD_DRAGON; 
    _dragon_birth();
}
static void _gold_gain_level(int new_level) {
    if (p_ptr->current_r_idx == MON_YOUNG_GOLD_DRAGON && new_level >= 20)
    {
        p_ptr->current_r_idx = MON_MATURE_GOLD_DRAGON;
        msg_print("You have evolved into a Mature Gold Dragon.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_MATURE_GOLD_DRAGON && new_level >= 30)
    {
        p_ptr->current_r_idx = MON_ANCIENT_GOLD_DRAGON;
        msg_print("You have evolved into an Ancient Gold Dragon.");
        p_ptr->redraw |= PR_MAP;
    }
}
static race_t *_gold_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[3] =  {"Young Gold Dragon", "Mature Gold Dragon", "Ancient Gold Dragon"};    
    int           rank = 0;

    if (p_ptr->lev >= 20) rank++;
    if (p_ptr->lev >= 30) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  35,  38,   2,  25,  26,  55,  30};
    skills_t xs = {  8,   9,  11,   0,   0,   0,  20,   7};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 250;

        me.birth = _gold_birth;
        me.get_powers = _gold_get_powers;
        me.calc_bonuses = _gold_calc_bonuses;
        me.get_flags = _gold_get_flags;
        me.gain_level = _gold_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  0 + 2*rank;
    me.stats[A_INT] = -1 + 2*rank;
    me.stats[A_WIS] = -6 + rank;
    me.stats[A_DEX] = -2 + 2*rank;
    me.stats[A_CON] = -1 + 2*rank;
    me.stats[A_CHR] = -1 + 2*rank;
    me.life = 100 + 5*rank;

    return &me;
}

/**********************************************************************
 * Steel: Stone Dragon -> Steel Dragon
 **********************************************************************/
static void _steel_calc_bonuses(void) {
    int l = p_ptr->lev;
    int to_a = l + l*l/50 + l*l*l/2500;
    int ac = 15 + (l/10)*2;
    
    p_ptr->ac += ac;
    p_ptr->dis_ac += ac;

    p_ptr->to_a += to_a;
    p_ptr->dis_to_a += to_a;

    if (p_ptr->lev < 40)
        res_add_vuln(RES_COLD);

    res_add(RES_FIRE);
    res_add(RES_ELEC);
    res_add(RES_POIS);
    p_ptr->levitation = TRUE;
    p_ptr->no_cut = TRUE;
    
    if (p_ptr->lev >= 20)
    {
        p_ptr->free_act = TRUE;
        p_ptr->see_inv = TRUE;
    }
    if (p_ptr->lev >= 30)
    {
        res_add(RES_CONF);
        res_add(RES_FEAR);
        p_ptr->no_stun = TRUE;
    }
    if (p_ptr->lev >= 40)
    {
        res_add(RES_SHARDS);
        p_ptr->pspeed += 2;
    }
}
static void _steel_get_flags(u32b flgs[TR_FLAG_SIZE]) {
    add_flag(flgs, TR_RES_FIRE);
    add_flag(flgs, TR_RES_ELEC);
    add_flag(flgs, TR_RES_POIS);
    add_flag(flgs, TR_LEVITATION);
    if (p_ptr->lev >= 10)
    {
    }
    if (p_ptr->lev >= 20)
    {
        add_flag(flgs, TR_FREE_ACT);
        add_flag(flgs, TR_SEE_INVIS);
    }
    if (p_ptr->lev >= 30)
    {
        add_flag(flgs, TR_RES_CONF);
        add_flag(flgs, TR_RES_FEAR);
    }
    if (p_ptr->lev >= 40)
    {
        add_flag(flgs, TR_RES_SHARDS);
        add_flag(flgs, TR_SPEED);
    }
}
static void _steel_get_vulnerabilities(u32b flgs[TR_FLAG_SIZE]) {
    if (p_ptr->lev < 40)
        add_flag(flgs, TR_RES_COLD);
}
static void _steel_birth(void) { 
    p_ptr->current_r_idx = MON_STONE_DRAGON; 
    _dragon_birth();
}
static void _steel_gain_level(int new_level) {
    if (p_ptr->current_r_idx == MON_STONE_DRAGON && new_level >= 40)
    {
        p_ptr->current_r_idx = MON_STEEL_DRAGON;
        msg_print("You have evolved into a Steel Dragon.");
        p_ptr->redraw |= PR_MAP;
    }
}
static race_t *_steel_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[2] =  {"Stone Dragon", "Steel Dragon"};    
    int           rank = 0;

    if (p_ptr->lev >= 40) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 28,  18,  40,   0,  10,   7,  75,  30};
    skills_t xs = {  8,   7,  15,   0,   0,   0,  30,   7};

        me.skills = bs;
        me.extra_skills = xs;

        me.infra = 5;
        me.exp = 250;

        me.birth = _steel_birth;
        me.calc_bonuses = _steel_calc_bonuses;
        me.get_flags = _steel_get_flags;
        me.get_vulnerabilities = _steel_get_vulnerabilities;
        me.gain_level = _steel_gain_level;
        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  5 + (p_ptr->lev / 10);
    me.stats[A_INT] = -6;
    me.stats[A_WIS] = -6;
    me.stats[A_DEX] =  0 - (p_ptr->lev / 10);
    me.stats[A_CON] =  4 + (p_ptr->lev / 10);
    me.stats[A_CHR] =  0 + (p_ptr->lev / 10);
    me.life = 125 + 5*(p_ptr->lev / 10);

    return &me;
}

/**********************************************************************
 * Public
 **********************************************************************/
race_t *mon_dragon_get_race_t(int psubrace)
{
    race_t *result = NULL;

    switch (psubrace)
    {
    case DRAGON_RED:
    case DRAGON_WHITE:
    case DRAGON_BLUE:
    case DRAGON_BLACK:
    case DRAGON_GREEN:
        result = _elemental_get_race_t(psubrace);
        break;
    case DRAGON_NETHER:
        result = _nether_get_race_t();
        break;
    case DRAGON_LAW:
        result = _law_get_race_t();
        break;
    case DRAGON_CHAOS:
        result = _chaos_get_race_t();
        break;
    case DRAGON_BALANCE:
        result = _balance_get_race_t();
        break;
    case DRAGON_ETHEREAL:
        result = _ethereal_get_race_t();
        break;
    case DRAGON_CRYSTAL:
        result = _crystal_get_race_t();
        break;
    case DRAGON_BRONZE:
        result = _bronze_get_race_t();
        break;
    case DRAGON_GOLD:
        result = _gold_get_race_t();
        break;
    case DRAGON_STEEL:
        result = _steel_get_race_t();
        break;
    default: /* Birth Menus */
        result = _nether_get_race_t();
    }

    result->name = "Dragon";
    result->desc = _desc;
    result->flags = RACE_IS_MONSTER;
    result->calc_innate_attacks = _calc_innate_attacks;
    result->equip_template = &_equip_template;
    result->base_hp = 40;

    result->boss_r_idx = MON_GLAURUNG;
    return result;
}


