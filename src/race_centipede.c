#include "angband.h"

#define MON_CENTIPEDE_WHITE        24
#define MON_CENTIPEDE_GREEN        42
#define MON_CENTIPEDE_BLUE         67
#define MON_CENTIPEDE_RED          77
#define MON_CENTIPEDE_CLEAR       276
#define MON_CENTIPEDE_MULTIHUED  1132

static const char * _desc = 
 "Centipedes are natural creatures with many legs. As they "
 "evolve, they change colors, but not much else seems to differentiate the various forms. "
 "Perhaps you prefer blue to red or green? Eventually, once you have evolved into your "
 "ultimate evolutionary form, you will be able to change colors at will and thereby become "
 "very powerful indeed.\n \n"
 "As a centipede, your main survival attribute will be the many pairs of boots you can wear. "
 "Hopefully, this will be enough.";

/**********************************************************************
 *                  7        14      21     28       35
 * Evolution: White -> Green -> Blue -> Red -> Clear -> Multihued
 **********************************************************************/
static void _birth(void) 
{ 
    object_type    forge;

    p_ptr->current_r_idx = MON_CENTIPEDE_WHITE;
    equip_on_change_race();
    skills_innate_init("Bite", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);
    skills_innate_init("Sting", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);
    skills_innate_init("Crawl", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);

    
    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_h = 3;
    forge.to_d = 2;
    forge.pval = 1;
    add_flag(forge.flags, OF_DEX);
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_BOOTS, SV_PAIR_OF_SOFT_LEATHER_BOOTS));
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_BOOTS, SV_PAIR_OF_SOFT_LEATHER_BOOTS));
    add_outfit(&forge);
}

static int _rank(void)
{
    switch (p_ptr->current_r_idx)
    {
    case MON_CENTIPEDE_WHITE: return 0;
    case MON_CENTIPEDE_GREEN: return 1;
    case MON_CENTIPEDE_BLUE: return 2;
    case MON_CENTIPEDE_RED: return 3;
    case MON_CENTIPEDE_CLEAR: return 4;
    case MON_CENTIPEDE_MULTIHUED: return 5;
    }
    return 0;
}

static void _gain_level(int new_level) 
{
    if (p_ptr->current_r_idx == MON_CENTIPEDE_WHITE && new_level >= 7)
    {
        p_ptr->current_r_idx = MON_CENTIPEDE_GREEN;
        equip_on_change_race();
        msg_print("You have evolved into a metallic green centipede.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_CENTIPEDE_GREEN && new_level >= 14)
    {
        p_ptr->current_r_idx = MON_CENTIPEDE_BLUE;
        equip_on_change_race();
        msg_print("You have evolved into a metallic blue centipede.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_CENTIPEDE_BLUE && new_level >= 21)
    {
        p_ptr->current_r_idx = MON_CENTIPEDE_RED;
        equip_on_change_race();
        msg_print("You have evolved into a metallic red centipede.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_CENTIPEDE_RED && new_level >= 28)
    {
        p_ptr->current_r_idx = MON_CENTIPEDE_CLEAR;
        equip_on_change_race();
        msg_print("You have evolved into a giant clear centipede.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_CENTIPEDE_CLEAR && new_level >= 35 && player_is_monster_king())
    {
        p_ptr->current_r_idx = MON_CENTIPEDE_MULTIHUED;
        equip_on_change_race();
        msg_print("You have evolved into The Multi-hued Centipede! There can only be one, you know ...");
        p_ptr->redraw |= PR_MAP;
    }
}

/**********************************************************************
 * Attacks
 **********************************************************************/
static void _calc_innate_attacks(void)
{
    int r = _rank();

    /* Bite */
    {
        innate_attack_t a = {0};

        a.dd = 1;
        a.ds = 3 + r;
        a.to_h = p_ptr->lev/3;
        a.to_d = r;
        a.weight = 70;
        a.effect[0] = GF_MISSILE;
        a.blows = 100;
        a.msg = "You bite.";
        a.name = "Bite";

        p_ptr->innate_attacks[p_ptr->innate_attack_ct++] = a;
    }

    /* Sting */
    {
        innate_attack_t a = {0};

        a.dd = 1;
        a.ds = 2 + r;
        a.to_h = p_ptr->lev/3;
        a.to_d = r;
        a.weight = 70;
        a.effect[0] = GF_MISSILE;
        if (r >= 5)
            a.effect[1] = GF_POIS;
        a.blows = 100;
        a.msg = "You sting.";
        a.name = "Sting";

        p_ptr->innate_attacks[p_ptr->innate_attack_ct++] = a;
    }

    /* Crawl */
    {
        innate_attack_t a = {0};

        a.dd = 1;
        a.ds = 3 + r;
        if (r >= 5)
            a.ds += 7;
        a.to_h = p_ptr->lev/3;
        a.to_d = r;
        a.weight = 70;
        a.effect[0] = GF_MISSILE;
        calc_innate_blows(&a, 100 + 50*r);
        a.msg = "You crawl.";
        a.name = "Crawl";

        p_ptr->innate_attacks[p_ptr->innate_attack_ct++] = a;
    }
}

/**********************************************************************
 * Bonuses
 **********************************************************************/
static void _calc_bonuses(void) 
{
    p_ptr->skill_dig += 50;
    p_ptr->pspeed += _rank();
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE]) 
{
    if (_rank())
        add_flag(flgs, OF_SPEED);
}
/**********************************************************************
 * Public Methods
 **********************************************************************/
race_t *mon_centipede_get_race(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    int           r = _rank();
    static cptr   titles[6] =  {"Giant white centipede", "Metallic green centipede", 
                                "Metallic blue centipede", "Metallic red centipede", 
                                "Giant clear centipede", "The Multi-hued Centipede"};    

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  21,  35,   5,  10,   7,  50,  10};
    skills_t xs = { 12,  10,  10,   0,   0,   0,  15,   2};

        me.skills = bs;
        me.extra_skills = xs;

        me.name = "Centipede";
        me.desc = _desc;

        me.infra = 5;
        me.exp = 100;
        me.base_hp = 15;
        me.shop_adjust = 120;

        me.calc_innate_attacks = _calc_innate_attacks;
        me.calc_bonuses = _calc_bonuses;
        me.get_flags = _get_flags;
        me.gain_level = _gain_level;
        me.birth = _birth;

        me.flags = RACE_IS_MONSTER;
        me.boss_r_idx = MON_CENTIPEDE_MULTIHUED;
        me.pseudo_class_idx = CLASS_WARRIOR;

        init = TRUE;
    }

    me.subname = titles[r];
    me.stats[A_STR] = (r + 1)/3;
    me.stats[A_INT] = -3;
    me.stats[A_WIS] = -3;
    me.stats[A_DEX] = r;
    me.stats[A_CON] = (r + 1)/2;
    me.stats[A_CHR] = -3;
    me.life = 90 + 2*r;
    me.equip_template = mon_get_equip_template();

    return &me;
}

