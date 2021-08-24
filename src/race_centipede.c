#include "angband.h"

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

    plr_mon_race_set("c.white");
    skills_innate_init("Bite", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);
    skills_innate_init("Sting", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);
    skills_innate_init("Crawl", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);


    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_h = 3;
    forge.to_d = 2;
    forge.pval = 1;
    add_flag(forge.flags, OF_DEX);
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_BOOTS, SV_PAIR_OF_SOFT_LEATHER_BOOTS));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_BOOTS, SV_PAIR_OF_SOFT_LEATHER_BOOTS));
    plr_birth_obj(&forge);

    plr_birth_food();
    plr_birth_light();
}

static int _rank(void)
{
    if (plr_mon_race_is_("c.white")) return 0;
    if (plr_mon_race_is_("c.green")) return 1;
    if (plr_mon_race_is_("c.blue")) return 2;
    if (plr_mon_race_is_("c.red")) return 3;
    if (plr_mon_race_is_("c.clear")) return 4;
    if (plr_mon_race_is_("c.Multi-hued")) return 5;
    return 0;
}

static void _gain_level(int new_level)
{
    if (plr_mon_race_is_("c.white") && new_level >= 7)
        plr_mon_race_evolve("c.green");
    if (plr_mon_race_is_("c.green") && new_level >= 14)
        plr_mon_race_evolve("c.blue");
    if (plr_mon_race_is_("c.blue") && new_level >= 21)
        plr_mon_race_evolve("c.red");
    if (plr_mon_race_is_("c.red") && new_level >= 28)
        plr_mon_race_evolve("c.clear");
    if (plr_mon_race_is_("c.clear") && new_level >= 35 && player_is_monster_king())
        plr_mon_race_evolve("c.Multi-hued");
}

/**********************************************************************
 * Attacks
 **********************************************************************/
static void _calc_innate_bonuses(mon_blow_ptr blow)
{
    plr->innate_attack_info.blows_calc.wgt = 100;
    plr->innate_attack_info.blows_calc.mul = 35;
    plr->innate_attack_info.blows_calc.max = 100;
    if (blow->method != RBM_CRAWL) return;
    plr->innate_attack_info.blows_calc.max += 50*_rank();
    plr_calc_blows_innate(blow);
}
static void _calc_innate_attacks(void)
{
    mon_blow_ptr blow;
    int r = _rank(), x = 0;

    /* Bite */
    blow = mon_blow_alloc(RBM_BITE);
    blow->power = plr->lev;
    mon_blow_push_effect(blow, RBE_HURT, dice_create(1, 3+r, 2*r));
    vec_add(plr->innate_blows, blow);

    /* Sting */
    blow = mon_blow_alloc(RBM_STING);
    blow->power = plr->lev;
    mon_blow_push_effect(blow, RBE_HURT, dice_create(1, 2+r, 2*r));
    if (r >= 5)
        mon_blow_push_effect(blow, GF_POIS, dice_create(1, 2+4, 0));
    vec_add(plr->innate_blows, blow);

    /* Crawl */
    blow = mon_blow_alloc(RBM_CRAWL);
    blow->power = plr->lev;
    if (r >= 5) x = 7;
    mon_blow_push_effect(blow, RBE_HURT, dice_create(1, 3+r+x, 2*r));
    _calc_innate_bonuses(blow);
    vec_add(plr->innate_blows, blow);
}

/**********************************************************************
 * Bonuses
 **********************************************************************/
static void _calc_bonuses(void)
{
    int r = _rank();
    plr->to_a += 2*r;
    plr->dis_to_a += 2*r;

    plr->skill_dig += 50;
    plr->pspeed += _rank();
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (_rank())
        add_flag(flgs, OF_SPEED);
}
/**********************************************************************
 * Public Methods
 **********************************************************************/
plr_race_ptr mon_centipede_get_race(void)
{
    static plr_race_ptr me = NULL;
    int           r = _rank();
    static cptr   titles[6] =  {"Giant white centipede", "Metallic green centipede",
                                "Metallic blue centipede", "Metallic red centipede",
                                "Giant clear centipede", "The Multi-hued Centipede"};

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  21,  35,   5,  10,   7,  50,  10};
    skills_t xs = { 60,  50,  50,   0,   0,   0,  75,  10};

        me = plr_race_alloc(RACE_MON_CENTIPEDE);
        me->skills = bs;
        me->extra_skills = xs;

        me->name = "Centipede";
        me->desc = _desc;

        me->infra = 5;
        me->exp = 100;
        me->base_hp = 15;
        me->shop_adjust = 120;

        me->hooks.calc_innate_attacks = _calc_innate_attacks;
        me->hooks.calc_innate_bonuses = _calc_innate_bonuses;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.gain_level = _gain_level;
        me->hooks.birth = _birth;

        me->flags = RACE_IS_MONSTER;
        me->boss_r_idx = mon_race_parse("c.Multi-hued")->id;
        me->pseudo_class_id = CLASS_WARRIOR;
    }

    if (!birth_hack && !spoiler_hack)
        me->subname = titles[r];
    else
        me->subname = NULL;
    me->stats[A_STR] = (r + 1)/3;
    me->stats[A_INT] = -3;
    me->stats[A_WIS] = -3;
    me->stats[A_DEX] = r;
    me->stats[A_CON] = (r + 1)/2;
    me->stats[A_CHR] = -3;
    me->life = 90 + 2*r;
    me->equip_template = plr_equip_template();

    return me;
}

