#include "angband.h"

static void _birth(void) 
{ 
    object_type    forge;

    plr_mon_race_set("X.umber hulk");
    skills_innate_init("Gaze", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);

    object_prep(&forge, lookup_kind(TV_SWORD, SV_LONG_SWORD));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_d = 5;
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_BOOTS, SV_PAIR_OF_METAL_SHOD_BOOTS));
    plr_birth_obj(&forge);

    equip_on_change_race();

    plr_birth_food();
    plr_birth_light();
}

static void _calc_innate_attacks(void)
{
    if (plr_mon_race_is_("X.umber hulk"))
    {
        mon_blow_ptr blow = mon_blow_alloc(RBM_GAZE);
        blow->power = 20;
        mon_blow_push_effect(blow, GF_OLD_CONF, dice_create(0, 0, 0));
        vec_add(plr->innate_blows, blow);
    }
}

static void _calc_bonuses(void) {
    int to_a = plr_prorata_level(75);
    int ac = 10;

    plr->ac += ac;
    plr->dis_ac += ac;

    plr->to_a += to_a;
    plr->dis_to_a += to_a;

    plr->skill_dig += 500;

    plr->free_act++;
    res_add(GF_POIS);
    res_add(GF_CONF);
    plr->sustain_str = TRUE;
    if (plr->lev < 20)
        plr->kill_wall = TRUE;

    if (plr->lev >= 20)
    {
        res_add(GF_COLD);
        res_add(GF_ELEC);
        res_add(GF_FIRE);
        plr->pass_wall = TRUE;
        plr->no_passwall_dam = TRUE;
    }
    if (plr->lev >= 35)
    {
        plr->pspeed += 2 + (plr->lev - 35)/5;
    }
}
static void _get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_FREE_ACT);
    add_flag(flgs, OF_SUST_STR);
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_RES_(GF_CONF));

    if (plr->lev >= 20)
    {
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_RES_(GF_ELEC));
        add_flag(flgs, OF_RES_(GF_FIRE));
    }
    if (plr->lev >= 35)
    {
        add_flag(flgs, OF_SPEED);
    }
}
static void _gain_level(int new_level) {
    if (plr_mon_race_is_("X.umber hulk") && new_level >= 20)
        plr_mon_race_evolve("X.xorn");
    if (plr_mon_race_is_("X.xorn") && new_level >= 35)
        plr_mon_race_evolve("X.xaren");
}
plr_race_ptr mon_xorn_get_race(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[3] =  {"Umber Hulk", "Xorn", "Xaren"};    
    int           rank = 0;

    if (plr->lev >= 20) rank++;
    if (plr->lev >= 35) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  20,  31,   2,  14,   5,  56,  30};
    skills_t xs = { 60,  40,  50,   0,   0,   0, 100,  35};

        me = plr_race_alloc(RACE_MON_XORN);
        me->skills = bs;
        me->extra_skills = xs;

        me->name = "Xorn";
        me->desc = "Xorn are huge creatures of the element earth. They begin life as an Umber Hulk which is a bizarre "
                    "creature with glaring eyes capable of confusing their foes, and large mandibles capable of slicing "
                    "through rock. At this stage in their evolution, their body is vaguely humanoid allowing them to "
                    "wear a helmet, an amulet, a cloak and even a pair of boots. However, once the Umber Hulk evolves "
                    "it can no longer wear these items. Instead, it can use its four massive arms for weapons, shields, rings "
                    "and gloves. At this stage, the Xorn can pass effortlessly through rock.\n \n"
                    "Xorns are monsters so cannot choose a normal class. They have no active powers but instead rely on "
                    "their ability to hide in rocks combined with their ability to attack with up to four weapons. They "
                    "play like warriors and are strong as such.";

        me->infra = 5;
        me->exp = 150;
        me->base_hp = 30;
        me->shop_adjust = 120;

        me->hooks.calc_innate_attacks = _calc_innate_attacks;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.gain_level = _gain_level;
        me->hooks.birth = _birth;

        me->flags = RACE_IS_MONSTER;
        me->pseudo_class_id = CLASS_WARRIOR;
    }

    me->subname = titles[rank];
    me->stats[A_STR] =  2 + rank;
    me->stats[A_INT] = -4;
    me->stats[A_WIS] = -2;
    me->stats[A_DEX] = -3 + rank;
    me->stats[A_CON] =  1 + rank;
    me->stats[A_CHR] = -1;
    me->life = 100 + 4*rank;

    me->equip_template = plr_equip_template();

    if (birth_hack || spoiler_hack)
    {
        me->subname = NULL;
        me->subdesc = NULL;
    }
    return me;
}
