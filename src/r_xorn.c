#include "angband.h"

static void _birth(void) 
{ 
    object_type    forge;

    p_ptr->current_r_idx = MON_UMBER_HULK;
    skills_innate_init("Gaze", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);

    object_prep(&forge, lookup_kind(TV_SWORD, SV_LONG_SWORD));
    py_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_d = 3;
    py_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_BOOTS, SV_PAIR_OF_METAL_SHOD_BOOTS));
    py_birth_obj(&forge);

    equip_on_change_race();

    py_birth_food();
    py_birth_light();
}

static void _calc_innate_attacks(void)
{
    if (p_ptr->lev < 20 && !p_ptr->blind) /* Umber Hulk only ... */
    {
        innate_attack_t    a = {0};

        a.flags |= INNATE_NO_DAM;
        a.effect[0] = GF_OLD_CONF;
        a.blows = 100;
        a.to_h = p_ptr->lev/5;
        a.msg = "You gaze.";
        a.name = "Gaze";

        p_ptr->innate_attacks[p_ptr->innate_attack_ct++] = a;
    }
}

static void _calc_bonuses(void) {
    int to_a = py_prorata_level(75);
    int ac = (p_ptr->lev < 20) ? 5 : 10;

    p_ptr->ac += ac;
    p_ptr->dis_ac += ac;

    p_ptr->to_a += to_a;
    p_ptr->dis_to_a += to_a;

    p_ptr->skill_dig += 500;

    p_ptr->free_act++;
    res_add(RES_POIS);
    res_add(RES_CONF);
    p_ptr->sustain_str = TRUE;
    if (p_ptr->lev < 20)
        p_ptr->kill_wall = TRUE;

    if (p_ptr->lev >= 20)
    {
        res_add(RES_COLD);
        res_add(RES_ELEC);
        res_add(RES_FIRE);
        p_ptr->pass_wall = TRUE;
        p_ptr->no_passwall_dam = TRUE;
    }
    if (p_ptr->lev >= 35)
    {
        p_ptr->pspeed += 2 + (p_ptr->lev - 35)/5;
    }
}
static void _get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_FREE_ACT);
    add_flag(flgs, OF_SUST_STR);
    add_flag(flgs, OF_RES_POIS);
    add_flag(flgs, OF_RES_CONF);

    if (p_ptr->lev >= 20)
    {
        add_flag(flgs, OF_RES_COLD);
        add_flag(flgs, OF_RES_ELEC);
        add_flag(flgs, OF_RES_FIRE);
    }
    if (p_ptr->lev >= 35)
    {
        add_flag(flgs, OF_SPEED);
    }
}
static void _gain_level(int new_level) {
    if (p_ptr->current_r_idx == MON_UMBER_HULK && new_level >= 20)
    {
        p_ptr->current_r_idx = MON_XORN;
        equip_on_change_race();
        msg_print("You have evolved into a Xorn.");
        p_ptr->redraw |= PR_MAP;
    }
    if (p_ptr->current_r_idx == MON_XORN && new_level >= 35)
    {
        p_ptr->current_r_idx = MON_XAREN;
        msg_print("You have evolved into a Xaren.");
        p_ptr->redraw |= PR_MAP;
    }
}
race_t *mon_xorn_get_race(void)
{
    static race_t me = {0};
    static bool   init = FALSE;
    static cptr   titles[3] =  {"Umber Hulk", "Xorn", "Xaren"};    
    int           rank = 0;

    if (p_ptr->lev >= 20) rank++;
    if (p_ptr->lev >= 35) rank++;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  20,  31,   2,  14,   5,  51,  30};
    skills_t xs = { 12,   8,  10,   0,   0,   0,  21,   7};

        me.skills = bs;
        me.extra_skills = xs;

        me.name = "Xorn";
        me.desc = "Xorns are massive creatures of earth. They begin life as an Umber Hulk, a bizarre "
                    "creature with large mandibles capable of slicing through rock and glaring eyes capable of confusing foes. "
                    "At this stage in their evolution their body is vaguely humanoid, allowing them to "
                    "wear a helmet, an amulet, a cloak and even a pair of boots; once the Umber Hulk evolves further, however, "
                    "it can no longer wear these items. Instead, a grown Xorn can use its four massive arms for weapons, shields, rings "
                    "and gloves; mature Xorns can also pass effortlessly through rock.\n \n"
                    "Xorns have no active magical powers, but instead rely on their ability to hide in rocks and the advantage that their "
                    "extra arms provide them in melee. Xorns, like most warrior characters, are very effective at killing monsters "
                    "but have problems with advanced magic devices.";

        me.infra = 5;
        me.exp = 150;
        me.base_hp = 25;
        me.shop_adjust = 120;

        me.calc_innate_attacks = _calc_innate_attacks;
        me.calc_bonuses = _calc_bonuses;
        me.get_flags = _get_flags;
        me.gain_level = _gain_level;
        me.birth = _birth;

        me.flags = RACE_IS_MONSTER;
        me.pseudo_class_idx = CLASS_WARRIOR;

        init = TRUE;
    }

    me.subname = titles[rank];
    me.stats[A_STR] =  2 + rank;
    me.stats[A_INT] = -4;
    me.stats[A_WIS] = -2;
    me.stats[A_DEX] = -3 + rank;
    me.stats[A_CON] =  rank;
    me.stats[A_CHR] = -1;
    me.life = (rank > 0) ? (96 + 4 * rank) : 92;

    me.equip_template = mon_get_equip_template();

    if (birth_hack || spoiler_hack)
    {
        me.subname = NULL;
        me.subdesc = NULL;
    }
    return &me;
}
