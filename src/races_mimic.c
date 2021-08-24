#include "angband.h"

/****************************************************************
 * Clay-Golem
 ****************************************************************/
static void _clay_golem_calc_bonuses(void)
{
    plr->free_act++;
    plr->hold_life++;
    plr->to_a += 10;
    plr->dis_to_a += 10;
}
static void _clay_golem_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_FREE_ACT);
    add_flag(flgs, OF_HOLD_LIFE);
}
plr_race_ptr clay_golem_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(MIMIC_CLAY_GOLEM);
        me->name = "Clay-Golem";
        me->desc = "";

        me->stats[A_STR] =  2;
        me->stats[A_INT] =  0;
        me->stats[A_WIS] =  0;
        me->stats[A_DEX] =  0;
        me->stats[A_CON] =  2;
        me->stats[A_CHR] = -2;
        
        me->skills.dis = -5;
        me->skills.dev = -5;
        me->skills.sav = 8;
        me->skills.stl = -2;
        me->skills.srh = -2;
        me->skills.fos = 5;
        me->skills.thn = 20;
        me->skills.thb = 0;

        me->life = 102;
        me->base_hp = 22;
        me->exp = 200;
        me->infra = 2;

        me->flags = RACE_IS_NONLIVING;

        me->hooks.calc_bonuses = _clay_golem_calc_bonuses;
        me->hooks.get_flags = _clay_golem_get_flags;
    }

    return me;
}

/****************************************************************
 * Colossus
 ****************************************************************/
static void _colossus_calc_bonuses(void)
{
    plr->free_act++;
    plr->see_inv++;
    plr->hold_life++;
    res_add(GF_POIS);
    res_add(GF_SHARDS);
    res_add(GF_SOUND);
    res_add(GF_DISEN);
    plr->reflect = TRUE;
    plr->pspeed -= 5;
    plr->to_a += 40;
    plr->dis_to_a += 40;
}
static void _colossus_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_FREE_ACT);
    add_flag(flgs, OF_SEE_INVIS);
    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_DEC_SPEED);
    add_flag(flgs, OF_RES_(GF_SHARDS));
    add_flag(flgs, OF_REFLECT);
    add_flag(flgs, OF_RES_(GF_SOUND));
    add_flag(flgs, OF_RES_(GF_DISEN));
}
plr_race_ptr colossus_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(MIMIC_COLOSSUS);
        me->name = "Colossus";
        me->desc = "";

        me->stats[A_STR] =  7;
        me->stats[A_INT] =  2;
        me->stats[A_WIS] =  2;
        me->stats[A_DEX] = -4;
        me->stats[A_CON] =  7;
        me->stats[A_CHR] =  4;
        
        me->skills.dis =  0;
        me->skills.dev =  0;
        me->skills.sav = 35;
        me->skills.stl = -4;
        me->skills.srh = -2;
        me->skills.fos = 5;
        me->skills.thn = 90;
        me->skills.thb = -12;

        me->life = 115;
        me->base_hp = 30;
        me->exp = 1000;
        me->infra = 5;
        me->flags = RACE_IS_NONLIVING;

        me->hooks.calc_bonuses = _colossus_calc_bonuses;
        me->hooks.get_flags = _colossus_get_flags;
    }

    return me;
}

/****************************************************************
 * Demon (cf Polymorph Demon)
 ****************************************************************/
static power_info _demon_powers[] =
{
    { A_CON, {15, 10, 70, demon_breath_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _demon_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _demon_powers);
}
static void _demon_calc_bonuses(void)
{
    plr->hold_life++;
    res_add(GF_CHAOS);
    res_add(GF_NETHER);
    res_add(GF_FIRE);
    res_add(GF_FIRE);
    plr->see_inv++;
    plr->pspeed += 3;
    plr->to_a += 10;
    plr->dis_to_a += 10;
    plr->align -= 200;
}
static void _demon_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_RES_(GF_CHAOS));
    add_flag(flgs, OF_RES_(GF_NETHER));
    add_flag(flgs, OF_RES_(GF_FIRE));
    add_flag(flgs, OF_SEE_INVIS);
    add_flag(flgs, OF_SPEED);
}
plr_race_ptr demon_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(MIMIC_DEMON);
        me->name = "Demon";
        me->desc = "";

        me->stats[A_STR] =  5;
        me->stats[A_INT] =  3;
        me->stats[A_WIS] =  2;
        me->stats[A_DEX] =  3;
        me->stats[A_CON] =  4;
        me->stats[A_CHR] =  3;
        
        me->skills.dis = -5;
        me->skills.dev = 18;
        me->skills.sav = 20;
        me->skills.stl = -2;
        me->skills.srh =  3;
        me->skills.fos = 10;
        me->skills.thn = 40;
        me->skills.thb = 10;

        me->life = 106;
        me->base_hp = 24;
        me->exp = 500;
        me->infra = 5;
        me->flags = RACE_IS_NONLIVING | RACE_IS_DEMON;

        me->hooks.calc_bonuses = _demon_calc_bonuses;
        me->hooks.get_powers = _demon_get_powers;
        me->hooks.get_flags = _demon_get_flags;
    }

    return me;
}

/****************************************************************
 * Demon-Lord (cf Polymorph Demon-Lord)
 ****************************************************************/
static power_info _demon_lord_powers[] =
{
    { A_CON, {15, 10, 70, demon_breath_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _demon_lord_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _demon_lord_powers);
}
static void _demon_lord_calc_bonuses(void)
{
    plr->hold_life++;
    res_add(GF_CHAOS);
    res_add(GF_NETHER);
    res_add_immune(GF_FIRE);
    res_add(GF_ACID);
    res_add(GF_COLD);
    res_add(GF_ELEC);
    res_add(GF_POIS);
    res_add(GF_CONF);
    res_add(GF_DISEN);
    res_add(GF_NEXUS);
    res_add(GF_FEAR);
    plr->sh_fire = TRUE;
    plr->see_inv++;
    plr->telepathy = TRUE;
    plr->levitation = TRUE;
    plr->kill_wall = TRUE;
    plr->pspeed += 5;
    plr->to_a += 20;
    plr->dis_to_a += 20;
    plr->align -= 200;
}
static void _demon_lord_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_RES_(GF_CHAOS));
    add_flag(flgs, OF_RES_(GF_NETHER));
    add_flag(flgs, OF_RES_(GF_FIRE));
    add_flag(flgs, OF_RES_(GF_COLD));
    add_flag(flgs, OF_RES_(GF_ELEC));
    add_flag(flgs, OF_RES_(GF_ACID));
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_RES_(GF_CONF));
    add_flag(flgs, OF_RES_(GF_DISEN));
    add_flag(flgs, OF_RES_(GF_NEXUS));
    add_flag(flgs, OF_RES_(GF_FEAR));
    add_flag(flgs, OF_IM_(GF_FIRE));
    add_flag(flgs, OF_AURA_FIRE);
    add_flag(flgs, OF_SEE_INVIS);
    add_flag(flgs, OF_TELEPATHY);
    add_flag(flgs, OF_LEVITATION);
    add_flag(flgs, OF_SPEED);

    add_flag(flgs, OF_IM_(GF_FIRE));
}
plr_race_ptr demon_lord_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(MIMIC_DEMON_LORD);
        me->name = "Demon-Lord";
        me->desc = "";

        me->stats[A_STR] = 10;
        me->stats[A_INT] = 10;
        me->stats[A_WIS] = 10;
        me->stats[A_DEX] = 10;
        me->stats[A_CON] = 10;
        me->stats[A_CHR] = 10;
        
        me->skills.dis = 20;
        me->skills.dev = 20;
        me->skills.sav = 25;
        me->skills.stl = -2;
        me->skills.srh =  3;
        me->skills.fos = 10;
        me->skills.thn = 70;
        me->skills.thb = 15;

        me->life = 110;
        me->base_hp = 28;
        me->exp = 1500;
        me->infra = 20;
        me->flags = RACE_IS_NONLIVING | RACE_IS_DEMON;

        me->hooks.calc_bonuses = _demon_lord_calc_bonuses;
        me->hooks.get_powers = _demon_lord_get_powers;
        me->hooks.get_flags = _demon_lord_get_flags;
    }

    return me;
}

/****************************************************************
 * Iron-Golem
 ****************************************************************/
static void _iron_golem_calc_bonuses(void)
{
    plr->free_act++;
    plr->see_inv++;
    plr->hold_life++;
    res_add(GF_POIS);
    plr->pspeed -= 1;
    plr->to_a += 15;
    plr->dis_to_a += 15;
}
static void _iron_golem_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_FREE_ACT);
    add_flag(flgs, OF_SEE_INVIS);
    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_DEC_SPEED);
}
plr_race_ptr iron_golem_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(MIMIC_IRON_GOLEM);
        me->name = "Iron-Golem";
        me->desc = "";

        me->stats[A_STR] =  3;
        me->stats[A_INT] =  0;
        me->stats[A_WIS] =  0;
        me->stats[A_DEX] = -1;
        me->stats[A_CON] =  3;
        me->stats[A_CHR] = -2;
        
        me->skills.dis = -5;
        me->skills.dev = -5;
        me->skills.sav = 15;
        me->skills.stl = -2;
        me->skills.srh = -2;
        me->skills.fos = 5;
        me->skills.thn = 30;
        me->skills.thb = -5;

        me->life = 105;
        me->base_hp = 24;
        me->exp = 250;
        me->infra = 3;
        me->flags = RACE_IS_NONLIVING;

        me->hooks.calc_bonuses = _iron_golem_calc_bonuses;
        me->hooks.get_flags = _iron_golem_get_flags;
    }

    return me;
}

/****************************************************************
 * Mangy Leper
 ****************************************************************/
plr_race_ptr mangy_leper_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(MIMIC_MANGY_LEPER);
        me->name = "Mangy-Leper";
        me->desc = "Mangy Lepers are humans who have contracted a horrible wasting disease. "
                    "You cannot help but feel disgusted as your body rots before your very "
                    "eyes.";
        
        me->stats[A_STR] = -1;
        me->stats[A_INT] =  0;
        me->stats[A_WIS] =  0;
        me->stats[A_DEX] = -1;
        me->stats[A_CON] = -3;
        me->stats[A_CHR] = -3;
        
        me->skills.dis = 0;
        me->skills.dev = 0;
        me->skills.sav = 0;
        me->skills.stl = 0;
        me->skills.srh = 0;
        me->skills.fos = 10;
        me->skills.thn = 0;
        me->skills.thb = 0;

        me->life = 88;
        me->base_hp = 10;
        me->exp = 100;
        me->infra = 0;
    }

    return me;
}

/****************************************************************
 * Mithril-Golem
 ****************************************************************/
static void _mithril_golem_calc_bonuses(void)
{
    plr->free_act++;
    plr->see_inv++;
    plr->hold_life++;
    res_add(GF_POIS);
    res_add(GF_SHARDS);
    plr->reflect = TRUE;
    plr->pspeed -= 2;
    plr->to_a += 20;
    plr->dis_to_a += 20;
}
static void _mithril_golem_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_FREE_ACT);
    add_flag(flgs, OF_SEE_INVIS);
    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_DEC_SPEED);
    add_flag(flgs, OF_RES_(GF_SHARDS));
    add_flag(flgs, OF_REFLECT);
}
plr_race_ptr mithril_golem_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(MIMIC_MITHRIL_GOLEM);
        me->name = "Mithril-Golem";
        me->desc = "";

        me->stats[A_STR] =  5;
        me->stats[A_INT] =  1;
        me->stats[A_WIS] =  1;
        me->stats[A_DEX] = -3;
        me->stats[A_CON] =  5;
        me->stats[A_CHR] =  2;
        
        me->skills.dis =  0;
        me->skills.dev =  0;
        me->skills.sav = 25;
        me->skills.stl = -3;
        me->skills.srh = -2;
        me->skills.fos = 5;
        me->skills.thn = 50;
        me->skills.thb = -7;

        me->life = 109;
        me->base_hp = 27;
        me->exp = 500;
        me->infra = 4;
        me->flags = RACE_IS_NONLIVING;

        me->hooks.calc_bonuses = _mithril_golem_calc_bonuses;
        me->hooks.get_flags = _mithril_golem_get_flags;
    }

    return me;
}

/****************************************************************
 * Small Kobold
 ****************************************************************/
static void _small_kobold_calc_bonuses(void)
{
    res_add(GF_POIS);
}
static void _small_kobold_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_POIS));
}
plr_race_ptr small_kobold_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(MIMIC_SMALL_KOBOLD);
        me->name = "Small-Kobold";
        me->desc = "Small Kobolds are a the runts of the kobold race, often relegated to the "
                    "performance of menial tasks deemed unworthy of their larger, more "
                    "respectable brethren.";

        me->stats[A_STR] =  0;
        me->stats[A_INT] = -2;
        me->stats[A_WIS] = -1;
        me->stats[A_DEX] =  0;
        me->stats[A_CON] = -1;
        me->stats[A_CHR] = -3;
        
        me->skills.dis = -5;
        me->skills.dev = -8;
        me->skills.sav = -3;
        me->skills.stl = -1;
        me->skills.srh =  1;
        me->skills.fos =  8;
        me->skills.thn =  5;
        me->skills.thb = -8;

        me->life = 91;
        me->base_hp = 13;
        me->exp = 50;
        me->infra = 2;

        me->hooks.calc_bonuses = _small_kobold_calc_bonuses;
        me->hooks.get_flags = _small_kobold_get_flags;
    }

    return me;
}

/****************************************************************
 * Vampire-Lord (cf Polymorph Vampire)
 ****************************************************************/
static power_info _vampire_lord_powers[] =
{
    { A_CON, {2, 1, 60, vampirism_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _vampire_lord_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _vampire_lord_powers);
}
static void _vampire_lord_calc_bonuses(void)
{
    res_add_immune(GF_DARK);
    plr->hold_life++;
    res_add(GF_NETHER);
    res_add(GF_COLD);
    res_add(GF_POIS);
    res_add_vuln(GF_LIGHT);
    plr->see_nocto = DUN_VIEW_MAX;

    plr->see_inv++;
    plr->pspeed += 3;
    plr->to_a += 10;
    plr->dis_to_a += 10;
}
static void _vampire_lord_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_VULN_(GF_LIGHT));
    add_flag(flgs, OF_IM_(GF_DARK));

    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_RES_(GF_NETHER));
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_RES_(GF_COLD));
    add_flag(flgs, OF_SEE_INVIS);
    add_flag(flgs, OF_SPEED);
}
plr_race_ptr vampire_lord_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(MIMIC_VAMPIRE);
        me->name = "Vampire-Lord";
        me->desc = "";

        me->stats[A_STR] =  4;
        me->stats[A_INT] =  4;
        me->stats[A_WIS] =  1;
        me->stats[A_DEX] =  1;
        me->stats[A_CON] =  2;
        me->stats[A_CHR] =  3;
        
        me->skills.dis = 6;
        me->skills.dev = 12;
        me->skills.sav = 8;
        me->skills.stl = 6;
        me->skills.srh = 2;
        me->skills.fos = 12;
        me->skills.thn = 30;
        me->skills.thb = 10;

        me->life = 103;
        me->base_hp = 22;
        me->exp = 300;
        me->infra = 5;
        me->flags = RACE_IS_NONLIVING | RACE_IS_UNDEAD;

        me->hooks.calc_bonuses = _vampire_lord_calc_bonuses;
        me->hooks.get_powers = _vampire_lord_get_powers;
        me->hooks.get_flags = _vampire_lord_get_flags;
    }

    return me;
}

