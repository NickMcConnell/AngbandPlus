#include "angband.h"

#include <assert.h>

/****************************************************************
 * Combat
 ****************************************************************/
static personality_ptr _get_combat_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Combat";
        me.desc = "The Combat personality favors melee and shooting at the expense "
                    "of other skills such as disarming, devices, and saving throws. "
                    "Strength and agility are enhanced, but intelligence and wisdom "
                    "are decreased. All Combat people have great respect "
                    "for the legendary \"Combat Echizen\".\n(See \"Death Crimson\" / Ecole Software Corp.)";

        me.stats[A_STR] =  1;
        me.stats[A_INT] = -1;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] =  0;
        me.stats[A_CHR] =  0;

        me.skills.dis = -2;
        me.skills.dev = -3;
        me.skills.sav = -3;
        me.skills.stl =  0;
        me.skills.srh = -1;
        me.skills.fos =  2;
        me.skills.thn =  5;
        me.skills.thb =  3;

        me.life = 100;
        me.exp = 100;

        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Craven
 ****************************************************************/
static personality_ptr _get_craven_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Craven";
        me.desc = "A Craven person is a coward, preferring to avoid a fight at "
                    "any cost. Craven adventurers shoot and use devices well, "
                    "and their stealth is impressive. But their stats and other "
                    "skills are somewhat wanting.";

        me.stats[A_STR] = -1;
        me.stats[A_INT] =  1;
        me.stats[A_WIS] =  1;
        me.stats[A_DEX] = -1;
        me.stats[A_CON] = -1;
        me.stats[A_CHR] = -3;

        me.skills.dis =  5;
        me.skills.dev =  6;
        me.skills.sav = -1;
        me.skills.stl =  3;
        me.skills.srh =  0;
        me.skills.fos =  0;
        me.skills.thn =-10;
        me.skills.thb =  7;

        me.life = 99;
        me.exp = 100;

        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Fearless
 ****************************************************************/
static void _fearless_calc_bonuses(void)
{
    res_add(RES_FEAR);
}
static void _fearless_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_FEAR);
}
static personality_ptr _get_fearless_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Fearless";
        me.desc = "Fearless raises your melee skills and force of personality. "
                    "Stats such as magic defense and constitution are reduced. "
                    "Also it has a direct bad influence on your hit-points.";

        me.stats[A_STR] =  1;
        me.stats[A_INT] =  0;
        me.stats[A_WIS] =  0;
        me.stats[A_DEX] = -1;
        me.stats[A_CON] = -1;
        me.stats[A_CHR] =  1;

        me.skills.dis = -5;
        me.skills.dev =  2;
        me.skills.sav = -2;
        me.skills.stl =  0;
        me.skills.srh =  2;
        me.skills.fos = -2;
        me.skills.thn =  5;
        me.skills.thb =  0;

        me.life = 98;
        me.exp = 100;

        me.calc_bonuses = _fearless_calc_bonuses;
        me.get_flags = _fearless_get_flags;

        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Hasty
 ****************************************************************/
static void _hasty_calc_bonuses(void)
{
    p_ptr->pspeed += 2;
    p_ptr->to_m_chance += 1;
}
static void _hasty_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SPEED);
}

static personality_ptr _get_hasty_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Hasty";
        me.desc = "A Hasty person edeavors to do all things quickly. Speed, "
                    "rather than skill and patience, are paramount, and the Hasty "
                    "adventurer moves quickly through the dungeon, bungling much.";

        me.stats[A_STR] =  0;
        me.stats[A_INT] = -1;
        me.stats[A_WIS] = -2;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] = -1;
        me.stats[A_CHR] =  0;

        me.skills.dis = -5;
        me.skills.dev = -5;
        me.skills.sav = -3;
        me.skills.stl = -1;
        me.skills.srh = -4;
        me.skills.fos = -2;
        me.skills.thn = -5;
        me.skills.thb = -3;

        me.life = 100;
        me.exp = 100;

        me.calc_bonuses = _hasty_calc_bonuses;
        me.get_flags = _hasty_get_flags;

        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Lazy
 ****************************************************************/
static void _lazy_birth(void)
{
    p_ptr->au /= 2;
}
static void _lazy_calc_bonuses(void)
{
    p_ptr->to_m_chance += 10;
}
static personality_ptr _get_lazy_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Lazy";
        me.desc = "A Lazy person has no good stats and can do no action well. "
                    "Also it has a direct bad influence on your spell fail rate.";

        me.stats[A_STR] = -2;
        me.stats[A_INT] = -2;
        me.stats[A_WIS] = -2;
        me.stats[A_DEX] = -2;
        me.stats[A_CON] = -2;
        me.stats[A_CHR] = -2;

        me.skills.dis = -5;
        me.skills.dev = -5;
        me.skills.sav = -3;
        me.skills.stl = -1;
        me.skills.srh = -4;
        me.skills.fos = -2;
        me.skills.thn = -8;
        me.skills.thb = -5;

        me.life = 95;
        me.exp = 100;

        me.birth = _lazy_birth;
        me.calc_bonuses = _lazy_calc_bonuses;

        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Lucky
 ****************************************************************/
static void _lucky_birth(void)
{
    mut_gain(MUT_GOOD_LUCK);
    mut_lock(MUT_GOOD_LUCK);
}
static personality_ptr _get_lucky_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Lucky";
        me.desc = "A Lucky person has poor stats but, surprisingly, can do all "
                    "things well. For some reason, good things seem to happen "
                    "more often to lucky players.";

        me.stats[A_STR] = -2;
        me.stats[A_INT] = -2;
        me.stats[A_WIS] = -2;
        me.stats[A_DEX] = -2;
        me.stats[A_CON] = -2;
        me.stats[A_CHR] = -2;

        me.skills.dis = 10;
        me.skills.dev =  7;
        me.skills.sav =  3;
        me.skills.stl =  2;
        me.skills.srh = 10;
        me.skills.fos =  8;
        me.skills.thn = 15;
        me.skills.thb =  9;

        me.life = 98;
        me.exp = 100;

        me.birth = _lucky_birth;

        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Mighty
 ****************************************************************/
static void _mighty_calc_bonuses(void)
{
    p_ptr->to_m_chance += 1;
}
static personality_ptr _get_mighty_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Mighty";
        me.desc = "Mighty raises your physical stats and skills, but reduces stats "
                    "and skills which influence magic. It makes your stats suitable "
                    "for a warrior. Also it directly influences your hit-points "
                    "and spell fail rate.";

        me.stats[A_STR] =  2;
        me.stats[A_INT] = -2;
        me.stats[A_WIS] = -2;
        me.stats[A_DEX] =  0;
        me.stats[A_CON] =  1;
        me.stats[A_CHR] =  1;

        me.skills.dis = -5;
        me.skills.dev = -5;
        me.skills.sav = -3;
        me.skills.stl = -1;
        me.skills.srh = -2;
        me.skills.fos = -2;
        me.skills.thn = 15;
        me.skills.thb =  0;

        me.life = 102;
        me.exp = 100;

        me.calc_bonuses = _mighty_calc_bonuses;

        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Munchkin
 ****************************************************************/
static void _munchkin_birth(void)
{
    p_ptr->au = 10 * 1000 * 1000;
}

static void _munchkin_calc_bonuses(void)
{
    p_ptr->auto_id = TRUE;
    res_add(RES_BLIND);
    res_add(RES_CONF);
    p_ptr->hold_life = TRUE;
    if (p_ptr->pclass != CLASS_NINJA)
        p_ptr->lite = TRUE;

    p_ptr->pspeed += p_ptr->lev/10 + 5;
}
static void _munchkin_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_BLIND);
    add_flag(flgs, OF_RES_CONF);
    add_flag(flgs, OF_HOLD_LIFE);
    if (p_ptr->pclass != CLASS_NINJA)
        add_flag(flgs, OF_LITE);
    add_flag(flgs, OF_SPEED);
}
static personality_ptr _get_munchkin_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Munchkin";
        me.desc = "The Munchkin personality is for beginners. It raises all your "
                    "stats and skills. With this personality, you can win the game "
                    "easily, but gain little honor in doing so.";

        me.stats[A_STR] = 3;
        me.stats[A_INT] = 3;
        me.stats[A_WIS] = 3;
        me.stats[A_DEX] = 3;
        me.stats[A_CON] = 3;
        me.stats[A_CHR] = 3;

        me.skills.dis = 10;
        me.skills.dev = 20;
        me.skills.sav = 15;
        me.skills.stl =  5;
        me.skills.srh = 20;
        me.skills.fos = 20;
        me.skills.thn = 40;
        me.skills.thb = 24;

        me.life = 150;
        me.exp = 50;
        me.flags = DEPRECATED;

        me.birth = _munchkin_birth;
        me.calc_bonuses = _munchkin_calc_bonuses;
        me.get_flags = _munchkin_get_flags;

        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Nimble
 ****************************************************************/
static personality_ptr _get_nimble_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Nimble";
        me.desc = "Nimble renders you highly skilled comparatively well, but "
                    "reduces your physical ability.";

        me.stats[A_STR] = -1;
        me.stats[A_INT] =  1;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] = -1;
        me.stats[A_CHR] =  0;

        me.skills.dis =  7;
        me.skills.dev =  3;
        me.skills.sav = -1;
        me.skills.stl =  1;
        me.skills.srh =  5;
        me.skills.fos =  5;
        me.skills.thn =  0;
        me.skills.thb =  7;

        me.life = 99;
        me.exp = 100;

        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Ordinary
 ****************************************************************/
static personality_ptr _get_odinary_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Ordinary";
        me.desc = "An ordinary person is average in every respect, gaining neither "
                    "skills, talents nor stat adjustments.";
        me.life = 100;
        me.exp = 100;

        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Patient
 ****************************************************************/
static void _patient_calc_bonuses(void)
{
    p_ptr->to_m_chance += 1;
}
static personality_ptr _get_patient_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Patient";
        me.desc = "A Patient person does things carefully. Patient people have "
                    "high constitution, and high resilience, but poor abilities "
                    "in most other skills. Also it directly influences your "
                    "life rating.";

        me.stats[A_STR] = -1;
        me.stats[A_INT] = -1;
        me.stats[A_WIS] =  1;
        me.stats[A_DEX] = -2;
        me.stats[A_CON] =  2;
        me.stats[A_CHR] =  0;

        me.skills.dis = -5;
        me.skills.dev = -3;
        me.skills.sav =  3;
        me.skills.stl =  1;
        me.skills.srh =  0;
        me.skills.fos = -3;
        me.skills.thn = -6;
        me.skills.thb = -3;

        me.life = 102;
        me.exp = 100;

        me.calc_bonuses = _patient_calc_bonuses;

        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Pious
 ****************************************************************/
static personality_ptr _get_pious_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Pious";
        me.desc = "Pious deepens your faith in your God. It makes your physical "
                    "ability average, and your stats suitable for priest.";

        me.stats[A_STR] =  0;
        me.stats[A_INT] = -1;
        me.stats[A_WIS] =  2;
        me.stats[A_DEX] = -1;
        me.stats[A_CON] =  0;
        me.stats[A_CHR] =  0;

        me.skills.dis = -5;
        me.skills.dev =  2;
        me.skills.sav =  4;
        me.skills.stl = -1;
        me.skills.srh =  3;
        me.skills.fos = -2;
        me.skills.thn = -3;
        me.skills.thb = -3;

        me.life = 100;
        me.exp = 100;

        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Sexy
 ****************************************************************/
static void _sexy_birth(void)
{
    if ( p_ptr->prace != RACE_MON_SWORD
      && !demon_is_(DEMON_BALROG) )
    {
        object_type forge = {0};
        object_prep(&forge, lookup_kind(TV_HAFTED, SV_WHIP));
        if (p_ptr->pclass == CLASS_RUNE_KNIGHT)
            rune_add(&forge, RUNE_ABSORPTION, FALSE);
        py_birth_obj(&forge);
    }
}
static void _sexy_calc_bonuses(void)
{
    p_ptr->cursed |= OFC_AGGRAVATE;
}
static void _sexy_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_AGGRAVATE);
}
static personality_ptr _get_sexy_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Sexy";
        me.desc = "Sexy increases all of your stats and skills, but your haughty "
                    "attitude will aggravate all monsters.";

        me.stats[A_STR] = 1;
        me.stats[A_INT] = 1;
        me.stats[A_WIS] = 1;
        me.stats[A_DEX] = 1;
        me.stats[A_CON] = 1;
        me.stats[A_CHR] = 1;

        me.skills.dis = 10;
        me.skills.dev =  5;
        me.skills.sav =  3;
        me.skills.stl =  0;
        me.skills.srh =  4;
        me.skills.fos =  2;
        me.skills.thn = 10;
        me.skills.thb =  7;

        me.life = 100;
        me.exp = 100;

        me.calc_bonuses = _sexy_calc_bonuses;
        me.get_flags = _sexy_get_flags;
        me.birth = _sexy_birth;

        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Shrewd
 ****************************************************************/
static void _shrewd_calc_bonuses(void)
{
    p_ptr->to_m_chance -= 3;
}
static personality_ptr _get_shrewd_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Shrewd";
        me.desc = "Shrewd reduces your physical stats, and raises your intelligence "
                    "and magical skills. It makes your stats suitable for a mage. "
                    "Also it directly influences your hit-points and spell fail rate.";

        me.stats[A_STR] = -2;
        me.stats[A_INT] =  2;
        me.stats[A_WIS] =  0;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] = -1;
        me.stats[A_CHR] = -1;

        me.skills.dis =  3;
        me.skills.dev =  8;
        me.skills.sav =  2;
        me.skills.stl =  0;
        me.skills.srh = -2;
        me.skills.fos =  5;
        me.skills.thn = -8;
        me.skills.thb = -3;

        me.life = 97;
        me.exp = 100;

        me.calc_bonuses = _shrewd_calc_bonuses;

        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Public Interface
 ****************************************************************/
personality_ptr get_personality_aux(int index)
{
    personality_ptr result = NULL;
    switch (index)
    {
    case PERS_COMBAT:
        result = _get_combat_personality();
        break;
    case PERS_CRAVEN:
        result = _get_craven_personality();
        break;
    case PERS_FEARLESS:
        result = _get_fearless_personality();
        break;
    case PERS_HASTY:
        result = _get_hasty_personality();
        break;
    case PERS_LAZY:
        result = _get_lazy_personality();
        break;
    case PERS_LUCKY:
        result = _get_lucky_personality();
        break;
    case PERS_MIGHTY:
        result = _get_mighty_personality();
        break;
    case PERS_MUNCHKIN:
        result = _get_munchkin_personality();
        break;
    case PERS_NIMBLE:
        result = _get_nimble_personality();
        break;
    case PERS_ORDINARY:
        result = _get_odinary_personality();
        break;
    case PERS_PATIENT:
        result = _get_patient_personality();
        break;
    case PERS_PIOUS:
        result = _get_pious_personality();
        break;
    case PERS_SEXY:
        result = _get_sexy_personality();
        break;
    case PERS_SHREWD:
        result = _get_shrewd_personality();
        break;
    }
    assert(result);
    result->id = index;
    return result;
}

personality_ptr get_personality(void)
{
    return get_personality_aux(p_ptr->personality);
}

