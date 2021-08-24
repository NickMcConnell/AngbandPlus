#include "angband.h"

#include <assert.h>

/****************************************************************
 * Chaotic
 ****************************************************************/
static const u32b _chaotic_seed_modifier[50] =
{
    684200517, /* 1 */
    225834954, /* 2 */
    3944202616, /* 3 */
    1943705505, /* 4 */
    138240376, /* 5 */
    3180517412, /* 6 */
    2871820153, /* 7 */
    605895243, /* 8 */ 
    597708903, /* 9 */
    3525434408, /* 10 */
    2948019236, /* 11 */
    2464136970, /* 12 */
    3773069027, /* 13 */
    466681736, /* 14 */
    1157404760, /* 15 */
    1930359308, /* 16 */
    1662126757, /* 17 */
    2730261851, /* 18 */
    2445628689, /* 19 */
    2557120090, /* 20 */
    1930283532, /* 21 */
    798086206, /* 22 */
    3794835879, /* 23 */
    3596314354, /* 24 */
    1482845135, /* 25 */
    3828841330, /* 26 */
    2739341129, /* 27 */
    2541552072, /* 28 */
    1901688662, /* 29 */
    2939571038, /* 30 */
    3237359151, /* 31 */
    1638698171, /* 32 */
    1525083078, /* 33 */
    4293987205, /* 34 */
    1979345876, /* 35 */
    4064464759, /* 36 */
    2079693315, /* 37 */
    127725156, /* 38 */
    3795227652, /* 39 */
    2179975309, /* 40 */
    1409032665, /* 41 */
    4228529657, /* 42 */
    3275436188, /* 43 */
    263708400, /* 44 */
    3237472283, /* 45 */
    1488557381, /* 46 */
    1536964663, /* 47 */
    2135793317, /* 48 */
    3698024470, /* 49 */
    4193053798 /* 50 */
};

static void _chaotic_birth(void)
{
    chaotic_py_seed = randint0(0x10000000);
    mut_gain(MUT_CHAOS_GIFT);
    mut_lock(MUT_CHAOS_GIFT);
}

static int _chaotic_calc_stats(int *modifiers, int level)
{
    u32b working_seed = (chaotic_py_seed ^ _chaotic_seed_modifier[level - 1]);
    int paikka = ((chaotic_py_seed + (level * 7)) % 50);
    int i, summa = 0;
    for (i = 0; i < MAX_STATS; i++)
    {
        int uusipaikka = ((i << 2) + paikka) % 24;
        int temp_seed = ((uusipaikka > 0) ? (working_seed >> uusipaikka) : working_seed) % 16;
        if (temp_seed < 4) { modifiers[i] = 2; summa += 2; continue; }
        else if (temp_seed < 8) { modifiers[i] = -2; summa -= 2; continue; }
        else if (temp_seed < 11) { modifiers[i] = 1; summa += 1; continue; }
        else if (temp_seed < 14) { modifiers[i] = -1; summa -= 1; continue; }
        else modifiers[i] = 0;
    }
    return summa;
}

static void _chaotic_calc_things(personality_ptr pers_ptr)
{
    int i, summa, stat_modifiers[MAX_STATS] = {0};
    if ((p_ptr->lev < 1) && (p_ptr->lev > 50)) return;   
    summa = _chaotic_calc_stats(stat_modifiers, p_ptr->lev);
    if (p_ptr->lev == 50) /* try to force balanced final stats to avoid huge life rating bonus/malus */
    {
        int koitto = 1;
        while (((summa * summa) > 5) && (koitto < 51))
        {
            summa = _chaotic_calc_stats(stat_modifiers, koitto);
            koitto++;
        }
    }
    for (i = 0; i < MAX_STATS; i++) 
    { 
        pers_ptr->stats[i] = stat_modifiers[i];
    }
    pers_ptr->life = 99 - summa;
}

static personality_ptr _get_chaotic_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Chaotic";
        me.desc = "Chaotic adventurers are servants of the Demon Lords of Chaos, and "
                    "often receive a reward - or punishment - from their patrons when "
                    "they gain a level. Their strengths and weaknesses are "
                    "unpredictable; even their stat bonuses are subject to change.";	

        me.life = 99;
        me.exp = 100;

        me.birth = _chaotic_birth;

        init = TRUE;
    }
    if (!spoiler_hack && !birth_hack) _chaotic_calc_things(&me);
    return &me;
}

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
        me.skills.dev = -2;
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
static void _craven_calc_bonuses(void)
{
    res_add_vuln(RES_FEAR);
}
static void _craven_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_VULN_FEAR);
}

static personality_ptr _get_craven_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Craven";
        me.desc = "A Craven person is a coward, preferring to avoid a fight at any "
                    "cost. Craven adventurers shoot and use devices well, their "
                    "stealth is impressive, and they can move fast when not fighting;"
                    "but their stats and other skills are somewhat wanting.";

        me.stats[A_STR] = -1;
        me.stats[A_INT] =  1;
        me.stats[A_WIS] =  1;
        me.stats[A_DEX] = -1;
        me.stats[A_CON] = -1;
        me.stats[A_CHR] = -2;

        me.skills.dis =  5;
        me.skills.dev =  5;
        me.skills.sav = -1;
        me.skills.stl =  3;
        me.skills.srh =  0;
        me.skills.fos =  0;
        me.skills.thn =-10;
        me.skills.thb =  7;

        me.life = 99;
        me.exp = 100;

        me.calc_bonuses = _craven_calc_bonuses;
        me.get_flags = _craven_get_flags;

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
        me.skills.dev =  1;
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
    p_ptr->pspeed += 2 + p_ptr->lev/37;
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
        me.desc = "A Hasty person endeavors to do all things quickly. Speed, "
                    "rather than skill and patience, are paramount, and the Hasty "
                    "adventurer moves quickly through the dungeon, bungling much.";

        me.stats[A_STR] =  0;
        me.stats[A_INT] = -1;
        me.stats[A_WIS] = -2;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] = -1;
        me.stats[A_CHR] =  0;

        me.skills.dis = -5;
        me.skills.dev = -4;
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
        me.skills.dev =  6;
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
        me.skills.dev = -4;
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
        me.desc = "The Munchkin personality is for players who love being overpowered. "
                    "It raises all your stats and skills; with this personality, you can "
                    "win the game easily, but gain no honor in doing so.";

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

        me.birth = _munchkin_birth;
        me.calc_bonuses = _munchkin_calc_bonuses;
        me.get_flags = _munchkin_get_flags;

        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Mundane
 ****************************************************************/

static void _mundane_calc_bonuses(void)
{
    p_ptr->anti_magic = TRUE;
}
static void _mundane_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_NO_MAGIC);
}
static personality_ptr _get_mundane_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Mundane";
        me.desc = "Mundane adventurers have few talents to speak of, and "
                    "are incapable of using spells and not very good with "
                    "magical devices; but their anti-magic allows them to "
                    "resist the effects of many dangerous magical attacks.";

        me.stats[A_STR] = -1;
        me.stats[A_INT] = -1;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] = -1;
        me.stats[A_CON] = -1;
        me.stats[A_CHR] = -1;
 
        me.skills.dev = -3;
        me.skills.sav = 10;

        me.life = 97;
        me.exp = 100;

        me.calc_bonuses = _mundane_calc_bonuses;
        me.get_flags = _mundane_get_flags;

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
        me.skills.dev =  2;
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
        me.skills.dev = -2;
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
        me.skills.dev =  1;
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
        skills_weapon_init(TV_HAFTED, SV_WHIP, WEAPON_EXP_BEGINNER);
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
 * Unlucky
 ****************************************************************/
static void _unlucky_birth(void)
{
    mut_gain(MUT_BAD_LUCK);
    mut_lock(MUT_BAD_LUCK);
}
static personality_ptr _get_unlucky_personality(void)
{
    static personality_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Unlucky";
        me.desc = "An Unlucky adventurer looks impressive on paper and "
                    "should, by rights, have an easy time - but somehow "
                    "their fights don't go quite so smoothly, and the items "
                    "they most need never seem to drop...";

        me.stats[A_STR] = 2;
        me.stats[A_INT] = 2;
        me.stats[A_WIS] = 2;
        me.stats[A_DEX] = 2;
        me.stats[A_CON] = 2;
        me.stats[A_CHR] = 2;

        me.skills.dis = -5;
        me.skills.dev =  3;
        me.skills.sav = -8;
        me.skills.stl =  0;
        me.skills.srh =  0;
        me.skills.fos =  4;
        me.skills.thn = -2;
        me.skills.thb = -2;

        me.life = 104;
        me.exp = 100;

        me.birth = _unlucky_birth;

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
    case PERS_CHAOTIC:
        result = _get_chaotic_personality();
        break;
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
    case PERS_MUNDANE:
        result = _get_mundane_personality();
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
    case PERS_UNLUCKY:
        result = _get_unlucky_personality();
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

