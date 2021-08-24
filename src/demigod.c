#include "angband.h"

#include <assert.h>

/****************************************************************
 * Spells
 ****************************************************************/
static void _kiss_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Kiss");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to charm an adjacent monster.");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, plr->lev * 2);
        break;
    case SPELL_CAST: {
        mon_ptr mon = plr_target_adjacent_mon();
        mon_race_ptr race;
        char desc[MAX_NLEN_MON];

        var_set_bool(res, FALSE);
        if (!mon) break;

        race = mon->race;
        monster_desc(desc, mon, 0);
        plr_on_touch_mon(mon); /* a big wet one! */
        if (mon_race_is_unique(race) || mon_save_p(mon, A_CHR))
        {
            mon_tim_remove(mon, MT_SLEEP);
            if (mon_is_hostile(mon))
            {
                switch (randint1(10))
                {
                case 1:
                    msg_format("%^s says 'Impudent Strumpet!'", desc);
                    break;
                case 2:
                    msg_format("%^s says 'Ewwww! Gross!!'", desc);
                    break;
                case 3:
                    msg_format("%^s says 'You ain't my type!'", desc);
                    break;
                default:
                    msg_format("%^s resists your charms.", desc);
                }

                if (allow_ticked_off(race))
                    mon_anger(mon);
            }
            else
                msg_format("%^s ignores you.", desc);
        }
        else
        {
            if (mon_is_pet(mon))
                msg_format("%^s slobbers on you affectionately.", desc);
            else if (mon_is_friendly(mon))
            {
                set_pet(mon);
                msg_format("%^s is charmed!", desc);
            }
            else
            {
                set_friendly(mon);
                msg_format("%^s suddenly becomes friendly.", desc);
            }
        }
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _demeter_clw_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Cure Wounds");
        break;
    case SPELL_DESC:
        var_set_string(res, "Heals cut and HP a little.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_heal(plr->lev/12 + 1, 10, 0));
        break;
    case SPELL_CAST:
        hp_player(damroll(plr->lev/12 + 1, 10));
        plr_tim_subtract(T_CUT, 10);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _shine_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shine");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generates a large ball of sunlight.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, plr->lev * 3));
        break;
    case SPELL_CAST:
        plr_burst(3, GF_LIGHT, 3*plr->lev);
        var_set_bool(res, TRUE);
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, (plr->lev - 20)/2);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

/****************************************************************
 * Demigod
 ****************************************************************/
static void _gain_power(int which)
{
    if (plr->demigod_power[which] < 0)
    {
        int idx = mut_gain_choice(mut_demigod_pred);
        mut_lock(idx);
        plr->demigod_power[which] = idx;
    }
    else if (!mut_present(plr->demigod_power[which]))
    {
        mut_gain(plr->demigod_power[which]);
        mut_lock(plr->demigod_power[which]);
    }
}
static void _gain_level(int new_level)
{
    if (new_level >= 20)
        _gain_power(0);
    if (new_level >= 40)
        _gain_power(1);
}

void demigod_rechoose_powers(void)
{
    int i, idx;
    for (i = 0; i < MAX_DEMIGOD_POWERS; i++)
    {
        idx = plr->demigod_power[i];
        if (idx >= 0)
        {
            mut_unlock(idx);
            mut_lose(idx);
            plr->demigod_power[i] = -1;
        }
    }
    _gain_level(plr->lev);
}

static plr_race_ptr _race_alloc(int psubrace)
{
    plr_race_ptr r = plr_race_alloc_aux(RACE_DEMIGOD, psubrace);
    r->name = "Demigod";
    r->desc = "The term demigod is commonly used to describe mythological figures whose one "
                "parent was a god and whose other parent was human; as such, demigods are "
                "human-god hybrids and are quite powerful. Demigods receive special abilities "
                "depending on their parentage.";
    
    r->stats[A_STR] =  1;
    r->stats[A_INT] =  1;
    r->stats[A_WIS] =  1;
    r->stats[A_DEX] =  1;
    r->stats[A_CON] =  1;
    r->stats[A_CHR] =  1;
    
    r->skills.dis =  2;
    r->skills.dev =  2;
    r->skills.sav =  1;
    r->skills.stl = -1;
    r->skills.srh =  0;
    r->skills.fos =  7;
    r->skills.thn = 10;
    r->skills.thb =  3;

    r->life = 100;
    r->base_hp = 20;
    r->exp = 180;
    r->shop_adjust = 100;

    r->hooks.gain_level = _gain_level;
    return r;
}
/****************************************************************
 * Minor God
 * Some of the following descriptions were inspired by wikipedia
 ****************************************************************/
static plr_race_ptr _minor_race(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {
        me = _race_alloc(DEMIGOD_MINOR);
        me->subname = "Minor-God";
        me->subdesc = "Fathered by a minor god, you gain no starting powers.";
    }
    return me;
}
/****************************************************************
 * Aphrodite
 ****************************************************************/
static power_info _aphrodite_powers[] =
{
    { A_CHR, {1, 10, 50, _kiss_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static void _aphrodite_calc_bonuses(void)
{
    plr->sustain_chr = TRUE;
}
static int _aphrodite_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _aphrodite_powers);
}
static void _aphrodite_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SUST_CHR);
}
static plr_race_ptr _aphrodite_race(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {
        me = _race_alloc(DEMIGOD_APHRODITE);
        me->subname = "Aphrodite";
        me->subdesc = "Aphrodite is the Greek goddess of love, beauty, pleasure, and procreation. "
                     "You inherit her sex appeal. As such, your pets are more obedient and shopkeepers "
                     "fawn over you in their efforts to please you. You may kiss monsters and they "
                     "might even decide to follow you with slavish devotion, though sometimes this "
                     "just angers them.";
        me->stats[A_CHR] += 2;
        me->exp += 40;
        me->skills.dev = 3;
        me->shop_adjust = 70;
        me->hooks.calc_bonuses = _aphrodite_calc_bonuses;
        me->hooks.get_powers = _aphrodite_get_powers;
        me->hooks.get_flags = _aphrodite_get_flags;
        me->boss_r_idx = mon_race_parse("P.Aphrodite")->id;
    }
    return me;
}
/****************************************************************
 * Apollo
 ****************************************************************/
static power_info _apollo_powers[] =
{
    { A_INT, { 5,  3, 50, light_area_spell}},
    { A_WIS, {12,  7, 60, ray_of_sunlight_spell}},
    { A_CHR, {20, 10, 40, _shine_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static void _apollo_calc_bonuses(void)
{
    res_add_immune(GF_LIGHT);
    res_add(GF_BLIND);
    /* cf calc_torch in xtra1.c for the 'extra light' */
}
static int _apollo_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _apollo_powers);
}
static void _apollo_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_BLIND));
    add_flag(flgs, OF_IM_(GF_LIGHT));
}
static plr_race_ptr _apollo_race(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {
        me = _race_alloc(DEMIGOD_APOLLO);
        me->subname = "Apollo";
        me->subdesc = "Apollo has been variously recognized as a god of light and the sun, "
                     "truth and prophecy, medicine, healing, plague, music, poetry, arts, "
                     "archery, and more. You inherit powers of illumination and are completely "
                     "immune to light based attacks. You are seldom blinded.";
        me->exp += 50;
        me->skills.dev = 4;
        me->hooks.calc_bonuses = _apollo_calc_bonuses;
        me->hooks.get_powers = _apollo_get_powers;
        me->hooks.get_flags = _apollo_get_flags;
        me->boss_r_idx = mon_race_parse("P.Apollo")->id;
    }
    return me;
}

/****************************************************************
 * Ares
 ****************************************************************/
static power_info _ares_powers[] =
{
    { A_STR, {10, 10, 30, berserk_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static void _ares_calc_bonuses(void)
{
    int dam = 1 + plr->lev/7;
    int ac = 1 + plr->lev/5;
    int hand;

    plr->sustain_str = TRUE;

    plr->to_a += ac;
    plr->dis_to_a += ac;

    plr->to_d_m  += dam;
    plr->innate_attack_info.to_d += dam;
    for (hand = 0; hand < MAX_HANDS; hand++)
    {
        if (plr->attack_info[hand].type != PAT_NONE)
        {
            plr->attack_info[hand].to_d += dam / plr->weapon_ct;
            plr->attack_info[hand].dis_to_d += dam / plr->weapon_ct;
        }
    }
}
static int _ares_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _ares_powers);
}
static void _ares_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SUST_STR);
}
static plr_race_ptr _ares_race(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {
        me = _race_alloc(DEMIGOD_ARES);
        me->subname = "Ares";
        me->subdesc = "Ares is the bold son of Zeus and Hera, whose very name is feared and respected "
                     "by warriors and citizens alike. His legendary combat prowess exceeds that of "
                     "Zeus and Poseidon, but he is less skilled in wiles than the other Olympians. "
                     "You inherit exceptional bonuses to combat and have a firm grip on your strength. "
                     "Later in life, you will be able to fly into a berserk rage on command. However, "
                     "your lust for combat decreases your stealth as you call out challenges "
                     "to all that you meet. And your resistance to magic is suspect as well.";
        me->stats[A_STR] += 2;
        me->skills.dev  = 0;
        me->skills.sav -= 5;
        me->skills.stl -= 1;
        me->skills.thn += 15;
        me->exp += 60;
        me->hooks.calc_bonuses = _ares_calc_bonuses;
        me->hooks.get_powers = _ares_get_powers;
        me->hooks.get_flags = _ares_get_flags;
        me->boss_r_idx = mon_race_parse("P.Ares")->id;
    }
    return me;
}

/****************************************************************
 * Artemis
 ****************************************************************/
static void _artemis_calc_bonuses(void)
{
    plr->shooter_info.to_d += 1 + plr->lev/7;
    plr->shooter_info.dis_to_d += 1 + plr->lev/7;
    plr->sustain_dex = TRUE;
}
static void _artemis_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SUST_DEX);
}
static void _artemis_birth(void)
{
    plr_birth_obj_aux(TV_BOW, SV_SHORT_BOW, 1);
    plr_birth_obj_aux(TV_ARROW, SV_ARROW, rand_range(15, 20));
    plr_birth_food();
    plr_birth_light();
}
static plr_race_ptr _artemis_race(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {
        me = _race_alloc(DEMIGOD_ARTEMIS);
        me->subname = "Artemis";
        me->subdesc = "Artemis was often described as the daughter of Zeus and Leto, and the "
                     "twin sister of Apollo. She was the Hellenic goddess of the hunt, wild "
                     "animals, wilderness, childbirth, virginity and young girls, bringing "
                     "and relieving disease in women; she often was depicted as a huntress "
                     "carrying a bow and arrows. You inherit powers of archery and are very "
                     "nimble. Your skills with the bow are unmatched and you shoot arrows "
                     "with increased range and deadliness.";
        me->stats[A_DEX] += 2;
        me->skills.thb += 12;
        me->exp += 50;
        me->hooks.birth = _artemis_birth;
        me->hooks.calc_bonuses = _artemis_calc_bonuses;
        me->hooks.get_flags = _artemis_get_flags;
        me->boss_r_idx = mon_race_parse("P.Artemis")->id;
    }
    return me;
}

/****************************************************************
 * Athena
 ****************************************************************/
static void _athena_calc_bonuses(void)
{
    plr->sustain_int = TRUE;
}
static void _athena_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SUST_INT);
}
static plr_race_ptr _athena_race(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {
        me = _race_alloc(DEMIGOD_ATHENA);
        me->subname = "Athena";
        me->subdesc = "Athena is the great goddess of wisdom and the protector of Athens. She was "
                     "born of Zeus and the Titan Metis, and her cunning far surpasses that of the "
                     "other deities. You inherit great clarity of thought and magic and will be "
                     "able to cast spells more reliably than other mortals. And should you fail to "
                     "cast a spell, you will pay a reduced casting cost rather than the full amount. "
                     "You also have a firm grip on your mental prowess.";
        me->stats[A_INT] += 2;
        me->exp += 60;
        me->skills.dev  = 9;
        me->skills.sav += 2;
        me->hooks.calc_bonuses = _athena_calc_bonuses;
        me->hooks.get_flags = _athena_get_flags;
        me->boss_r_idx = mon_race_parse("P.Athena")->id;
    }
    return me;
}

/****************************************************************
 * Demeter
 ****************************************************************/
static power_info _demeter_powers[] =
{
    { A_WIS, {5, 0, 60, _demeter_clw_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static void _demeter_calc_bonuses(void)
{
    plr->regen += 100;
    plr->slow_digest = TRUE;
    if (plr->lev >= 40)
        res_add(GF_TIME);
}
static int _demeter_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _demeter_powers);
}
static void _demeter_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_REGEN);
    add_flag(flgs, OF_SLOW_DIGEST);
    if (plr->lev >= 40)
        add_flag(flgs, OF_RES_(GF_TIME));
}
static plr_race_ptr _demeter_race(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {
        me = _race_alloc(DEMIGOD_DEMETER);
        me->subname = "Demeter";
        me->subdesc = "Demeter is the goddess of the harvest, who presided over grains, the "
                     "fertility of the earth, and the seasons. You gain powers of regeneration, "
                     "healing, and temperance. Eventually, you will become resistant to the "
                     "ravages of time.";
        me->exp += 40;
        me->skills.sav += 5;
        me->hooks.calc_bonuses = _demeter_calc_bonuses;
        me->hooks.get_powers = _demeter_get_powers;
        me->hooks.get_flags = _demeter_get_flags;
        me->boss_r_idx = mon_race_parse("P.Demeter")->id;
    }
    return me;
}

/****************************************************************
 * Hades
 ****************************************************************/
static void _hades_calc_bonuses(void)
{
    res_add(GF_NETHER);
    plr->hold_life++;
    plr->sustain_con = TRUE;
}
static void _hades_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_NETHER));
    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_SUST_CON);
}
static plr_race_ptr _hades_race(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {
        me = _race_alloc(DEMIGOD_HADES);
        me->subname = "Hades";
        me->subdesc = "Hades is Ruler of the Underworld. You gain resistance to nether forces, "
                     "increased fortitude and have a firm grasp on your life and health.";
        me->stats[A_CON] += 2;
        me->skills.sav += 10;
        me->skills.thn += 5;
        me->skills.stl += 1;
        me->life += 7;
        me->exp += 60;
        me->hooks.calc_bonuses = _hades_calc_bonuses;
        me->hooks.get_flags = _hades_get_flags;
        me->boss_r_idx = mon_race_parse("P.Hades")->id;
    }
    return me;
}

/****************************************************************
 * Hephaestus
 ****************************************************************/
static void _hephaestus_calc_bonuses(void)
{
    res_add(GF_DISENCHANT);
}
static void _hephaestus_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_DISENCHANT));
}
static plr_race_ptr _hephaestus_race(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {
        me = _race_alloc(DEMIGOD_HEPHAESTUS);
        me->subname = "Hephaestus";
        me->subdesc = "Hephaestus was the god of technology, blacksmiths, craftsmen, artisans, "
                     "sculptors, metals, metallurgy, fire and volcanoes. Like other mythic smiths "
                     "but unlike most other gods, Hephaestus was lame, which gave him a "
                     "grotesque appearance in Greek eyes. He served as the blacksmith of the "
                     "gods. You inherit powers of enchantment and protection. Your equipment "
                     "will not suffer diminution by corrosion or magical attack.";
        me->exp += 50;
        me->skills.thn += 10;
        me->skills.sav += 2;
        me->hooks.calc_bonuses = _hephaestus_calc_bonuses;
        me->hooks.get_flags = _hephaestus_get_flags;
        me->boss_r_idx = mon_race_parse("P.Hephaestus")->id;
    }
    return me;
}

/****************************************************************
 * Hera
 ****************************************************************/
static power_info _hera_powers[] =
{
    { A_WIS, {15, 0, 30, clear_mind_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static void _hera_calc_bonuses(void)
{
    plr->spell_cap += 2;
    if (plr->lev >= 15)
        plr->clear_mind = TRUE;
}
static int _hera_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _hera_powers);
}
static void _hera_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SPELL_CAP);
}
static plr_race_ptr _hera_race(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {
        me = _race_alloc(DEMIGOD_HERA);
        me->subname = "Hera";
        me->subdesc = "Hera was the wife and one of three sisters of Zeus. Her chief function "
                     "was as the goddess of women and marriage. You inherit great clarity of "
                     "mind and capacity for magic. You are very wise and have a firm grasp on "
                     "your magical energy, which is even augmented.";
        me->stats[A_WIS] += 2;
        me->exp += 40;
        me->skills.sav += 5;
        me->skills.dev  = 6;
        me->hooks.calc_bonuses = _hera_calc_bonuses;
        me->hooks.get_powers = _hera_get_powers;
        me->hooks.get_flags = _hera_get_flags;
        me->boss_r_idx = mon_race_parse("P.Hera")->id;
    }
    return me;
}

/****************************************************************
 * Hermes
 ****************************************************************/
static void _hermes_calc_bonuses(void)
{
    plr->pspeed += 5 * plr->lev/50;
    plr->no_slow = TRUE;
}
static void _hermes_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (plr->lev >= 10)
        add_flag(flgs, OF_SPEED);
}
static plr_race_ptr _hermes_race(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {
        me = _race_alloc(DEMIGOD_HERMES);
        me->subname = "Hermes";
        me->subdesc = "Hermes, the Messenger, is the extremely cunning diplomat used by the Olympians "
                     "to negotiate truces. With his Winged Sandals and his powerful magic, there "
                     "is no place barred from him, and there is no way to detain him. You inherit "
                     "great powers of motion and will never become slowed. Your stealth is better "
                     "than your brethren and your speed will increase with experience.";
        me->skills.dis += 10;
        me->skills.stl += 3;
        me->skills.srh += 5;
        me->skills.fos += 5;
        me->exp += 60;
        me->hooks.calc_bonuses = _hermes_calc_bonuses;
        me->hooks.get_flags = _hermes_get_flags;
        me->boss_r_idx = mon_race_parse("P.Hermes")->id;
    }
    return me;
}

/****************************************************************
 * Poseidon
 ****************************************************************/
static void _poseidon_birth(void)
{
    plr_birth_obj_aux(TV_POLEARM, SV_TRIDENT, 1);
    skills_weapon_init(TV_POLEARM, SV_TRIDENT, WEAPON_EXP_BEGINNER);
    plr_birth_food();
    plr_birth_light();
}

static void _poseidon_calc_bonuses(void)
{
    plr->melt_armor = TRUE;
    if (plr->lev >= 5)
        res_add(GF_ACID);
    if (plr->lev >= 10)
        res_add(GF_COLD);
    if (plr->lev >= 20)
        res_add(GF_ELEC);
}
static void _poseidon_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (plr->lev >= 5)
        add_flag(flgs, OF_RES_(GF_ACID));
    if (plr->lev >= 10)
        add_flag(flgs, OF_RES_(GF_COLD));
    if (plr->lev >= 20)
        add_flag(flgs, OF_RES_(GF_ELEC));
}
static plr_race_ptr _poseidon_race(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {
        me = _race_alloc(DEMIGOD_POSEIDON);
        me->subname = "Poseidon";
        me->subdesc = "Poseidon, Brother of Zeus, is Lord of the Seas and Storm. You inherit "
                     "elemental protection and corrosive attacks that melt the armor of your foes.";
        me->stats[A_STR] += 1;
        me->stats[A_DEX] += 1;
        me->skills.thn += 7;
        me->exp += 60;
        me->hooks.birth = _poseidon_birth;
        me->hooks.calc_bonuses = _poseidon_calc_bonuses;
        me->hooks.get_flags = _poseidon_get_flags;
        me->boss_r_idx = mon_race_parse("P.Poseidon")->id;
    }
    return me;
}

/****************************************************************
 * Zeus
 ****************************************************************/
static void _zeus_calc_bonuses(void)
{
    res_add(GF_ELEC);
    plr->sh_elec = TRUE;
}
static void _zeus_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_ELEC));
    add_flag(flgs, OF_AURA_ELEC);
}
static plr_race_ptr _zeus_race(void)
{
    static plr_race_ptr me = NULL;
    if (!me)
    {
        int i;
        me = _race_alloc(DEMIGOD_ZEUS);
        me->subname = "Zeus";
        me->subdesc = "Zeus, King of the gods and ruler of Mount Olympus, is god of the Sky "
                     "and Thunder, and nominal husband of Hera. You inherit increased stature "
                     "and your divine birth will be marked by your aura of electricity. You also "
                     "resist lightning.";
        me->skills.dis += 2;
        me->skills.dev  = 5;
        me->skills.sav += 5;
        me->skills.thn += 10;
        me->skills.thb += 3;
        for (i = 0; i < 6; i++)
            me->stats[i]++;
        me->exp += 70;
        me->shop_adjust = 90;
        me->hooks.calc_bonuses = _zeus_calc_bonuses;
        me->hooks.get_flags = _zeus_get_flags;
        me->boss_r_idx = mon_race_parse("P.Zeus")->id;
    }
    return me;
}

/****************************************************************
 * Public
 ****************************************************************/
plr_race_ptr demigod_get_race(int psubrace)
{
    if (birth_hack && psubrace >= DEMIGOD_MAX)
        psubrace = 0;
    assert(0 <= psubrace && psubrace < DEMIGOD_MAX);
    switch (psubrace)
    {
    case DEMIGOD_APHRODITE:  return _aphrodite_race();
    case DEMIGOD_APOLLO:     return _apollo_race();
    case DEMIGOD_ARES:       return _ares_race();
    case DEMIGOD_ARTEMIS:    return _artemis_race();
    case DEMIGOD_ATHENA:     return _athena_race();
    case DEMIGOD_DEMETER:    return _demeter_race();
    case DEMIGOD_HADES:      return _hades_race();
    case DEMIGOD_HEPHAESTUS: return _hephaestus_race();
    case DEMIGOD_HERA:       return _hera_race();
    case DEMIGOD_HERMES:     return _hermes_race();
    case DEMIGOD_POSEIDON:   return _poseidon_race();
    case DEMIGOD_ZEUS:       return _zeus_race();
    }
    return _minor_race();
}
