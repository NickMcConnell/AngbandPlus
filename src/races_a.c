#include "angband.h"

/****************************************************************
 * Amberite
 ****************************************************************/
static power_info _amberite_powers[] =
{
    { A_INT, {30, 50, 70, shadow_shifting_spell}},
    { A_WIS, {40, 75, 75, pattern_mindwalk_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _amberite_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _amberite_powers);
}
static void _amberite_calc_bonuses(void)
{
    p_ptr->sustain_con = TRUE;
    p_ptr->regenerate = TRUE;
}
static void _amberite_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_SUST_CON);
    add_flag(flgs, TR_REGEN);
}
race_t *amberite_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Amberite";
        me.desc = "The Amberites are a reputedly immortal race, who are endowed with numerous "
                    "advantages in addition to their longevity. They are very tough and their "
                    "constitution cannot be reduced, and their ability to heal wounds far "
                    "surpasses that of any other race. Having seen virtually everything, "
                    "very little is new to them, and they gain levels much slower than the "
                    "other races.";

        me.stats[A_STR] =  1;
        me.stats[A_INT] =  2;
        me.stats[A_WIS] =  2;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] =  3;
        me.stats[A_CHR] =  0;
        
        me.skills.dis =  4;
        me.skills.dev =  5;
        me.skills.sav =  3;
        me.skills.stl =  2;
        me.skills.srh =  3;
        me.skills.fos = 13;
        me.skills.thn = 15;
        me.skills.thb = 10;

        me.life = 100;
        me.base_hp = 20;
        me.exp = 190;
        me.infra = 0;

        me.calc_bonuses = _amberite_calc_bonuses;
        me.get_powers = _amberite_get_powers;
        me.get_flags = _amberite_get_flags;
        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Android
 ****************************************************************/
static power_info _android_powers[] =
{
    { A_STR, {1, 7, 30, android_ray_gun_spell}},
    { A_STR, {10, 13, 30, android_blaster_spell}},
    { A_STR, {25, 26, 40, android_bazooka_spell}},
    { A_STR, {35, 40, 50, android_beam_cannon_spell}},
    { A_STR, {45, 60, 70, android_rocket_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _android_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _android_powers);
}
static void _android_calc_bonuses(void)
{
    int ac = 10 + (p_ptr->lev * 2 / 5);

    p_ptr->to_a += ac;
    p_ptr->dis_to_a += ac;

    p_ptr->slow_digest = TRUE;
    p_ptr->free_act = TRUE;
    res_add(RES_POIS);
    res_add_vuln(RES_ELEC);
    p_ptr->hold_life = TRUE;
}
static void _android_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_FREE_ACT);
    add_flag(flgs, TR_RES_POIS);
    add_flag(flgs, TR_SLOW_DIGEST);
    add_flag(flgs, TR_HOLD_LIFE);
}
static void _android_get_vulnerabilities(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_ELEC);
}
race_t *android_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Android";
        me.desc = "An android is a artificial creation with a body of machinery. Over the millenia, artificial "
                    "intelligence has improved to the point where androids are nearly as smart as humans, though "
                    "perhaps not so wise. Of course, their mechanical body offers great physical advantages, far "
                    "surpassing the powers of man.  Androids don't acquire experience like other "
                    "races, but rather gain in power as they attach new equipment to their frame. "
                    "Rings, amulets, and lights do not influence growth.  Androids are resistant to "
                    "poison, can move freely, and are immune to life-draining attacks.  Moreover, "
                    "because of their hard metallic bodies, they get a bonus to AC. Androids have "
                    "electronic circuits throughout their body and must beware of electric shocks. "
                    "They gain very little nutrition from the food of mortals, but they can use flasks "
                    "of oil as their energy source.";

        me.stats[A_STR] =  3;
        me.stats[A_INT] = -1;
        me.stats[A_WIS] = -5;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] =  3;
        me.stats[A_CHR] =  0;
        
        me.skills.dis =  0;
        me.skills.dev = -5;
        me.skills.sav =  0;
        me.skills.stl = -2;
        me.skills.srh =  3;
        me.skills.fos = 14;
        me.skills.thn = 20;
        me.skills.thb = 10;

        me.life = 108;
        me.base_hp = 26;
        me.exp = 200;
        me.infra = 0;

        me.calc_bonuses = _android_calc_bonuses;
        me.get_powers = _android_get_powers;
        me.get_flags = _android_get_flags;
        me.get_vulnerabilities = _android_get_vulnerabilities;
        me.flags = RACE_IS_NONLIVING;

        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Archon
 ****************************************************************/
static void _archon_calc_bonuses(void)
{
    p_ptr->levitation = TRUE;
    p_ptr->see_inv = TRUE;
    p_ptr->align += 200;
}
static void _archon_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_LEVITATION);
    add_flag(flgs, TR_SEE_INVIS);
}
race_t *archon_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Archon";
        me.desc = "Archons are a higher class of angels. They are good at all skills, and are strong, "
                    "wise, and are a favorite with any people. They are able to see the unseen, and "
                    "their wings allow them to safely fly over traps and other dangerous places. However, "
                    "belonging to a higher plane as they do, the experiences of this world do not leave "
                    "a strong impression on them and they gain levels slowly.";

        me.stats[A_STR] =  2;
        me.stats[A_INT] =  0;
        me.stats[A_WIS] =  4;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] =  2;
        me.stats[A_CHR] =  3;
        
        me.skills.dis =  0;
        me.skills.dev = 12;
        me.skills.sav =  8;
        me.skills.stl =  2;
        me.skills.srh =  2;
        me.skills.fos = 11;
        me.skills.thn = 10;
        me.skills.thb = 10;

        me.life = 103;
        me.base_hp = 22;
        me.exp = 200;
        me.infra = 3;

        me.calc_bonuses = _archon_calc_bonuses;
        me.get_flags = _archon_get_flags;
        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Balrog
 ****************************************************************/
static power_info _balrog_powers[] =
{
    { A_CON, {15, 10, 70, demon_breath_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _balrog_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _balrog_powers);
}
static void _balrog_calc_bonuses(void)
{
    res_add(RES_FIRE);
    res_add(RES_NETHER);
    p_ptr->hold_life = TRUE;
    if (p_ptr->lev >= 10) p_ptr->see_inv = TRUE;
    if (p_ptr->lev >= 45) res_add(RES_FIRE);
    p_ptr->align -= 200;
}
static void _balrog_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_FIRE);
    add_flag(flgs, TR_RES_NETHER);
    add_flag(flgs, TR_HOLD_LIFE);
    if (p_ptr->lev >= 10)
        add_flag(flgs, TR_SEE_INVIS);
}
race_t *balrog_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Balrog";
        me.desc = "Balrogs are a higher class of demons. They are strong, intelligent and tough. They do "
                    "not believe in gods, and are not suitable for priest at all. Balrog are resistant to "
                    "fire and nether, and have a firm hold on their life force. They also eventually learn "
                    "to see invisible things. They are good at almost all skills except stealth. They gain "
                    "very little nutrition from the food of mortals, and need human corpses as sacrifices "
                    "to regain their vitality.";

        me.stats[A_STR] =  4;
        me.stats[A_INT] =  2;
        me.stats[A_WIS] =-10;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] =  3;
        me.stats[A_CHR] =  2;
        
        me.skills.dis = -3;
        me.skills.dev = 12;
        me.skills.sav = 15;
        me.skills.stl = -2;
        me.skills.srh =  1;
        me.skills.fos =  8;
        me.skills.thn = 20;
        me.skills.thb =  0;

        me.life = 106;
        me.base_hp = 24;
        me.exp = 180;
        me.infra = 5;
        me.flags = RACE_IS_NONLIVING | RACE_IS_DEMON;

        me.calc_bonuses = _balrog_calc_bonuses;
        me.get_powers = _balrog_get_powers;
        me.get_flags = _balrog_get_flags;
        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Barbarian
 ****************************************************************/
static power_info _barbarian_powers[] =
{
    { A_STR, {8, 10, 30, berserk_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _barbarian_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _barbarian_powers);
}
static void _barbarian_calc_bonuses(void)
{
    res_add(RES_FEAR);
}
static void _barbarian_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_FEAR);
}
race_t *barbarian_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Barbarian";
        me.desc = "Barbarians are hardy men of the north. They are fierce in combat, and their wrath "
                    "is feared throughout the world. Combat is their life: they feel no fear, and they "
                    "learn to enter battle frenzy at will even sooner than half-trolls. Barbarians are, "
                    "however, suspicious of magic, which makes magic devices fairly hard for them to use.";

        me.stats[A_STR] =  3;
        me.stats[A_INT] = -2;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] =  2;
        me.stats[A_CHR] =  2;
        
        me.skills.dis = -2;
        me.skills.dev = -10;
        me.skills.sav = 2;
        me.skills.stl = -1;
        me.skills.srh = 1;
        me.skills.fos = 7;
        me.skills.thn = 12;
        me.skills.thb = 10;

        me.life = 103;
        me.base_hp = 22;
        me.exp = 135;
        me.infra = 0;

        me.calc_bonuses = _barbarian_calc_bonuses;
        me.get_powers = _barbarian_get_powers;
        me.get_flags = _barbarian_get_flags;
        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Beastman
 ****************************************************************/
static void _beastman_gain_level(int new_level)
{
    if (one_in_(5))
    {
        msg_print("You feel different...");
        mut_gain_random(NULL);
    }
}
static void _beastman_calc_bonuses(void)
{
    res_add(RES_CONF);
    res_add(RES_SOUND);
}
static void _beastman_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_SOUND);
    add_flag(flgs, TR_RES_CONF);
}
race_t *beastman_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Beastman";
        me.desc = "This race is a blasphemous abomination produced by Chaos. It is not an independent "
                    "race but rather a humanoid creature, most often a human, twisted by the Chaos, "
                    "or a nightmarish crossbreed of a human and a beast. All Beastmen are accustomed "
                    "to Chaos so much that they are untroubled by confusion and sound, although raw "
                    "logrus can still have effects on them. Beastmen revel in chaos, as it twists them "
                    "more and more. Beastmen are subject to mutations: when they have been created, "
                    "they receive a random mutation. After that, every time they advance a level "
                    "they have a small chance of gaining yet another mutation.";

        me.stats[A_STR] =  2;
        me.stats[A_INT] = -2;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] = -1;
        me.stats[A_CON] =  2;
        me.stats[A_CHR] =  1;
        
        me.skills.dis = -5;
        me.skills.dev = -2;
        me.skills.sav = -1;
        me.skills.stl = -1;
        me.skills.srh = -1;
        me.skills.fos = 5;
        me.skills.thn = 12;
        me.skills.thb = 5;

        me.life = 102;
        me.base_hp = 22;
        me.exp = 150;
        me.infra = 0;

        me.calc_bonuses = _beastman_calc_bonuses;
        me.gain_level = _beastman_gain_level;
        me.get_flags = _beastman_get_flags;
        init = TRUE;
    }

    return &me;
}


/****************************************************************
 * Centaur
 ****************************************************************/
static void _centaur_birth(void) 
{ 
    equip_on_change_race();
}

static void _jump_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Jump");
        break;
    case SPELL_DESC:
        var_set_string(res, "Leap a short distance, clearing any intervening monsters or obstacles.");
        break;
    case SPELL_CAST:
    {
        int x, y;
        int len = 2 + p_ptr->lev/35;

        var_set_bool(res, FALSE);

        if (!tgt_pt(&x, &y, len)) return;

        if (distance(y, x, py, px) > len)
        {
            msg_print("You can't jump that far.");
            return;
        }
        if (!los(py, px, y, x))
        {
            msg_print("You can't see that location.");
            return;
        }
        if (!cave_player_teleportable_bold(y, x, 0L))
        {
            msg_print("You can't leap there!");
            return;
        }
        teleport_player_to(y, x, 0L);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static power_info _centaur_powers[] =
{
    { A_DEX, {15, 10, 50, _jump_spell}},
    { -1, {-1, -1, -1, NULL} }
};

static int _centaur_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _centaur_powers);
}

static void _centaur_calc_bonuses(void)
{
    int slot = equip_find_first(object_is_body_armour);
    p_ptr->pspeed += p_ptr->lev / 10;

    if (slot)
    {
        object_type *o_ptr = equip_obj(slot);
        p_ptr->to_a -= o_ptr->ac / 3;
        p_ptr->to_a -= o_ptr->to_a / 3;

        p_ptr->dis_to_a -= o_ptr->ac / 3;
        p_ptr->dis_to_a -= o_ptr->to_a / 3;
    }
}

static void _centaur_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    if (p_ptr->lev >= 10)
        add_flag(flgs, TR_SPEED);
}

static void _centaur_calc_innate_attacks(void)
{
    int l = p_ptr->lev;
    int to_d = l/10 + l*l/500 + l*l*l/25000;
    int to_h = l/2;
    innate_attack_t    a = {0};

    a.dd = 1 + l / 16;
    a.ds = 4 + l / 21;
    a.to_d += to_d; 
    a.to_h += to_h;

    a.weight = 150;
    calc_innate_blows(&a, 200);
    a.msg = "You kick %s.";
    a.name = "Hooves";

    p_ptr->innate_attacks[p_ptr->innate_attack_ct++] = a;
}

race_t *centaur_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Centaur";
        me.desc = "Centaurs are creatures with the head, arms and torso of a human combined with the "
                    "body and legs of a horse. As such, they are able to move more quickly as "
                    "they gain experience and are capable of leaping great distances. They may "
                    "attack monsters with their hooves in addition to any normal melee weapons. "
                    "Centaurs are strong, agile, and wise, but not so smart. "
                    "They are skilled in fighting and archery but are rather distrustful of magic. "
                    "Finally, being at home in the forests of the world, Centaurs "
                    "are able to move quickly through foliage.";

        me.stats[A_STR] =  3;
        me.stats[A_INT] = -2;
        me.stats[A_WIS] =  1;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] =  1;
        me.stats[A_CHR] =  0;
        
        me.skills.dis =  0;
        me.skills.dev = -5;
        me.skills.sav =  2;
        me.skills.stl =  1;
        me.skills.srh =  3;
        me.skills.fos =  5;
        me.skills.thn = 10;
        me.skills.thb = 12;

        me.life = 103;
        me.base_hp = 22;
        me.exp = 190;
        me.infra = 0;

        me.birth = _centaur_birth;
        me.calc_innate_attacks = _centaur_calc_innate_attacks;
        me.get_powers = _centaur_get_powers;
        me.calc_bonuses = _centaur_calc_bonuses;
        me.get_flags = _centaur_get_flags;

        me.equip_template = &b_info[46];

        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Cyclops
 ****************************************************************/
static power_info _cyclops_powers[] =
{
    { A_STR, {20, 0, 50, throw_boulder_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _cyclops_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _cyclops_powers);
}
static void _cyclops_calc_bonuses(void)
{
    res_add(RES_SOUND);
}
static void _cyclops_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_SOUND);
}
race_t *cyclops_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Cyclops";
        me.desc = "With but one eye, a Cyclops can see more than many with two eyes. They are "
                    "headstrong, and loud noises bother them very little. They are not quite "
                    "qualified for the magic using professions, but as a certain Mr. Ulysses "
                    "can testify, their accuracy with thrown rocks can be deadly!";

        me.stats[A_STR] =  4;
        me.stats[A_INT] = -3;
        me.stats[A_WIS] = -2;
        me.stats[A_DEX] = -3;
        me.stats[A_CON] =  4;
        me.stats[A_CHR] = -1;
        
        me.skills.dis = -4;
        me.skills.dev = -5;
        me.skills.sav = -3;
        me.skills.stl = -2;
        me.skills.srh = -2;
        me.skills.fos =  5;
        me.skills.thn = 20;
        me.skills.thb = 12;

        me.life = 108;
        me.base_hp = 24;
        me.exp = 155;
        me.infra = 1;

        me.calc_bonuses = _cyclops_calc_bonuses;
        me.get_powers = _cyclops_get_powers;
        me.get_flags = _cyclops_get_flags;
        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Dark-Elf
 ****************************************************************/
static power_info _dark_elf_powers[] =
{
    { A_INT, {1, 2, 30, magic_missile_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _dark_elf_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _dark_elf_powers);
}
static void _dark_elf_calc_bonuses(void)
{
    res_add(RES_DARK);
    p_ptr->spell_cap += 3;
    if (p_ptr->lev >= 20) p_ptr->see_inv = TRUE;
}
static void _dark_elf_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_DARK);
    add_flag(flgs, TR_SPELL_CAP);
    if (p_ptr->lev >= 20)
        add_flag(flgs, TR_SEE_INVIS);
}
race_t *dark_elf_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Dark-Elf";
        me.desc = "Another dark, cave-dwelling race, likewise unhampered by darkness attacks, "
                    "the Dark Elves have a long tradition and knowledge of magic. They have an "
                    "inherent magic missile attack available to them at a low level. With their "
                    "keen sight, they also learn to see invisible things as their relatives "
                    "High-Elves do, but at a higher level.";

        me.stats[A_STR] = -1;
        me.stats[A_INT] =  3;
        me.stats[A_WIS] =  2;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] = -2;
        me.stats[A_CHR] =  3;
        
        me.skills.dis = 5;
        me.skills.dev = 10;
        me.skills.sav = 12;
        me.skills.stl = 3;
        me.skills.srh = 8;
        me.skills.fos = 12;
        me.skills.thn = -5;
        me.skills.thb = 10;

        me.life = 97;
        me.base_hp = 18;
        me.exp = 155;
        me.infra = 5;

        me.calc_bonuses = _dark_elf_calc_bonuses;
        me.get_powers = _dark_elf_get_powers;
        me.get_flags = _dark_elf_get_flags;
        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Draconian
 ****************************************************************/
static power_info _draconian_powers[] =
{
    { A_CON, {1, 1, 70, draconian_breath_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _draconian_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _draconian_powers);
}
static void _draconian_calc_bonuses(void)
{
    p_ptr->levitation = TRUE;
    if (p_ptr->lev >=  5) res_add(RES_FIRE);
    if (p_ptr->lev >= 10) res_add(RES_COLD);
    if (p_ptr->lev >= 15) res_add(RES_ACID);
    if (p_ptr->lev >= 20) res_add(RES_ELEC);
    if (p_ptr->lev >= 30) res_add(RES_POIS);
}
static void _draconian_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_LEVITATION);
    if (p_ptr->lev >= 5)
        add_flag(flgs, TR_RES_FIRE);
    if (p_ptr->lev >= 10)
        add_flag(flgs, TR_RES_COLD);
    if (p_ptr->lev >= 15)
        add_flag(flgs, TR_RES_ACID);
    if (p_ptr->lev >= 20)
        add_flag(flgs, TR_RES_ELEC);
    if (p_ptr->lev >= 30)
        add_flag(flgs, TR_RES_POIS);
}
race_t *draconian_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Draconian";
        me.desc = "Draconians are a humanoid race with dragon-like attributes. As they advance levels, they "
                    "gain new elemental resistances (up to Poison Resistance), and they also have a "
                    "breath weapon, which becomes more powerful with experience. The exact type of the "
                    "breath weapon depends on the Draconian's class and level.  With their wings, they "
                    "can easily escape any pit trap unharmed.";

        me.stats[A_STR] =  2;
        me.stats[A_INT] =  1;
        me.stats[A_WIS] =  1;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] =  2;
        me.stats[A_CHR] =  2;
        
        me.skills.dis = -2;
        me.skills.dev = 5;
        me.skills.sav = 2;
        me.skills.stl = 0;
        me.skills.srh = 1;
        me.skills.fos = 10;
        me.skills.thn = 5;
        me.skills.thb = 5;

        me.life = 103;
        me.base_hp = 22;
        me.exp = 195;
        me.infra = 2;

        me.calc_bonuses = _draconian_calc_bonuses;
        me.get_powers = _draconian_get_powers;
        me.get_flags = _draconian_get_flags;
        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Dunadan
 ****************************************************************/
static void _dunadan_calc_bonuses(void)
{
    p_ptr->sustain_con = TRUE;
}
static void _dunadan_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_SUST_CON);
}
race_t *dunadan_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Dunadan";
        me.desc = "Dunedain are a race of hardy men from the West. This elder race surpasses human "
                    "abilities in every field, especially constitution. However, being men of the world, "
                    "very little is new to them, and levels are very hard for them to gain. Their "
                    "constitution cannot be reduced. ";

        me.stats[A_STR] =  1;
        me.stats[A_INT] =  2;
        me.stats[A_WIS] =  2;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] =  3;
        me.stats[A_CHR] =  0;
        
        me.skills.dis =  4;
        me.skills.dev =  5;
        me.skills.sav =  3;
        me.skills.stl =  2;
        me.skills.srh =  3;
        me.skills.fos = 13;
        me.skills.thn = 15;
        me.skills.thb = 10;

        me.life = 100;
        me.base_hp = 20;
        me.exp = 160;
        me.infra = 0;

        me.calc_bonuses = _dunadan_calc_bonuses;
        me.get_flags = _dunadan_get_flags;
        init = TRUE;
    }

    return &me;
}


/****************************************************************
 * Dwarf
 ****************************************************************/
static power_info _dwarf_powers[] =
{
    { A_WIS, {5, 5, 50, detect_doors_stairs_traps_spell}},
    { A_CHR, {10, 5, 50, detect_treasure_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _dwarf_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _dwarf_powers);
}
static void _dwarf_calc_bonuses(void)
{
    res_add(RES_BLIND);
}
static void _dwarf_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_BLIND);
}
race_t *dwarf_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Dwarf";
        me.desc = "Dwarves are the headstrong miners and fighters of legend. Dwarves tend to be stronger "
                    "and tougher but slower and less intelligent than humans. Because they are so headstrong "
                    "and are somewhat wise, they resist spells which are cast on them. They are very good "
                    "at searching, perception, fighting, and bows. Dwarves  have miserable stealth. They "
                    "can never be blinded.";

        me.stats[A_STR] =  2;
        me.stats[A_INT] = -2;
        me.stats[A_WIS] =  2;
        me.stats[A_DEX] = -2;
        me.stats[A_CON] =  2;
        me.stats[A_CHR] =  1;
        
        me.skills.dis = 2;
        me.skills.dev = 7;
        me.skills.sav = 6;
        me.skills.stl = -1;
        me.skills.srh = 7;
        me.skills.fos = 10;
        me.skills.thn = 15;
        me.skills.thb = 0;

        me.life = 103;
        me.base_hp = 22;
        me.exp = 135;
        me.infra = 5;

        me.calc_bonuses = _dwarf_calc_bonuses;
        me.get_powers = _dwarf_get_powers;
        me.get_flags = _dwarf_get_flags;
        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Ent
 ****************************************************************/
static power_info _ent_powers[] =
{
    { A_CHR, {10, 20, 70, summon_tree_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _ent_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _ent_powers);
}
static void _ent_calc_bonuses(void)
{
    res_add_vuln(RES_FIRE);
    if (!equip_find_first(object_is_melee_weapon)) 
        p_ptr->skill_dig += p_ptr->lev * 10;
}
static void _ent_get_flags(u32b flgs[TR_FLAG_SIZE])
{
}
static void _ent_get_vulnerabilities(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_FIRE);
}
race_t *ent_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Ent";
        me.desc = "The Ents are a powerful race dating from the beginning of the world, oldest of all "
                    "animals or plants who inhabit Arda. Spirits of the land, they were summoned to "
                    "guard the forests of Middle-earth. Being much like trees they are very clumsy but "
                    "strong, and very susceptible to fire. They gain very little nutrition from the food "
                    "of mortals, but they can absorb water from potions as their nutrition.";

        me.skills.dis = -5;
        me.skills.dev =  2;
        me.skills.sav =  5;
        me.skills.stl = -1;
        me.skills.srh =  0;
        me.skills.fos =  9;
        me.skills.thn = 15;
        me.skills.thb = -5;

        me.life = 105;
        me.base_hp = 25;
        me.exp = 135;
        me.infra = 0;

        me.calc_bonuses = _ent_calc_bonuses;
        me.get_powers = _ent_get_powers;
        me.get_flags = _ent_get_flags;
        me.get_vulnerabilities = _ent_get_vulnerabilities;
        init = TRUE;
    }

    /* Since Ent racial stat bonuses are level dependent, we recalculate. 
       Note, this prevents hackery in files.c for displaying racial stat bonuses correctly.
    */
    {
        me.stats[A_STR] =  2;
        me.stats[A_INT] =  0;
        me.stats[A_WIS] =  2;
        me.stats[A_DEX] = -3;
        me.stats[A_CON] =  2;
        me.stats[A_CHR] =  0;

        if (!spoiler_hack) /* Otherwise, I need to be careful when generating automatic spoiler files! */
        {
            int amount = 0;
            if (p_ptr->lev >= 26) amount++;
            if (p_ptr->lev >= 41) amount++;
            if (p_ptr->lev >= 46) amount++;
            me.stats[A_STR] += amount;
            me.stats[A_DEX] -= amount;
            me.stats[A_CON] += amount;
        }
    }
    return &me;
}

/****************************************************************
 * Gnome
 ****************************************************************/
static power_info _gnome_powers[] =
{
    { A_INT, {5, 2, 50, phase_door_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _gnome_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _gnome_powers);
}
static void _gnome_calc_bonuses(void)
{
    p_ptr->free_act = TRUE;
}
static void _gnome_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_FREE_ACT);
}
race_t *gnome_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Gnome";
        me.desc = "Gnomes are smaller than dwarves but larger than Halflings. They, like the hobbits, "
                    "live in the earth in burrow-like homes. Gnomes make excellent mages, and have very "
                    "good saving throws. They are good at searching, disarming, perception, and stealth. "
                    "They have lower strength than humans so they are not very good at fighting with hand "
                    "weapons. Gnomes have fair infra-vision, so they can detect warm-blooded creatures "
                    "at a distance. Gnomes are intrinsically protected against paralysis.";

        me.stats[A_STR] = -1;
        me.stats[A_INT] =  2;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] =  1;
        me.stats[A_CHR] = -1;
        
        me.skills.dis = 10;
        me.skills.dev = 8;
        me.skills.sav = 7;
        me.skills.stl = 3;
        me.skills.srh = 6;
        me.skills.fos = 13;
        me.skills.thn = -8;
        me.skills.thb = 12;

        me.life = 95;
        me.base_hp = 16;
        me.exp = 115;
        me.infra = 4;

        me.calc_bonuses = _gnome_calc_bonuses;
        me.get_powers = _gnome_get_powers;
        me.get_flags = _gnome_get_flags;
        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Golem
 ****************************************************************/
static power_info _golem_powers[] =
{
    { A_CON, {20, 20, 50, stone_skin_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _golem_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _golem_powers);
}
static void _golem_calc_bonuses(void)
{
    int ac = 10 + (p_ptr->lev * 2 / 5);
    p_ptr->to_a += ac;
    p_ptr->dis_to_a += ac;
    p_ptr->no_stun = TRUE;

    p_ptr->slow_digest = TRUE;
    p_ptr->free_act = TRUE;
    p_ptr->see_inv = TRUE;
    res_add(RES_POIS);
    if (p_ptr->lev >= 35) p_ptr->hold_life = TRUE;

    p_ptr->pspeed -= p_ptr->lev/16;
}
static void _golem_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_SEE_INVIS);
    add_flag(flgs, TR_FREE_ACT);
    add_flag(flgs, TR_RES_POIS);
    add_flag(flgs, TR_SLOW_DIGEST);
    if (p_ptr->lev >= 35)
        add_flag(flgs, TR_HOLD_LIFE);
    if (p_ptr->lev >= 16)
        add_flag(flgs, TR_SPEED);
}
race_t *golem_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Golem";
        me.desc = "A Golem is an artificial creature, built from a lifeless raw material like clay, "
                    "and awakened to life. They are nearly mindless, making them useless for "
                    "professions which rely on magic, but as warriors they are very tough. They "
                    "are resistant to poison, they can see invisible things, and move freely. "
                    "At higher levels, they also become resistant to attacks which threaten to "
                    "drain away their life force. Golems gain very little nutrition from ordinary "
                    "food, but can absorb mana from staves and wands as their power source. Golems "
                    "also gain a natural armor class bonus from their tough body. Golems become "
                    "slower with age.";

        me.stats[A_STR] =  4;
        me.stats[A_INT] = -5;
        me.stats[A_WIS] = -5;
        me.stats[A_DEX] = -2;
        me.stats[A_CON] =  4;
        me.stats[A_CHR] =  0;
        
        me.skills.dis = -5;
        me.skills.dev = -5;
        me.skills.sav = 6;
        me.skills.stl = -1;
        me.skills.srh = -1;
        me.skills.fos = 8;
        me.skills.thn = 20;
        me.skills.thb = 0;

        me.life = 105;
        me.base_hp = 23;
        me.exp = 185;
        me.infra = 4;
        me.flags = RACE_IS_NONLIVING;

        me.get_powers = _golem_get_powers;
        me.calc_bonuses = _golem_calc_bonuses;
        me.get_flags = _golem_get_flags;
        init = TRUE;
    }

    return &me;
}


/****************************************************************
 * Half-Giant
 ****************************************************************/
static power_info _half_giant_powers[] =
{
    { A_STR, {20, 10, 70, stone_to_mud_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _half_giant_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _half_giant_powers);
}
static void _half_giant_calc_bonuses(void)
{
    p_ptr->sustain_str = TRUE;
    res_add(RES_SHARDS);
}
static void _half_giant_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_SHARDS);
    add_flag(flgs, TR_SUST_STR);
}
race_t *half_giant_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Half-Giant";
        me.desc = "Half-Giants limited intelligence makes it difficult for them to become full spellcasters, "
                    "but with their huge strength they make excellent warriors. Their thick skin makes "
                    "them resistant to shards, and like Half-Ogres and Half-Trolls, they have their strength "
                    "sustained.";

        me.stats[A_STR] =  4;
        me.stats[A_INT] = -2;
        me.stats[A_WIS] = -2;
        me.stats[A_DEX] = -2;
        me.stats[A_CON] =  3;
        me.stats[A_CHR] =  0;
        
        me.skills.dis = -6;
        me.skills.dev = -8;
        me.skills.sav = -3;
        me.skills.stl = -2;
        me.skills.srh = -1;
        me.skills.fos =  5;
        me.skills.thn = 25;
        me.skills.thb =  5;

        me.life = 108;
        me.base_hp = 26;
        me.exp = 150;
        me.infra = 3;

        me.calc_bonuses = _half_giant_calc_bonuses;
        me.get_powers = _half_giant_get_powers;
        me.get_flags = _half_giant_get_flags;
        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Half-Ogre
 ****************************************************************/
static power_info _half_ogre_powers[] =
{
    { A_INT, {25, 35, 70, explosive_rune_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _half_ogre_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _half_ogre_powers);
}
static void _half_ogre_calc_bonuses(void)
{
    res_add(RES_DARK);
    p_ptr->sustain_str = TRUE;
}
static void _half_ogre_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_SUST_STR);
    add_flag(flgs, TR_RES_DARK);
}
race_t *half_ogre_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Half-Ogre";
        me.desc = "Half-Ogres are like Half-Orcs, only more so. They are big, bad, and stupid. "
                    "For warriors, they have all the necessary attributes, and they can even "
                    "become wizards: after all, they are related to Ogre Magi, from whom they "
                    "have learned the skill of setting trapped runes once their level is high "
                    "enough.  Like Half-Orcs, they resist darkness, and like Half-Trolls, they "
                    "have their strength sustained.";

        me.stats[A_STR] =  3;
        me.stats[A_INT] = -2;
        me.stats[A_WIS] =  0;
        me.stats[A_DEX] = -1;
        me.stats[A_CON] =  3;
        me.stats[A_CHR] =  1;
        
        me.skills.dis = -3;
        me.skills.dev = -5;
        me.skills.sav = -3;
        me.skills.stl = -2;
        me.skills.srh = -1;
        me.skills.fos =  5;
        me.skills.thn = 20;
        me.skills.thb =  0;

        me.life = 106;
        me.base_hp = 23;
        me.exp = 140;
        me.infra = 3;

        me.calc_bonuses = _half_ogre_calc_bonuses;
        me.get_powers = _half_ogre_get_powers;
        me.get_flags = _half_ogre_get_flags;
        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Half-Titan
 ****************************************************************/
static power_info _half_titan_powers[] =
{
    { A_INT, {15, 10, 60, probing_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _half_titan_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _half_titan_powers);
}
static void _half_titan_calc_bonuses(void)
{
    res_add(RES_CHAOS);
}
static void _half_titan_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_CHAOS);
}
race_t *half_titan_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Half-Titan";
        me.desc = "Half-mortal descendants of the mighty titans, these immensely powerful creatures "
                    "put almost any other race to shame. They may lack the fascinating special powers "
                    "of certain other races, but their enhanced attributes more than make up for that. "
                    "They learn to estimate the strengths of their foes, and their love for law and "
                    "order makes them resistant to the effects of Chaos.";

        me.stats[A_STR] =  5;
        me.stats[A_INT] =  1;
        me.stats[A_WIS] =  2;
        me.stats[A_DEX] = -2;
        me.stats[A_CON] =  3;
        me.stats[A_CHR] =  3;
        
        me.skills.dis = -5;
        me.skills.dev =  5;
        me.skills.sav =  1;
        me.skills.stl = -2;
        me.skills.srh =  1;
        me.skills.fos =  8;
        me.skills.thn = 25;
        me.skills.thb =- 0;

        me.life = 112;
        me.base_hp = 28;
        me.exp = 255;
        me.infra = 0;

        me.calc_bonuses = _half_titan_calc_bonuses;
        me.get_powers = _half_titan_get_powers;
        me.get_flags = _half_titan_get_flags;
        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Half-Troll
 ****************************************************************/
static power_info _half_troll_powers[] =
{
    { A_STR, {10, 12, 50, berserk_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _half_troll_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _half_troll_powers);
}
static void _half_troll_calc_bonuses(void)
{
    p_ptr->sustain_str = TRUE;
    if (p_ptr->lev >= 15)
        p_ptr->regenerate = TRUE;
}
static void _half_troll_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_SUST_STR);
    if (p_ptr->lev >= 15)
        add_flag(flgs, TR_REGEN);
}
race_t *half_troll_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Half-Troll";
        me.desc = "Half-Trolls are incredibly strong, and have more hit points than most other races. "
                    "They are also very stupid and slow. They are bad at searching, disarming, perception, "
                    "and stealth. They are so ugly that a Half-Orc grimaces in their presence. "
                    "They also happen to be fun to run... Half-trolls always have their strength sustained. "
                    "At higher levels, Half-Trolls regenerate wounds automatically.";

        me.stats[A_STR] =  4;
        me.stats[A_INT] = -4;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] = -3;
        me.stats[A_CON] =  3;
        me.stats[A_CHR] = -2;
        
        me.skills.dis = -5;
        me.skills.dev = -8;
        me.skills.sav = -5;
        me.skills.stl = -2;
        me.skills.srh = -1;
        me.skills.fos =  5;
        me.skills.thn = 20;
        me.skills.thb =-10;

        me.life = 107;
        me.base_hp = 25;
        me.exp = 150;
        me.infra = 3;

        me.calc_bonuses = _half_troll_calc_bonuses;
        me.get_powers = _half_troll_get_powers;
        me.get_flags = _half_troll_get_flags;
        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * High-Elf
 ****************************************************************/
static void _high_elf_calc_bonuses(void)
{
    res_add(RES_LITE);
    p_ptr->see_inv = TRUE;
}
static void _high_elf_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_LITE);
    add_flag(flgs, TR_SEE_INVIS);
}
race_t *high_elf_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "High-Elf";
        me.desc = "High-elves are a race of immortal beings dating from the beginning of time. "
                    "They are masters of all skills, and are strong and intelligent, although "
                    "their wisdom is sometimes suspect. High-elves begin their lives able to "
                    "see the unseen, and resist light effects just like regular elves. However, "
                    "there are few things that they have not seen already, and experience is "
                    "very hard for them to gain.";

        me.stats[A_STR] =  1;
        me.stats[A_INT] =  3;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] =  3;
        me.stats[A_CON] =  1;
        me.stats[A_CHR] =  1;
        
        me.skills.dis =  4;
        me.skills.dev = 13;
        me.skills.sav = 12;
        me.skills.stl =  4;
        me.skills.srh =  3;
        me.skills.fos = 14;
        me.skills.thn = 10;
        me.skills.thb = 25;

        me.life = 99;
        me.base_hp = 19;
        me.exp = 190;
        me.infra = 4;

        me.calc_bonuses = _high_elf_calc_bonuses;
        me.get_flags = _high_elf_get_flags;
        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Hobbit
 ****************************************************************/
static power_info _hobbit_powers[] =
{
    { A_INT, {15, 10, 50, create_food_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _hobbit_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _hobbit_powers);
}
static void _hobbit_calc_bonuses(void)
{
    p_ptr->hold_life = TRUE;
}
static void _hobbit_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_HOLD_LIFE);
}
race_t *hobbit_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Hobbit";
        me.desc = "Hobbits, or Halflings, are very good at bows, throwing, and have good saving throws. "
                    "They also are very good at searching, disarming, perception, and stealth; so they "
                    "make excellent rogues, but prefer to be called burglars. They are much weaker than "
                    "humans, and no good at melee fighting. Halflings have fair infravision, so they can "
                    "detect warm creatures at a distance. They have a strong hold on their life force, "
                    "and are thus intrinsically resistant to life draining.";

        me.stats[A_STR] = -2;
        me.stats[A_INT] =  1;
        me.stats[A_WIS] =  1;
        me.stats[A_DEX] =  3;
        me.stats[A_CON] =  2;
        me.stats[A_CHR] =  1;
        
        me.skills.dis = 15;
        me.skills.dev = 12;
        me.skills.sav = 10;
        me.skills.stl = 5;
        me.skills.srh = 12;
        me.skills.fos = 15;
        me.skills.thn = -10;
        me.skills.thb = 20;

        me.life = 92;
        me.base_hp = 14;
        me.exp = 120;
        me.infra = 4;

        me.calc_bonuses = _hobbit_calc_bonuses;
        me.get_powers = _hobbit_get_powers;
        me.get_flags = _hobbit_get_flags;
        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Human
 ****************************************************************/
 static void _human_gain_level(int new_level)
{
    if (new_level >= 30)
    {
        if (p_ptr->demigod_power[0] < 0)
        {
            int idx = mut_gain_choice(mut_demigod_pred/*mut_human_pred*/);
            mut_lock(idx);
            p_ptr->demigod_power[0] = idx;
        }
        else if (!mut_present(p_ptr->demigod_power[0]))
        {
            mut_gain(p_ptr->demigod_power[0]);
            mut_lock(p_ptr->demigod_power[0]);
        }
    }
}

race_t *human_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Human";
        me.desc = "The human is the base character. All other races are compared to them. "
                    "Humans are average at everything and also tend to go up levels faster "
                    "than most other races because of their shorter life spans. No racial "
                    "adjustments or intrinsics occur to characters choosing human. However, "
                    "humans may choose a special talent at L30 that more than makes up for "
                    "their overall mediocrity.";
        
        me.stats[A_STR] =  0;
        me.stats[A_INT] =  0;
        me.stats[A_WIS] =  0;
        me.stats[A_DEX] =  0;
        me.stats[A_CON] =  0;
        me.stats[A_CHR] =  0;
        
        me.skills.dis = 0;
        me.skills.dev = 0;
        me.skills.sav = 0;
        me.skills.stl = 0;
        me.skills.srh = 0;
        me.skills.fos = 10;
        me.skills.thn = 0;
        me.skills.thb = 0;

        me.life = 100;
        me.base_hp = 20;
        me.exp = 100;
        me.infra = 0;

        me.gain_level = _human_gain_level;
        init = TRUE;
    }

    return &me;
}

/****************************************************************
 * Imp
 ****************************************************************/
static power_info _imp_powers[] =
{
    { A_INT, {9, 8, 50, imp_fire_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _imp_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _imp_powers);
}
static void _imp_calc_bonuses(void)
{
    res_add(RES_FIRE);
    if (p_ptr->lev >= 10) p_ptr->see_inv = TRUE;
}
static void _imp_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_FIRE);
    if (p_ptr->lev >= 10)
        add_flag(flgs, TR_SEE_INVIS);
}
race_t *imp_get_race_t(void)
{
    static race_t me = {0};
    static bool init = FALSE;

    if (!init)
    {
        me.name = "Imp";
        me.desc = "A demon-creature from the nether-world, naturally resistant to fire attacks, "
                    "and capable of learning fire bolt and fire ball attacks. They are little "
                    "loved by other races, but can perform fairly well in most professions. "
                    "As they advance levels, they gain the powers of See Invisible.";

        me.stats[A_STR] =  0;
        me.stats[A_INT] = -1;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] =  2;
        me.stats[A_CHR] = -1;
        
        me.skills.dis = -3;
        me.skills.dev = 2;
        me.skills.sav = -1;
        me.skills.stl = 1;
        me.skills.srh = -1;
        me.skills.fos = 10;
        me.skills.thn = 5;
        me.skills.thb = -5;

        me.life = 99;
        me.base_hp = 19;
        me.exp = 90;
        me.infra = 3;
        me.flags = RACE_IS_DEMON;

        me.calc_bonuses = _imp_calc_bonuses;
        me.get_powers = _imp_get_powers;
        me.get_flags = _imp_get_flags;
        init = TRUE;
    }

    return &me;
}

