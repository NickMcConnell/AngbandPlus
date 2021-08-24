#include "angband.h"

/****************************************************************
 * Klackon
 ****************************************************************/
static power_info _klackon_powers[] =
{
    { A_DEX, {9, 9, 50, spit_acid_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _klackon_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _klackon_powers);
}
static void _klackon_calc_bonuses(void)
{
    res_add(GF_CONF);
    res_add(GF_ACID);
    plr->pspeed += (plr->lev) / 10;
}
static void _klackon_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_CONF));
    add_flag(flgs, OF_RES_(GF_ACID));
    if (plr->lev > 9)
        add_flag(flgs, OF_SPEED);
}
plr_race_ptr klackon_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_KLACKON);
        me->name = "Klackon";
        me->desc = "Klackons are bizarre semi-intelligent ant-like insectoid creatures. "
                    "They make great fighters, but their mental abilities are severely limited. "
                    "Obedient and well-ordered, they are resistant to confusion. They are also very "
                    "nimble, and become faster as they advance levels. They are also very acidic, "
                    "inherently resisting acid, and capable of spitting acid at higher levels.";

        me->stats[A_STR] =  2;
        me->stats[A_INT] = -1;
        me->stats[A_WIS] = -1;
        me->stats[A_DEX] =  1;
        me->stats[A_CON] =  2;
        me->stats[A_CHR] =  1;

        me->skills.dis = 10;
        me->skills.dev = -2;
        me->skills.sav = 3;
        me->skills.stl = 0;
        me->skills.srh = -1;
        me->skills.fos = 10;
        me->skills.thn = 5;
        me->skills.thb = 3;

        me->life = 105;
        me->base_hp = 23;
        me->exp = 170;
        me->infra = 2;
        me->shop_adjust = 115;

        me->hooks.calc_bonuses = _klackon_calc_bonuses;
        me->hooks.get_powers = _klackon_get_powers;
        me->hooks.get_flags = _klackon_get_flags;
    }

    return me;
}

/****************************************************************
 * Kobold
 ****************************************************************/
static power_info _kobold_powers[] =
{
    { A_DEX, {12, 8, 50, poison_dart_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _kobold_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _kobold_powers);
}
static void _kobold_calc_bonuses(void)
{
    res_add(GF_POIS);
}
static void _kobold_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_POIS));
}
plr_race_ptr kobold_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_KOBOLD);
        me->name = "Kobold";
        me->desc = "Kobolds are a weak goblin race. They love poisoned weapons, and can learn to throw "
                    "poisoned darts (of which they carry an unlimited supply). They are also inherently "
                    "resistant to poison, although they are not one of the more powerful races.";

        me->stats[A_STR] =  1;
        me->stats[A_INT] = -1;
        me->stats[A_WIS] =  0;
        me->stats[A_DEX] =  1;
        me->stats[A_CON] =  0;
        me->stats[A_CHR] = -2;

        me->skills.dis = -2;
        me->skills.dev = -2;
        me->skills.sav = -1;
        me->skills.stl = -1;
        me->skills.srh =  1;
        me->skills.fos =  8;
        me->skills.thn = 10;
        me->skills.thb =  3;

        me->life = 98;
        me->base_hp = 19;
        me->exp = 90;
        me->infra = 3;
        me->shop_adjust = 120;

        me->hooks.calc_bonuses = _kobold_calc_bonuses;
        me->hooks.get_powers = _kobold_get_powers;
        me->hooks.get_flags = _kobold_get_flags;
    }

    return me;
}

/****************************************************************
 * Kutar
 ****************************************************************/
static void _kutar_expand_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Expand Horizontally");
        break;
    case SPELL_DESC:
        var_set_string(res, "Expand like a cat, gaining AC but becoming more susceptible to magical attacks.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_KUTAR_EXPAND, randint1(20) + 30);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static power_info _kutar_powers[] =
{
    { A_CHR, {20, 15, 70, _kutar_expand_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _kutar_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _kutar_powers);
}
static void _kutar_calc_bonuses(void)
{
    res_add(GF_CONF);
}
static void _kutar_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_CONF));
}
plr_race_ptr kutar_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_KUTAR);
        me->name = "Kutar";
        me->desc = "A Kutar is an expressionless animal-like living creature. The word 'kuta' means "
                    "'absentmindedly' or 'vacantly'. Their absentmindedness hurts their searching and "
                    "perception skills, but renders them resistant to being confused. Their unearthly "
                    "calmness and serenity make them among the most stealthy of any race. Kutars, "
                    "although expressionless, are beautiful and so have a high charisma. Members of "
                    "this race can learn to expand their body horizontally. This increases armour class, "
                    "but renders them vulnerable to magical attacks.";

        me->stats[A_STR] =  0;
        me->stats[A_INT] = -1;
        me->stats[A_WIS] = -1;
        me->stats[A_DEX] =  1;
        me->stats[A_CON] =  2;
        me->stats[A_CHR] =  2;

        me->skills.dis = -2;
        me->skills.dev = 3;
        me->skills.sav = 5;
        me->skills.stl = 5;
        me->skills.srh = -2;
        me->skills.fos = 6;
        me->skills.thn = 0;
        me->skills.thb = -3;

        me->life = 102;
        me->base_hp = 21;
        me->exp = 175;
        me->infra = 0;
        me->shop_adjust = 95;

        me->hooks.calc_bonuses = _kutar_calc_bonuses;
        me->hooks.get_powers = _kutar_get_powers;
        me->hooks.get_flags = _kutar_get_flags;
    }

    return me;
}

/****************************************************************
 * Mindflayer
 ****************************************************************/
static power_info _mindflayer_powers[] =
{
    { A_INT, {5, 3, 50, mind_blast_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _mindflayer_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _mindflayer_powers);
}
static void _mindflayer_calc_bonuses(void)
{
    plr->sustain_int = TRUE;
    plr->sustain_wis = TRUE;
    if (plr->lev >= 15) plr->see_inv++;
    if (plr->lev >= 30)
    {
        plr->telepathy = TRUE;
        plr->wizard_sight = TRUE;
    }
}
static void _mindflayer_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SUST_INT);
    add_flag(flgs, OF_SUST_WIS);
    if (plr->lev >= 15)
        add_flag(flgs, OF_SEE_INVIS);
    if (plr->lev >= 30)
        add_flag(flgs, OF_TELEPATHY);
}
plr_race_ptr mindflayer_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_MIND_FLAYER);
        me->name = "Mindflayer";
        me->desc = "Mindflayers are a secretive and mysterious ancient race. Their civilization may well "
                    "be older than any other on our planet, and their intelligence and wisdom are "
                    "naturally sustained, and are so great that they enable Mindflayers to become more "
                    "powerful spellcasters than any other race, even if their physical attributes are "
                    "a good deal less admirable. As they advance levels, they gain the powers of "
                    "See Invisible and Telepathy.";

        me->stats[A_STR] = -3;
        me->stats[A_INT] =  4;
        me->stats[A_WIS] =  4;
        me->stats[A_DEX] =  0;
        me->stats[A_CON] = -2;
        me->stats[A_CHR] = -1;

        me->skills.dis = 10;
        me->skills.dev = 11;
        me->skills.sav = 9;
        me->skills.stl = 2;
        me->skills.srh = 5;
        me->skills.fos = 12;
        me->skills.thn = -10;
        me->skills.thb = -5;

        me->life = 97;
        me->base_hp = 18;
        me->exp = 150;
        me->infra = 4;
        me->shop_adjust = 115;

        me->hooks.calc_bonuses = _mindflayer_calc_bonuses;
        me->hooks.get_powers = _mindflayer_get_powers;
        me->hooks.get_flags = _mindflayer_get_flags;
    }

    return me;
}


/****************************************************************
 * Nibelung
 ****************************************************************/
static power_info _nibelung_powers[] =
{
    { A_WIS, {10, 5, 50, detect_doors_stairs_traps_spell}},
    { A_CHR, {10, 5, 50, detect_treasure_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _nibelung_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _nibelung_powers);
}
static void _nibelung_calc_bonuses(void)
{
    res_add(GF_DISEN);
    res_add(GF_DARK);
}
static void _nibelung_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_DISEN));
    add_flag(flgs, OF_RES_(GF_DARK));
}
plr_race_ptr nibelung_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_NIBELUNG);
        me->name = "Nibelung";
        me->desc = "The hated and persecuted race of nocturnal dwarves, these cave-dwellers are "
                    "not much bothered by darkness. Their natural inclination to magical items "
                    "has made them resistant to effects which could disenchant magical energy.";

        me->stats[A_STR] =  1;
        me->stats[A_INT] = -1;
        me->stats[A_WIS] =  2;
        me->stats[A_DEX] =  0;
        me->stats[A_CON] =  2;
        me->stats[A_CHR] = -2;

        me->skills.dis =  3;
        me->skills.dev =  3;
        me->skills.sav =  6;
        me->skills.stl =  1;
        me->skills.srh =  5;
        me->skills.fos = 10;
        me->skills.thn =  9;
        me->skills.thb =  0;

        me->life = 103;
        me->base_hp = 22;
        me->exp = 165;
        me->infra = 5;
        me->shop_adjust = 115;

        me->hooks.calc_bonuses = _nibelung_calc_bonuses;
        me->hooks.get_powers = _nibelung_get_powers;
        me->hooks.get_flags = _nibelung_get_flags;
    }

    return me;
}

/****************************************************************
 * Shadow-Fairy
 ****************************************************************/
static void _shadow_fairy_calc_bonuses(void)
{
    plr->levitation = TRUE;
    plr->fairy_stealth = TRUE;
    res_add_vuln(GF_LIGHT);
}
static void _shadow_fairy_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_LEVITATION);
    add_flag(flgs, OF_VULN_(GF_LIGHT));
}
plr_race_ptr shadow_fairy_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_SHADOW_FAIRY);
        me->name = "Shadow-Fairy";
        me->desc = "Shadow Fairies are one of the several fairy races. They have wings, and can fly over "
                    "traps that may open up beneath them. Shadow Fairies must beware of sunlight, as "
                    "they are vulnerable to bright light. They are physically weak, but have advantages "
                    "in using magic and are amazingly stealthy. Shadow Fairies have a wonderful advantage "
                    "in that they never aggravate monsters (If their equipment normally aggravates monsters, "
                    "they only suffer a penalty to stealth, but if they aggravate by their personality "
                    "itself, the advantage will be lost).";

        me->stats[A_STR] = -2;
        me->stats[A_INT] =  2;
        me->stats[A_WIS] =  2;
        me->stats[A_DEX] =  1;
        me->stats[A_CON] = -1;
        me->stats[A_CHR] = -3;

        me->skills.dis =  7;
        me->skills.dev =  6;
        me->skills.sav =  0;
        me->skills.stl =  6;
        me->skills.srh = 12;
        me->skills.fos = 15;
        me->skills.thn =-10;
        me->skills.thb = -3;

        me->life = 91;
        me->base_hp = 13;
        me->exp = 140;
        me->infra = 4;
        me->shop_adjust = 110;

        me->hooks.calc_bonuses = _shadow_fairy_calc_bonuses;
        me->hooks.get_flags = _shadow_fairy_get_flags;
    }

    return me;
}


/****************************************************************
 * Skeleton
 ****************************************************************/
static power_info _skeleton_powers[] =
{
    { A_WIS, {30, 30, 70, restore_life_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _skeleton_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _skeleton_powers);
}
static void _skeleton_calc_bonuses(void)
{
    res_add(GF_SHARDS);
    plr->hold_life++;
    plr->see_inv++;
    res_add(GF_POIS);
    if (plr->lev >= 10) res_add(GF_COLD);
    plr->see_nocto = MAX(plr->see_nocto, 2 + plr->lev/13);
}
static void _skeleton_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SEE_INVIS);
    add_flag(flgs, OF_RES_(GF_SHARDS));
    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_RES_(GF_POIS));
    if (plr->lev >= 10)
        add_flag(flgs, OF_RES_(GF_COLD));
}
static void _skeleton_birth(void)
{
    plr_birth_obj_aux(TV_STAFF, EFFECT_NOTHING, 1);
    plr_birth_light();
}
plr_race_ptr skeleton_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_SKELETON);
        me->name = "Skeleton";
        me->desc = "There are two types of skeletons: the ordinary, warrior-like skeletons, and the "
                    "spell-using skeletons, which are also called liches. As undead beings, skeletons "
                    "need to worry very little about poison or attacks that can drain life. They do "
                    "not really use eyes for perceiving things, and are thus not fooled by invisibility. "
                    "Being undead, skeletons can also see a short distance without the need for light. "
                    "Their bones are resistant to sharp shrapnel, and they will quickly become resistant "
                    "to cold. Although the magical effects of these will affect the skeleton even "
                    "without entering the skeleton's (non-existent) belly, the potion or food itself "
                    "will fall through the skeleton's jaws, giving no nutritional benefit. They can "
                    "absorb mana from staves and wands as their energy source.";

        me->stats[A_STR] =  0;
        me->stats[A_INT] =  1;
        me->stats[A_WIS] = -2;
        me->stats[A_DEX] =  0;
        me->stats[A_CON] =  1;
        me->stats[A_CHR] =  1;

        me->skills.dis = -5;
        me->skills.dev = 0;
        me->skills.sav = 3;
        me->skills.stl = -1;
        me->skills.srh = -1;
        me->skills.fos = 8;
        me->skills.thn = 10;
        me->skills.thb = 0;

        me->life = 100;
        me->base_hp = 21;
        me->exp = 115;
        me->infra = 2;
        me->flags = RACE_IS_NONLIVING | RACE_IS_UNDEAD;
        me->shop_adjust = 125;

        me->hooks.birth = _skeleton_birth;
        me->hooks.calc_bonuses = _skeleton_calc_bonuses;
        me->hooks.get_powers = _skeleton_get_powers;
        me->hooks.get_flags = _skeleton_get_flags;
    }

    return me;
}

/****************************************************************
 * Snotling (A Joke Race)
 ****************************************************************/
static void _devour_flesh_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Devour Flesh");
        break;
    case SPELL_DESC:
        var_set_string(res, "Devour flesh (yours) in order to fill your belly.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!get_check("It might hurt a bit. Are you sure?")) return;
        msg_print("You devour your own flesh!");
        set_food(PY_FOOD_MAX - 1);
        if (!plr->no_cut) plr_tim_add(T_CUT, CUT_SEVERE);
        take_hit(DAMAGE_USELIFE, plr->mhp / 3, "devouring your own flesh");
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static power_info _snotling_powers[] =
{
    { A_CHR, {1, 0, 0, _devour_flesh_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _snotling_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _snotling_powers);
}
static void _snotling_calc_bonuses(void)
{
}
static void _snotling_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
}
static void _snotling_birth(void)
{
    plr_birth_obj_aux(TV_FOOD, SV_FOOD_CURE_SERIOUS, randint1(3));
    plr_birth_food();
    plr_birth_light();
}
plr_race_ptr snotling_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_SNOTLING);
        me->name = "Snotling";
        me->desc = "Snotlings are greenskins, and are cousins of Goblins and Orcs, smaller than the "
                    "former and dumber than the latter, often used by them as cannon fodder, food "
                    "or even cannon missiles. They are on the lowest rung of greenskin society and "
                    "are bullied by all. Snotlings often wield mushrooms or sticks into battle.";

        me->stats[A_STR] = -2;
        me->stats[A_INT] = -2;
        me->stats[A_WIS] = -2;
        me->stats[A_DEX] = -2;
        me->stats[A_CON] = -2;
        me->stats[A_CHR] = -5;

        me->skills.dis = -3;
        me->skills.dev = -2;
        me->skills.sav = -2;
        me->skills.stl = 2;
        me->skills.srh = 0;
        me->skills.fos = 7;
        me->skills.thn = -10;
        me->skills.thb = -5;

        me->life = 85;
        me->base_hp = 10;
        me->exp = 45;
        me->infra = 2;
        me->shop_adjust = 125;

        me->hooks.birth = _snotling_birth;
        me->hooks.calc_bonuses = _snotling_calc_bonuses;
        me->hooks.get_powers = _snotling_get_powers;
        me->hooks.get_flags = _snotling_get_flags;
    }

    return me;
}

/****************************************************************
 * Spectre
 ****************************************************************/
static power_info _spectre_powers[] =
{
    { A_INT, {4, 6, 50, scare_monster_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _spectre_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _spectre_powers);
}
static void _spectre_calc_bonuses(void)
{
    plr->levitation = TRUE;
    res_add(GF_NETHER);
    plr->hold_life++;
    plr->see_inv++;
    res_add(GF_POIS);
    plr->slow_digest = TRUE;
    res_add(GF_COLD);
    plr->pass_wall = TRUE;
    plr->see_nocto = MAX(plr->see_nocto, 2 + plr->lev/13);
}
static void _spectre_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_LEVITATION);
    add_flag(flgs, OF_RES_(GF_COLD));
    add_flag(flgs, OF_SEE_INVIS);
    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_RES_(GF_NETHER));
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_SLOW_DIGEST);
}
static void _spectre_birth(void)
{
    plr_birth_obj_aux(TV_STAFF, EFFECT_NOTHING, 1);
    plr_birth_light();
}
plr_race_ptr spectre_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_SPECTRE);
        me->name = "Spectre";
        me->desc = "Another powerful undead creature: the Spectre is a ghastly apparition, surrounded by "
                    "an unearthly green glow. They exist only partially on our plane of existence: "
                    "half-corporeal, they can pass through walls, although the density of the wall "
                    "will hurt them in the process of doing this. As undead, they have a firm hold "
                    "on their life force, see invisible, and resist poison and cold. They also resist "
                    "nether. Spectres make superb spellcasters, but their physical form is very weak. "
                    "They gain very little nutrition from the food of mortals, but can absorb mana "
                    "from staves and wands as their energy source. "
                    "Being undead, spectres can also see a short distance without the need for light.";

        me->stats[A_STR] = -5;
        me->stats[A_INT] =  4;
        me->stats[A_WIS] =  2;
        me->stats[A_DEX] =  2;
        me->stats[A_CON] = -2;
        me->stats[A_CHR] = -3;

        me->skills.dis = 10;
        me->skills.dev = 10;
        me->skills.sav = 12;
        me->skills.stl =  5;
        me->skills.srh =  5;
        me->skills.fos = 14;
        me->skills.thn =-15;
        me->skills.thb = -5;

        me->life = 90;
        me->base_hp = 13;
        me->exp = 250;
        me->infra = 5;
        me->flags = RACE_IS_NONLIVING | RACE_IS_UNDEAD;
        me->shop_adjust = 135;

        me->hooks.birth = _spectre_birth;
        me->hooks.calc_bonuses = _spectre_calc_bonuses;
        me->hooks.get_powers = _spectre_get_powers;
        me->hooks.get_flags = _spectre_get_flags;
    }

    return me;
}

/****************************************************************
 * Sprite
 ****************************************************************/
static power_info _sprite_powers[] =
{
    { A_INT, {12, 12, 50, sleeping_dust_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _sprite_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _sprite_powers);
}
static void _sprite_calc_bonuses(void)
{
    plr->levitation = TRUE;
    res_add(GF_LIGHT);
    plr->pspeed += (plr->lev) / 10;
}
static void _sprite_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_LIGHT));
    add_flag(flgs, OF_LEVITATION);
    if (plr->lev >= 10)
        add_flag(flgs, OF_SPEED);
}
plr_race_ptr sprite_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_SPRITE);
        me->name = "Sprite";
        me->desc = "One of the several fairy races, Sprites are very small. They have tiny wings and can "
                    "fly over traps that may open up beneath them. They enjoy sunlight intensely, and "
                    "need worry little about light based attacks. Although physically among the weakest "
                    "races, Sprites are very talented in magic, and can become highly skilled wizards. "
                    "Sprites have the special power of spraying Sleeping Dust, and at higher levels they "
                    "learn to fly faster.";

        me->stats[A_STR] = -4;
        me->stats[A_INT] =  3;
        me->stats[A_WIS] =  3;
        me->stats[A_DEX] =  3;
        me->stats[A_CON] = -2;
        me->stats[A_CHR] = -2;

        me->skills.dis = 10;
        me->skills.dev =  6;
        me->skills.sav =  6;
        me->skills.stl =  4;
        me->skills.srh = 10;
        me->skills.fos = 10;
        me->skills.thn =-12;
        me->skills.thb =  0;

        me->life = 92;
        me->base_hp = 14;
        me->exp = 135;
        me->infra = 4;
        me->shop_adjust = 90;

        me->hooks.calc_bonuses = _sprite_calc_bonuses;
        me->hooks.get_powers = _sprite_get_powers;
        me->hooks.get_flags = _sprite_get_flags;
    }

    return me;
}

/****************************************************************
 * Tengu
 ****************************************************************/
static power_info _tengu_powers[] =
{
    { A_DEX, {1,  2, 50, phase_door_spell} },
    { A_DEX, {15, 8, 60, teleport_spell} },
    { -1, {-1, -1, -1, NULL} }
};
static int _tengu_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _tengu_powers);
}
static void _tengu_calc_bonuses(void)
{
    plr->pspeed += 2;
    if (plr->lev >= 20)
        res_add(GF_TELEPORT);
}
static void _tengu_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SPEED);
    if (plr->lev >= 20)
        add_flag(flgs, OF_RES_(GF_TELEPORT));
}
plr_race_ptr tengu_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_TENGU);
        me->name = "Tengu";
        me->desc = "Tengu are fast-moving demons that blink quickly in and out of existence. "
                   "Their mastery of teleportation is unmatched. Eventually, they will even "
                   "resist being teleported by their enemies. Being demons, they are rather "
                   "unsuited for the priestly professions.";

        me->stats[A_STR] =  0;
        me->stats[A_INT] =  1;
        me->stats[A_WIS] = -3;
        me->stats[A_DEX] =  2;
        me->stats[A_CON] =  0;
        me->stats[A_CHR] = -1;

        me->skills.dis = 2;
        me->skills.dev = 3;
        me->skills.sav = -2;
        me->skills.stl = 2;
        me->skills.srh = -1;
        me->skills.fos = 10;
        me->skills.thn = 0;
        me->skills.thb = 5;

        me->life = 99;
        me->base_hp = 19;
        me->exp = 140;
        me->infra = 3;
        me->flags = RACE_IS_DEMON;
        me->shop_adjust = 120;

        me->hooks.calc_bonuses = _tengu_calc_bonuses;
        me->hooks.get_powers = _tengu_get_powers;
        me->hooks.get_flags = _tengu_get_flags;
    }

    return me;
}

/****************************************************************
 * Vampire
 ****************************************************************/
static power_info _vampire_powers[] =
{
    { A_CON, {2, 1, 60, vampirism_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _vampire_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _vampire_powers);
}
static void _vampire_calc_bonuses(void)
{
    res_add(GF_DARK);
    res_add(GF_NETHER);
    res_add(GF_COLD);
    res_add(GF_POIS);
    res_add_vuln(GF_LIGHT);
    plr->hold_life++;
    plr->see_nocto = MAX(plr->see_nocto, 2 + plr->lev/13);
}
static void _vampire_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_VULN_(GF_LIGHT));

    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_RES_(GF_DARK));
    add_flag(flgs, OF_RES_(GF_NETHER));
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_RES_(GF_COLD));
}
static void _vampire_birth(void)
{
    plr_birth_obj_aux(TV_SCROLL, SV_SCROLL_DARKNESS, rand_range(2, 5));
}
plr_race_ptr vampire_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_VAMPIRE);
        me->name = "Vampire";
        me->desc = "One of the mightier undead creatures, the Vampire is an awe-inspiring sight. Yet this "
                    "dread creature has a serious weakness: the bright rays of sun are its bane, and it "
                    "will need to flee the surface to the deep recesses of earth until the sun finally "
                    "sets. Darkness, on the other hand, only makes the Vampire stronger. As undead, the "
                    "Vampire has a firm hold on its life force, and resists nether attacks. The Vampire "
                    "also resists cold and poison based attacks. It is, however, susceptible to its "
                    "perpetual hunger for fresh blood, which can only be satiated by sucking the blood "
                    "from a nearby monster. "
                    "Being undead, vampires can also see a short distance without the need for light.";

        me->stats[A_STR] =  3;
        me->stats[A_INT] =  3;
        me->stats[A_WIS] = -1;
        me->stats[A_DEX] = -1;
        me->stats[A_CON] =  1;
        me->stats[A_CHR] =  2;

        me->skills.dis = 4;
        me->skills.dev = 5;
        me->skills.sav = 6;
        me->skills.stl = 4;
        me->skills.srh = 1;
        me->skills.fos = 8;
        me->skills.thn = 5;
        me->skills.thb = 0;

        me->life = 102;
        me->base_hp = 22;
        me->exp = 200;
        me->infra = 5;
        me->flags = RACE_IS_NONLIVING | RACE_IS_UNDEAD;
        me->shop_adjust = 130;

        me->hooks.birth = _vampire_birth;
        me->hooks.calc_bonuses = _vampire_calc_bonuses;
        me->hooks.get_powers = _vampire_get_powers;
        me->hooks.get_flags = _vampire_get_flags;
    }

    return me;
}

/****************************************************************
 * Water-Elf
 ****************************************************************/
static void _water_elf_calc_bonuses(void)
{
    res_add(GF_WATER);
}
static void _water_elf_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_WATER));
}
plr_race_ptr water_elf_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_WATER_ELF);
        me->name = "Water-Elf";
        me->desc = "Water-Elves inhabit the lakes, rivers and seas of the world. They move quickly "
                    "in their native element and can never drown. They also resist water based attacks. "
                    "Like all elves, they excel with both archery and magical devices.";

        me->stats[A_STR] =  0;
        me->stats[A_INT] =  1;
        me->stats[A_WIS] =  1;
        me->stats[A_DEX] =  2;
        me->stats[A_CON] = -1;
        me->stats[A_CHR] =  1;

        me->skills.dis = 5;
        me->skills.dev = 4;
        me->skills.sav = 4;
        me->skills.stl = 3;
        me->skills.srh = 8;
        me->skills.fos = 12;
        me->skills.thn = 0;
        me->skills.thb = 9;

        me->life = 98;
        me->base_hp = 18;
        me->exp = 125;
        me->infra = 3;
        me->shop_adjust = 95;
        me->hooks.calc_bonuses = _water_elf_calc_bonuses;
        me->hooks.get_flags = _water_elf_get_flags;
    }

    return me;
}

/****************************************************************
 * Wood-Elf
 ****************************************************************/
static power_info _wood_elf_powers[] =
{
    { A_WIS, {20, 15, 50, nature_awareness_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _wood_elf_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _wood_elf_powers);
}
static void _wood_elf_calc_bonuses(void)
{
    plr->pass_tree = TRUE;
}
plr_race_ptr wood_elf_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_WOOD_ELF);
        me->name = "Wood-Elf";
        me->desc = "Wood-Elves are the most common of elves. They prefer the seclusion of thick "
                    "forests and are unhampered when moving through dense foliage. Their skills "
                    "with tracking and bow are unsurpassed, and as they advance they gain the "
                    "power of Nature Awareness.";

        me->stats[A_STR] = -1;
        me->stats[A_INT] =  1;
        me->stats[A_WIS] =  2;
        me->stats[A_DEX] =  1;
        me->stats[A_CON] = -1;
        me->stats[A_CHR] =  1;

        me->skills.dis = 5;
        me->skills.dev = 4;
        me->skills.sav = 4;
        me->skills.stl = 3;
        me->skills.srh = 8;
        me->skills.fos = 12;
        me->skills.thn = -5;
        me->skills.thb = 12;

        me->life = 97;
        me->base_hp = 16;
        me->exp = 125;
        me->infra = 3;
        me->shop_adjust = 95;

        me->hooks.get_powers = _wood_elf_get_powers;
        me->hooks.calc_bonuses = _wood_elf_calc_bonuses;
    }

    return me;
}

/****************************************************************
 * Yeek
 ****************************************************************/
static power_info _yeek_powers[] =
{
    { A_WIS, {15, 15, 50, scare_monster_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _yeek_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _yeek_powers);
}
static void _yeek_calc_bonuses(void)
{
    res_add(GF_ACID);
    if (plr->lev >= 20)
        res_add_immune(GF_ACID);
}
static void _yeek_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_ACID));
    if (plr->lev >= 20)
        add_flag(flgs, OF_IM_(GF_ACID));
}
plr_race_ptr yeek_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_YEEK);
        me->name = "Yeek";
        me->desc = "Yeeks are among the most pathetic creatures. Fortunately, their horrible screams "
                    "can scare away less confident foes, and their skin becomes more and more resistant "
                    "to acid, as they gain experience. But having said that, even a mediocre monster "
                    "can wipe the proverbial floor with an unwary Yeek.";

        me->stats[A_STR] = -2;
        me->stats[A_INT] =  1;
        me->stats[A_WIS] = -2;
        me->stats[A_DEX] =  1;
        me->stats[A_CON] = -2;
        me->stats[A_CHR] = -4;

        me->skills.dis = 2;
        me->skills.dev = 3;
        me->skills.sav = 6;
        me->skills.stl = 3;
        me->skills.srh = 5;
        me->skills.fos = 15;
        me->skills.thn = -5;
        me->skills.thb = -3;

        me->life = 92;
        me->base_hp = 14;
        me->exp = 70;
        me->infra = 2;
        me->shop_adjust = 105;

        me->hooks.calc_bonuses = _yeek_calc_bonuses;
        me->hooks.get_powers = _yeek_get_powers;
        me->hooks.get_flags = _yeek_get_flags;
    }

    return me;
}

/****************************************************************
 * Zombie
 ****************************************************************/
static power_info _zombie_powers[] =
{
    { A_WIS, {30, 30, 70, restore_life_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static int _zombie_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _zombie_powers);
}
static void _zombie_calc_bonuses(void)
{
    res_add(GF_NETHER);
    plr->hold_life++;
    plr->see_inv++;
    res_add(GF_POIS);
    plr->slow_digest = TRUE;
    if (plr->lev >= 5) res_add(GF_COLD);
    plr->see_nocto = MAX(plr->see_nocto, 2 + plr->lev/13);
}
static void _zombie_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SEE_INVIS);
    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_RES_(GF_NETHER));
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_SLOW_DIGEST);
    if (plr->lev >= 5)
        add_flag(flgs, OF_RES_(GF_COLD));
}
static void _zombie_birth(void)
{
    plr_birth_obj_aux(TV_STAFF, EFFECT_NOTHING, 1);
    plr_birth_light();
}
plr_race_ptr zombie_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_ZOMBIE);
        me->name = "Zombie";
        me->desc = "Zombies are undead horrors, resistant to life draining and the forces "
            "of the netherworld. The grave is cold but this does not bother the undead and "
            "poison scarcely affects the unliving. Zombies gain little nutrition from "
            "ordinary food. Instead, they must absorb mana from magical devices to maintain "
            "their undead existence. "
            "Being undead, zombies can also see a short distance without the need for light.";

        me->stats[A_STR] =  2;
        me->stats[A_INT] = -6;
        me->stats[A_WIS] = -6;
        me->stats[A_DEX] =  1;
        me->stats[A_CON] =  4;
        me->stats[A_CHR] = -3;

        me->skills.dis = -5;
        me->skills.dev = -5;
        me->skills.sav = 5;
        me->skills.stl = -1;
        me->skills.srh = -1;
        me->skills.fos = 5;
        me->skills.thn = 15;
        me->skills.thb = 0;

        me->life = 108;
        me->base_hp = 24;
        me->exp = 180;
        me->infra = 2;
        me->flags = RACE_IS_NONLIVING | RACE_IS_UNDEAD;
        me->shop_adjust = 140;

        me->hooks.birth = _zombie_birth;
        me->hooks.calc_bonuses = _zombie_calc_bonuses;
        me->hooks.get_powers = _zombie_get_powers;
        me->hooks.get_flags = _zombie_get_flags;
    }

    return me;
}

