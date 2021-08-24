#include "angband.h"

#include <assert.h>

static cptr _desc =
    "Demons are powerful servants of evil and come in many forms. Being monsters, they "
    "may not choose a normal class. Instead, they rely on their devilish powers or their "
    "brutish strength to survive.\n \n"
    "The various demonic races include the Balrog, powerful demons of fire; the Sevants "
    "of Khorne, mighty warriors of destruction; the Tanar'ri, weaker demons whose ultimate "
    "form has three sets of arms, but prefers to fight naked; and Cyberdemons, whose firepower "
    "is unsurpassable.\n \n"
    "All demon races cannot eat normal food, but must feast upon the remains of their human "
    "enemies. They are unaffected by the Eldritch Horror.";

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "devilish power";
        me.which_stat = A_INT;
        me.encumbrance.max_wgt = 750;
        me.encumbrance.weapon_pct = 0;
        me.encumbrance.enc_wgt = 800;
        me.options = CASTER_GAIN_SKILL;
        init = TRUE;
    }
    return &me;
}

static void _demon_birth(void)
{
    int i, ct = rand_range(3, 4);
    mon_alloc_push_filter(monster_hook_human);
    for (i = 0; i < ct; i++)
    {
        object_type forge = {0};
        object_prep(&forge, lookup_kind(TV_CORPSE, SV_CORPSE));
        forge.race_id = mon_alloc_choose(2)->id;
        plr_birth_obj(&forge);
    }
    mon_alloc_pop_filter();
    plr_birth_light();
}

/******************************************************************************
 *                                20            30            40
 * Servant of Khorne: Bloodletter -> Fleshhound -> Juggernaut -> Bloodthirster
 ******************************************************************************/
static void _khorne_birth(void)
{
    object_type    forge;

    plr_mon_race_set("U.bloodletter");

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_d = 6;
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_SWORD, SV_BLADE_OF_CHAOS));
    forge.name2 = EGO_WEAPON_SHARPNESS;
    forge.pval = 1;
    forge.to_h = 0;
    forge.to_d = 0;
    forge.ds = 6;
    forge.weight = 220;
    add_flag(forge.flags, OF_VORPAL);
    plr_birth_obj(&forge);

    _demon_birth();
}

static void _khorne_calc_innate_bonuses(mon_blow_ptr blow)
{
    if (plr_mon_race_is_("C.fleshhound"))
    {
        plr->innate_attack_info.blows_calc.wgt = 100;
        plr->innate_attack_info.blows_calc.mul = 40;
        if (blow->method == RBM_CLAW)
        {
            plr->innate_attack_info.blows_calc.max = 200;
            plr_calc_blows_innate(blow);
        }
        if (blow->method == RBM_BITE)
        {
            plr->innate_attack_info.blows_calc.max = 300;
            plr_calc_blows_innate(blow);
        }
    }
    else if (plr_mon_race_is_("g.juggernaut"))
    {
        plr->innate_attack_info.blows_calc.wgt = 200;
        plr->innate_attack_info.blows_calc.mul = 50;
        if (blow->method == RBM_BUTT)
        {
            plr->innate_attack_info.blows_calc.max = 400;
            plr_calc_blows_innate(blow);
        }
        if (blow->method == RBM_CRUSH)
        {
            plr->innate_attack_info.blows_calc.max = 200;
            plr_calc_blows_innate(blow);
        }
    }
}
static void _khorne_calc_innate_attacks(void)
{
    if (plr_mon_race_is_("C.fleshhound"))
    {
        int to_d = plr_prorata_level(15);
        int power = plr_prorata_level(90);
        mon_blow_ptr blow;

        /* Claws */
        blow = mon_blow_alloc(RBM_CLAW);
        blow->power = power;
        mon_blow_push_effect(blow, RBE_HURT, dice_create(5, 2, to_d));
        _khorne_calc_innate_bonuses(blow);
        vec_add(plr->innate_blows, blow);

        /* Bite */
        blow = mon_blow_alloc(RBM_BITE);
        blow->power = power;
        mon_blow_push_effect(blow, RBE_HURT, dice_create(10, 2, to_d));
        _khorne_calc_innate_bonuses(blow);
        vec_add(plr->innate_blows, blow);
    }
    else if (plr_mon_race_is_("g.juggernaut"))
    {
        int to_d = plr_prorata_level(15);
        int power = plr_prorata_level(90);
        mon_blow_ptr blow;

        /* Butt */
        blow = mon_blow_alloc(RBM_BUTT);
        blow->power = power;
        mon_blow_push_effect(blow, RBE_HURT, dice_create(8, 6, to_d));
        _khorne_calc_innate_bonuses(blow);
        vec_add(plr->innate_blows, blow);

        /* Crush */
        blow = mon_blow_alloc(RBM_CRUSH);
        blow->power = power;
        mon_blow_push_effect(blow, RBE_HURT, dice_create(10, 6, to_d));
        _khorne_calc_innate_bonuses(blow);
        vec_add(plr->innate_blows, blow);
    }
}

static void _khorne_calc_bonuses(void)
{
    plr->align -= 200;

    res_add(GF_FIRE);
    res_add(GF_NETHER);

    plr->slow_digest = TRUE;
    plr->hold_life++;
    plr->no_eldritch = TRUE;

    if (plr_mon_race_is_("U.bloodletter"))
    {
        plr->regen += 100;
        res_add(GF_COLD);
        res_add(GF_POIS);
        res_add(GF_CHAOS);
    }
    else if (plr_mon_race_is_("C.fleshhound"))
    {
        plr->pspeed += 2;
        res_add(GF_CONF);
        res_add(GF_NEXUS);
        res_add(GF_DISEN);
    }
    else if (plr_mon_race_is_("g.juggernaut"))
    {
        plr->pspeed += 4;
        plr->to_a += 100;
        plr->dis_to_a += 100;
        plr->reflect = TRUE;
        plr->free_act++;
        plr->see_inv++;

        res_add(GF_COLD);
        res_add(GF_ELEC);
        res_add(GF_POIS);
        res_add(GF_CONF);
        res_add(GF_FEAR);
    }
    else if (plr_mon_race_is_("U.bloodthirster"))
    {
        plr->pspeed += 6;
        plr->to_a += 50;
        plr->dis_to_a += 50;
        plr->regen += 150;
        plr->levitation = TRUE;
        plr->free_act++;
        plr->see_inv++;
        res_add(GF_ACID);
        res_add(GF_COLD);
        res_add(GF_POIS);
        res_add(GF_CONF);
        res_add(GF_NEXUS);
        res_add(GF_TELEPORT);
    }
}

static void _khorne_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_FIRE));
    add_flag(flgs, OF_RES_(GF_NETHER));

    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_SLOW_DIGEST);

    if (plr_mon_race_is_("U.bloodletter"))
    {
        add_flag(flgs, OF_REGEN);
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_RES_(GF_POIS));
        add_flag(flgs, OF_RES_(GF_CHAOS));
    }
    else if (plr_mon_race_is_("C.fleshhound"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_RES_(GF_NEXUS));
        add_flag(flgs, OF_RES_(GF_DISEN));
    }
    else if (plr_mon_race_is_("g.juggernaut"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_REFLECT);
        add_flag(flgs, OF_FREE_ACT);
        add_flag(flgs, OF_SEE_INVIS);

        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_RES_(GF_ELEC));
        add_flag(flgs, OF_RES_(GF_POIS));
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_RES_(GF_FEAR));
    }
    else if (plr_mon_race_is_("U.bloodthirster"))
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_REGEN);
        add_flag(flgs, OF_LEVITATION);
        add_flag(flgs, OF_FREE_ACT);
        add_flag(flgs, OF_SEE_INVIS);
        add_flag(flgs, OF_RES_(GF_ACID));
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_RES_(GF_POIS));
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_RES_(GF_NEXUS));
    }
}

static void _khorne_gain_level(int new_level)
{
    if (plr_mon_race_is_("U.bloodletter") && new_level >= 20)
        plr_mon_race_evolve("C.fleshhound");
    if (plr_mon_race_is_("C.fleshhound") && new_level >= 30)
        plr_mon_race_evolve("g.juggernaut");
    if (plr_mon_race_is_("g.juggernaut") && new_level >= 40)
    {
        object_type forge;
        plr_mon_race_evolve("U.bloodthirster");
        object_prep(&forge, lookup_kind(TV_SWORD, SV_BLADE_OF_CHAOS));
        forge.name2 = EGO_WEAPON_DEATH; /* Prevent ?Artifact or ?WeaponBranding */
        forge.dd = 50;
        forge.ds = 1;
        forge.weight = 500;
        forge.to_h = 15;
        forge.to_d = 15;
        plr_birth_obj(&forge);
        plr->update |= PU_BONUS;
    }
}

static plr_race_ptr _khorne_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[4] =  {"Bloodletter of Khorne", "Fleshhound of Khorne", "Juggernaut of Khorne", "Bloodthirster"};
    int           rank = 0;

    if (plr->lev >= 20) rank++;
    if (plr->lev >= 30) rank++;
    if (plr->lev >= 40) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  20,  40,  -1,  13,   7,  70,  30};
    skills_t xs = { 60,  40,  50,   0,   0,   0, 160,  35};

        me = plr_race_alloc_aux(RACE_MON_DEMON, DEMON_KHORNE);
        me->subdesc = "Khorne's servants come in many forms and are powerful forces of melee. They know nothing "
        "save melee, and strike at all that dare oppose the will of their master. As they gain "
        "experience, Khorne rewards his servants with new and more powerful forms.";

        me->skills = bs;
        me->extra_skills = xs;

        me->exp = 250;
        me->infra = 5;
        me->base_hp = 42;

        me->hooks.birth = _khorne_birth;
        me->hooks.calc_innate_attacks = _khorne_calc_innate_attacks;
        me->hooks.calc_innate_bonuses = _khorne_calc_innate_bonuses;
        me->hooks.calc_bonuses = _khorne_calc_bonuses;
        me->hooks.get_flags = _khorne_get_flags;
        me->hooks.gain_level = _khorne_gain_level;

        me->pseudo_class_id = CLASS_WARRIOR;
        me->boss_r_idx = mon_race_parse("U.Demogorgon")->id;
    }

    if (spoiler_hack || birth_hack)
    {
        me->subname = "Servant of Khorne";
        rank = 0;
    }
    else
        me->subname = titles[rank];
    me->stats[A_STR] =  3 + rank;
    me->stats[A_INT] = -5;
    me->stats[A_WIS] = -5;
    me->stats[A_DEX] =  0 + rank/3;
    me->stats[A_CON] =  2 + rank;
    me->stats[A_CHR] =  rank/3;
    me->life = 100 + 5*rank;

    me->equip_template = plr_equip_template();

    return me;
}

/******************************************************************************
 *                 10        20       30              40
 * Marilith: Manes -> Quasit -> Bodak -> Death Quasit -> Marilith
 ******************************************************************************/
static spell_info _marilith_spells[] = {
    { 10,  3, 30, phase_door_spell},
    { 12,  5, 35, scare_spell},
    { 15,  7, 35, teleport_spell},
    { 17, 10, 35, slow_spell},
    { 20,  7, 40, fire_bolt_spell},
    { 22,  9, 40, summon_manes_spell},
    { 25, 16, 40, fire_ball_spell},
    { 30, 18, 45, cause_wounds_III_spell},
    { 32, 20, 50, amnesia_spell},
    { 36, 70, 85, summon_demon_spell},
    { 40, 10, 50, enchantment_spell}, /* Note: Mariliths need corpses to eat, so they cannot spam
                                         this spell in the town the way other characters may */
    { -1, -1, -1, NULL}
};
static int _marilith_get_spells(spell_info* spells, int max) {
    return get_spells_aux(spells, max, _marilith_spells);
}

static void _marilith_birth(void) {
    object_type    forge;

    plr_mon_race_set("u.manes");

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_d = 3;
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_SWORD, SV_BROAD_SWORD));
    forge.to_h = 1;
    forge.to_d = 2;
    plr_birth_obj(&forge);

    _demon_birth();
}

static void _marilith_calc_innate_attacks(void) {
    if (plr->lev >= 40)
    {
        mon_blow_ptr blow = mon_blow_alloc(RBM_STING);
        blow->name = "Tail";
        blow->power = plr->lev*3/2;
        mon_blow_push_effect(blow, RBE_HURT, dice_create(3, 7, 0));
        vec_add(plr->innate_blows, blow);
    }
}

static void _marilith_calc_bonuses(void) {
    plr->align -= 200;

    res_add(GF_FIRE);
    res_add(GF_NETHER);

    plr->slow_digest = TRUE;
    plr->hold_life++;
    plr->no_eldritch = TRUE;

    if (plr_mon_race_is_("u.quasit"))
    {
        plr->levitation = TRUE;
        plr->see_inv++;
    }
    else if (plr_mon_race_is_("u.bodak"))
    {
        res_add(GF_CONF);
        res_add(GF_POIS);
        plr->sh_fire = TRUE;
        plr->free_act++;
        plr->see_inv++;
    }
    else if (plr_mon_race_is_("u.quasit.death"))
    {
        res_add(GF_CONF);
        res_add(GF_POIS);
        res_add(GF_TELEPORT);
        plr->pspeed += 5;
        plr->levitation = TRUE;
        plr->pass_wall = TRUE;
        plr->no_passwall_dam = TRUE;
        plr->free_act++;
        plr->see_inv++;
    }
    else if (plr_mon_race_is_("U.marilith"))
    {
        res_add(GF_POIS);
        res_add(GF_CONF);
        res_add(GF_CHAOS);
        plr->pspeed += 5;
        plr->free_act++;
        plr->see_inv++;
    }
}

static void _marilith_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_RES_(GF_FIRE));
    add_flag(flgs, OF_RES_(GF_NETHER));

    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_SLOW_DIGEST);

    if (plr_mon_race_is_("u.quasit"))
    {
        add_flag(flgs, OF_LEVITATION);
        add_flag(flgs, OF_SEE_INVIS);
    }
    else if (plr_mon_race_is_("u.bodak"))
    {
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_RES_(GF_POIS));
        add_flag(flgs, OF_AURA_FIRE);
        add_flag(flgs, OF_FREE_ACT);
        add_flag(flgs, OF_SEE_INVIS);
    }
    else if (plr_mon_race_is_("u.quasit.death"))
    {
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_RES_(GF_POIS));
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_LEVITATION);
        add_flag(flgs, OF_FREE_ACT);
        add_flag(flgs, OF_SEE_INVIS);
    }
    else if (plr_mon_race_is_("U.marilith"))
    {
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_RES_(GF_POIS));
        add_flag(flgs, OF_RES_(GF_CHAOS));
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_FREE_ACT);
        add_flag(flgs, OF_SEE_INVIS);
    }
}

static void _marilith_gain_level(int new_level) {
    if (plr_mon_race_is_("u.manes") && new_level >= 10)
        plr_mon_race_evolve("u.quasit");
    if (plr_mon_race_is_("u.quasit") && new_level >= 20)
        plr_mon_race_evolve("u.bodak");
    if (plr_mon_race_is_("u.bodak") && new_level >= 30)
        plr_mon_race_evolve("u.quasit.death");
    if (plr_mon_race_is_("u.quasit.death") && new_level >= 40)
        plr_mon_race_evolve("U.marilith");
}

static plr_race_ptr _marilith_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[5] =  {"Manes", "Quasit", "Bodak", "Death Quasit", "Marilith"};
    int           rank = 0;

    if (plr->lev >= 10) rank++;
    if (plr->lev >= 20) rank++;
    if (plr->lev >= 30) rank++;
    if (plr->lev >= 40) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  35,  36,   3,  16,  10,  56,  35};
    skills_t xs = { 60,  55,  50,   0,   0,   0, 100,  55};

        me = plr_race_alloc_aux(RACE_MON_DEMON, DEMON_MARILITH);
        me->subdesc = "Tanar'ri were originally slave demons, but rose up to overthrow their masters. "
        "They generally take on a humanoid form and are classic demons full of malice and "
        "cruelty. The ultimate form for this demon is the Marilith, a female demon with "
        "three sets of arms and a serpent body capable of attacking with six melee weapons!";

        me->skills = bs;
        me->extra_skills = xs;

        me->exp = 250;
        me->infra = 5;
        me->base_hp = 30;

        me->hooks.birth = _marilith_birth;
        me->hooks.calc_innate_attacks = _marilith_calc_innate_attacks;
        me->hooks.get_spells = _marilith_get_spells;
        me->hooks.calc_bonuses = _marilith_calc_bonuses;
        me->hooks.get_flags = _marilith_get_flags;
        me->hooks.gain_level = _marilith_gain_level;
        me->hooks.caster_info = _caster_info;

        me->pseudo_class_id = CLASS_CHAOS_WARRIOR;
        me->boss_r_idx = mon_race_parse("U.Mephistopheles")->id;
    }

    if (spoiler_hack || birth_hack)
    {
        me->subname = "Tanar'ri";
        rank = 0;
    }
    else
        me->subname = titles[rank];

    me->stats[A_STR] =  rank;
    me->stats[A_INT] =  rank/2;
    me->stats[A_WIS] = -5;
    me->stats[A_DEX] =  rank;
    me->stats[A_CON] =  rank;
    me->stats[A_CHR] =  rank/2;
    me->life = 95 + 2*rank;

    me->equip_template = plr_equip_template();

    return me;
}

/******************************************************************************
 *                       40
 * Balrog: Lesser Balrog -> Greater Balrog
 ******************************************************************************/
static spell_info _balrog_spells[] = {
    {  2,  1, 20, detect_unlife_spell},
    {  3,  2, 25, evil_bless_spell},
    {  4,  5, 30, resist_fire_spell},
    {  7,  5, 45, scare_spell},
    {  9,  5, 40, fire_bolt_spell},
    { 10,  6, 40, nether_bolt_spell},
    { 11,  9, 35, summon_manes_spell},
    { 20, 12, 40, plasma_bolt_spell},
    { 25, 12, 40, fire_ball_spell},
    { 27, 17, 50, flow_of_lava_spell},
    { 30, 20, 50, recharging_spell},
    { 32, 22, 50, nether_ball_spell},
    { 34, 24, 50, plasma_ball_spell},
    { 36, 70, 55, summon_demon_spell},
    { 37, 32, 60, kiss_of_succubus_spell},
    { 40, 35, 50, brain_smash_spell},
    { 43, 90, 90, summon_greater_demon_spell},
    { 45, 80, 85, hellfire_spell},
    { -1, -1, -1, NULL}
};
static int _balrog_get_spells(spell_info* spells, int max) {
    return get_spells_aux(spells, max, _balrog_spells);
}
static void _balrog_birth(void)
{
    object_type    forge;

    plr_mon_race_set("U.balrog.lesser");

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_d = 5;
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_HAFTED, SV_WHIP));
    forge.name2 = EGO_WEAPON_SLAYING;
    add_flag(forge.flags, OF_BRAND_FIRE);
    forge.dd = 2;
    forge.ds = 6;
    forge.to_h = 5;
    forge.to_d = 5;
    plr_birth_obj(&forge);

    _demon_birth();
}
static void _balrog_calc_bonuses(void) {
    plr->align -= 200;

    res_add(GF_FIRE);
    res_add(GF_NETHER);

    plr->hold_life++;
    plr->no_eldritch = TRUE;
    plr->pspeed += plr->lev/8; /* Angels get +7 speed. Demons get +6 speed. */
    plr->sh_fire = TRUE;

    if (equip_find_art("~.Daemon"))
    {
        plr->dec_mana++;
        plr->easy_spell++;
    }

    if (plr->lev >= 10)
        plr->see_inv++;

    if (plr->lev >= 30)
    {
        res_add(GF_FIRE);
        res_add(GF_CHAOS);
    }

    if (plr->lev >= 40)
    {
        res_add_immune(GF_FIRE);
        res_add(GF_NETHER);
        plr->kill_wall = TRUE;
        plr->no_charge_drain = TRUE;
    }
}
static void _balrog_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_RES_(GF_FIRE));
    add_flag(flgs, OF_RES_(GF_NETHER));

    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_AURA_FIRE);

    if (plr->lev >= 8)
        add_flag(flgs, OF_SPEED);
    if (plr->lev >= 10)
        add_flag(flgs, OF_SEE_INVIS);
    if (plr->lev >= 30)
        add_flag(flgs, OF_RES_(GF_CHAOS));
    if (plr->lev >= 40)
        add_flag(flgs, OF_IM_(GF_FIRE));
}
static void _balrog_gain_level(int new_level) {
    if (plr_mon_race_is_("U.balrog.lesser") && new_level >= 40)
        plr_mon_race_evolve("U.balrog");
}
static plr_race_ptr _balrog_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[2] =  {"Lesser Balrog", "Greater Balrog"};
    int           rank = 0;

    if (plr->lev >= 40) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  35,  40,  -2,  10,   7,  75,  30};
    skills_t xs = { 60,  55,  75,   0,   0,   0, 175,  35};

        me = plr_race_alloc_aux(RACE_MON_DEMON, DEMON_BALROG);
        me->subdesc = "Balrogs are demons of shadow and flame. Their evil knows no bounds. Their spells are "
        "the most powerful of all demonkind and at very high levels they may even call forth "
        "fires directly from hell.";

        me->skills = bs;
        me->extra_skills = xs;

        me->exp = 300;
        me->base_hp = 45;

        me->hooks.birth = _balrog_birth;
        me->hooks.get_spells = _balrog_get_spells;
        me->hooks.calc_bonuses = _balrog_calc_bonuses;
        me->hooks.get_flags = _balrog_get_flags;
        me->hooks.gain_level = _balrog_gain_level;
        me->hooks.caster_info = _caster_info;

        me->pseudo_class_id = CLASS_CHAOS_WARRIOR;
        me->boss_r_idx = mon_race_parse("U.Gothmog")->id;
    }

    if (spoiler_hack || birth_hack)
        me->subname = "Balrog";
    else
        me->subname = titles[rank];
    me->stats[A_STR] =  4 + 3*rank;
    me->stats[A_INT] =  1 + 2*rank;
    me->stats[A_WIS] = -10;
    me->stats[A_DEX] =  1 + 2*rank;
    me->stats[A_CON] =  4 + 2*rank;
    me->stats[A_CHR] =  2 + rank;
    me->infra = 5 + 10*rank;
    me->life = 105 + 10*rank;

    return me;
}

/******************************************************************************
 * Cyberdemon
 ******************************************************************************/
static int _rocket_amount(void)
{
    int pct = 15 + plr_prorata_level(30);
    return 25 + plr->chp * pct / 100;
}

void _cyber_rocket_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Rocket");
        break;
    case SPELL_DESC:
        var_set_string(res, "Launches a powerful rocket at your opponent.");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, plr->lev*19/50 + plr->lev*plr->lev*19/2500);
        break;
    default:
        rocket_spell_aux(cmd, res, innate_dice(0, 0, _rocket_amount()));
    }
}

static power_info _cyber_powers[] = {
    { A_CON, {  1,  2, 30, _cyber_rocket_spell} },
    {    -1, { -1, -1, -1, NULL}}
};
static int _cyber_get_powers(spell_info* spells, int max) {
    return get_powers_aux(spells, max, _cyber_powers);
}

static void _cyber_birth(void)
{
    object_type    forge;

    plr_mon_race_set("U.cyber");

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_d = 10;
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_HAFTED, SV_GREAT_HAMMER));
    plr_birth_obj(&forge);

    _demon_birth();
}

static void _cyber_calc_bonuses(void)
{
    int to_a = plr_prorata_level(75);

    plr->move_random = TRUE;

    plr->to_a += to_a;
    plr->dis_to_a += to_a;
    plr->pspeed -= 1 + plr->lev/23;

    res_add(GF_FIRE);
    res_add(GF_POIS);
/*  Cyberdemons are vulnerable to confusion. See res_pct_aux() in resist.c
    res_add_vuln(GF_CONF); */

    plr->hold_life++;
    plr->no_eldritch = TRUE;
    plr->free_act++;
}

static void _cyber_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_FIRE));
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_DEC_SPEED);

    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_FREE_ACT);
}

static void _mon_disturb(int id, mon_ptr mon)
{
    if (mon->cdis < MAX_SIGHT * 2)
        mon_tim_remove(mon, MT_SLEEP);
}
static void _cyber_move_player(void)
{
    /* Cyberdemons move erratically (cf get_rep_dir()) and make a lot of noise */
    if (one_in_(66))
    {
        cmsg_print(TERM_RED, "The dungeon trembles!");
        if (disturb_minor) disturb(0, 0);
        dun_iter_mon(cave, _mon_disturb);
    }
}

static plr_race_ptr _cyber_get_race_t(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  18,  31,  -1,  13,   7,  75,  30};
    skills_t xs = { 60,  30,  45,   0,   0,   0, 175,  35};

        me = plr_race_alloc_aux(RACE_MON_DEMON, DEMON_CYBERDEMON);
        me->subname = "Cyberdemon";
        me->subdesc = "Cyberdemons are giant humanoid forms, half demon and half machine. They are a bit "
        "slow and move erratically, but their immense bodies and unsurpassable firepower "
        "more than make up for this. The walls of the dungeon reverberate with their heavy steps!";
        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 5;
        me->life = 120;

        me->exp = 275;
        me->base_hp = 50;

        me->hooks.birth = _cyber_birth;
        me->hooks.get_powers = _cyber_get_powers;
        me->hooks.calc_bonuses = _cyber_calc_bonuses;
        me->hooks.get_flags = _cyber_get_flags;
        me->hooks.move_player = _cyber_move_player;

        me->pseudo_class_id = CLASS_WARRIOR;
        me->boss_r_idx = mon_race_parse("U.Oremorj")->id;
    }

    me->stats[A_STR] =  5 + plr->lev/10;
    me->stats[A_INT] = -10;
    me->stats[A_WIS] = -10;
    me->stats[A_DEX] = -3;
    me->stats[A_CON] =  5 + plr->lev/10;
    me->stats[A_CHR] =  0;

    return me;
}

/**********************************************************************
 * Public
 **********************************************************************/
plr_race_ptr mon_demon_get_race(int psubrace)
{
    plr_race_ptr result = NULL;

    if (birth_hack && psubrace >= DEMON_MAX)
        psubrace = 0;

    assert(0 <= psubrace && psubrace < DEMON_MAX);

    switch (psubrace)
    {
    case DEMON_BALROG:
        result = _balrog_get_race_t();
        break;
    case DEMON_MARILITH:
        result = _marilith_get_race_t();
        break;
    case DEMON_CYBERDEMON:
        result = _cyber_get_race_t();
        break;
    case DEMON_KHORNE:
        result = _khorne_get_race_t();
        break;
    default: /* Birth Menus */
        result = _balrog_get_race_t();
    }

    result->name = "Demon";
    result->desc = _desc;
    result->flags = RACE_IS_MONSTER | RACE_IS_DEMON | RACE_IS_NONLIVING;
    result->shop_adjust = 140;

    return result;
}
