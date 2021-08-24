#include "angband.h"

#include <assert.h>

static cptr _desc = 
    "Liches are skeletal forms back from the dead to hunt the living. There "
    "are several types of liches from the <color:keyword>Archlich</color> with powerful undead "
    "sorcery to the <color:keyword>Monastic Lich</color> who prefers martial arts combat, "
    "with a necromantic twist. The <color:keyword>Iron Lich</color> is a floating skull "
    "with a powerful arsenal of attack spells but severely limited equipment slots. "
    "Finally, the <color:keyword> Black Reaver</color> is out to harvest the living, "
    "allowing nothing to stand in his way! Each form of lich will begin life as a normal "
    "Lich with some weak magic, and must evolve a bit before they gain their specific "
    "powers.\n \n"
    "Liches are monsters and cannot choose a normal class. Instead, they are born "
    "with magical powers and gain additional powers as they advance. Intelligence is "
    "the primary spell stat. In their initial form, they gain the ability to touch "
    "opponents in melee for various foul effects should they forgo the use of a "
    "normal melee weapon.";

static void _birth(void) 
{ 
    obj_t forge;

    plr_mon_race_set("L.lich");
    skills_innate_init("Finger", WEAPON_EXP_BEGINNER, WEAPON_EXP_MASTER);

    object_prep(&forge, lookup_kind(TV_CROWN, SV_IRON_CROWN));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_STAFF, SV_ANY));
    if (device_init_fixed(&forge, EFFECT_ANIMATE_DEAD))
        plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_SOFT_ARMOR, SV_ROBE));
    plr_birth_obj(&forge);
}

/**********************************************************************
 * Innate Attacks
 **********************************************************************/
static void _calc_innate_bonuses(mon_blow_ptr blow)
{
    if (blow->method == RBM_TOUCH)
    {
        plr->innate_attack_info.blows_calc.wgt = 100;
        plr->innate_attack_info.blows_calc.mul = 30;
        plr->innate_attack_info.blows_calc.max = 400;
        plr_calc_blows_innate(blow);
    }
}
static void _calc_innate_attacks(void)
{
    int l = plr->lev;
    int dd = 1 + l/12;
    int ds = 6 + l/15;
    mon_blow_ptr blow = mon_blow_alloc(RBM_TOUCH);

    blow->name = "Finger";
    blow->power = 20 + 2*l; /* compensate for weak melee skills */
    mon_blow_push_effect(blow, GF_NETHER, dice_create(dd, ds, 0));
    if (l >= 40)
        mon_blow_push_effect(blow, GF_DISENCHANT, dice_create(dd, ds, 0));
    if (l >= 25)
        mon_blow_push_effect(blow, GF_UNLIFE, dice_create(dd, ds, 0));

    _calc_innate_bonuses(blow);
    vec_add(plr->innate_blows, blow);
}

/**********************************************************************
 * Spells
 **********************************************************************/
static void _disenchantment_ball_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Antimagic Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a huge ball of antimagic which disenchants your enemies.");
        break;
    default:
        ball_spell(cmd, res, 2 + plr->lev/20, GF_DISENCHANT, plr_prorata_level(300));
    }
}

static void _orb_of_draining_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Orb of Draining");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a powerful ball that damages living monsters, draining their life force.");
        break;
    default:
        ball_spell(cmd, res, 3, GF_LICH_DRAIN, plr_prorata_level(125));
    }
}

static void _send_to_netherworld_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Send to Netherworld");
        break;
    case SPELL_DESC:
        var_set_string(res, "Point at a single living monster to send it directly to the netherworld.");
        break;
    default:
        direct_spell(cmd, res, GF_LICH_GENOCIDE, 6*plr->lev);
    }
}

static void _word_of_vecna_spell(int cmd, var_ptr res)
{
    int power = spell_power(plr_prorata_level(150) + plr->to_d_spell);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Word of Vecna");
        break;
    case SPELL_DESC:
        var_set_string(res, "The unspeakable Word of Vecna damages all monsters in sight, greatly "
            "hurting living monsters. Various effects such as stunning, fear and confusion are also "
            "possible. Undead monsters may be forced to obey you.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_power(power));
        break;
    case SPELL_CAST:
        plr_project_los(GF_LICH_WORD, power);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

/**********************************************************************
 *                25             40          50
 * Archlich (Lich -> Master Lich -> Demilich -> Archlich)
 **********************************************************************/
static spell_info _archlich_spells[] = {
    {  1,  2, 20, malediction_spell },
    {  3,  2, 25, phase_door_spell },
    {  6,  4, 30, detect_life_spell },
    {  9,  4, 40, nether_bolt_spell },
    { 11,  5, 50, teleport_spell },
    { 14,  7, 50, slow_spell }, 
    { 17,  9, 50, paralyze_spell },
    { 20, 12, 50, teleport_other_spell },
    { 23, 15, 50, brain_smash_spell },
    { 26, 15, 50, nether_ball_spell },
    { 28, 20, 55, ice_bolt_spell },
    { 30, 20, 60, dispel_life_spell }, 
    { 32, 40, 60, summon_undead_spell }, 
    { 34, 15, 50, animate_dead_spell }, 
    { 36, 35, 70, _orb_of_draining_spell },
    { 38, 35, 80, recharging_spell },
    { 40, 40, 80, _send_to_netherworld_spell },
    { 45, 40, 70, _disenchantment_ball_spell },
    { 50, 50, 68, _word_of_vecna_spell },
    { -1, -1, -1, NULL}
};
static int _archlich_get_spells(spell_info* spells, int max) {
    return get_spells_aux(spells, max, _archlich_spells);
}

static int _archlich_get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 25;
    spell->cost = 1;
    spell->fail = calculate_fail_rate(spell->level, 90, plr->stat_ind[A_INT]);
    spell->fn = eat_magic_spell;

    return ct;
}

static void _archlich_calc_bonuses(void) {
    if (plr->lev < 25)
        res_add_vuln(GF_LIGHT);

    plr->align -= 200;
    plr->see_nocto = DUN_VIEW_MAX;
    plr->see_inv++;
    plr->slow_digest = TRUE;
    plr->hold_life++;
    res_add(GF_COLD);
    res_add(GF_POIS);
    res_add(GF_NETHER);
    if (plr->lev >= 25)
    {
        plr->pspeed += 1;
        res_add(GF_CONF);
        res_add(GF_TELEPORT);
        plr->free_act++;
        plr->hold_life++;
        plr->wizard_sight = TRUE;
    }
    if (plr->lev >= 40)
    {
        plr->pspeed += 2;
        res_add(GF_COLD);
        res_add(GF_POIS);
        res_add(GF_NETHER);
        plr->hold_life++;
        plr->telepathy = TRUE;
    }
    if (plr->lev >= 50)
    {
        plr->pspeed += 2;
        plr->levitation = TRUE;
        res_add_immune(GF_NETHER);
        plr->pass_wall = TRUE;
        plr->no_passwall_dam = TRUE;
        plr->hold_life++;
    }
}
static void _archlich_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_SEE_INVIS);
    add_flag(flgs, OF_SLOW_DIGEST);
    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_RES_(GF_COLD));
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_RES_(GF_NETHER));

    if (plr->lev < 25)
        add_flag(flgs, OF_VULN_(GF_LIGHT));

    if (plr->lev >= 25)
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_FREE_ACT);
    }
    if (plr->lev >= 40)
    {
        add_flag(flgs, OF_TELEPATHY);
    }
    if (plr->lev >= 50)
    {
        add_flag(flgs, OF_LEVITATION);
        add_flag(flgs, OF_IM_(GF_NETHER));
    }
}
static void _archlich_gain_level(int new_level) {
    if (plr_mon_race_is_("L.lich") && new_level >= 25)
        plr_mon_race_evolve("L.master");
    if (plr_mon_race_is_("L.master") && new_level >= 40)
        plr_mon_race_evolve("L.demilich");
    if (plr_mon_race_is_("L.demilich") && new_level >= 50)
        plr_mon_race_evolve("L.archlich");
}
static caster_info * _archlich_caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "unholy power";
        me.which_stat = A_INT;
        me.encumbrance.max_wgt = 450;
        me.encumbrance.weapon_pct = 100;
        me.encumbrance.enc_wgt = 600;
        me.options = CASTER_ALLOW_DEC_MANA | CASTER_GAIN_SKILL;
        init = TRUE;
    }
    return &me;
}

static plr_race_ptr _archlich_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[4] =  {"Lich", "Master Lich", "Demilich", "Archlich"};    
    int           rank = 0;

    if (plr->lev >= 25) rank++;
    if (plr->lev >= 40) rank++;
    if (plr->lev >= 50) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  45,  38,   7,  20,  30,  34,  20 };
    skills_t xs = { 35,  75,  60,   0,   0,   0,  30,  35 };

        me = plr_race_alloc_aux(RACE_MON_LICH, LICH_ARCH);

        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 5;
        me->exp = 275;
        me->base_hp = 20;

        me->hooks.birth = _birth;
        me->hooks.caster_info = _archlich_caster_info;
        me->hooks.get_spells = _archlich_get_spells;
        me->hooks.get_powers = _archlich_get_powers;
        me->hooks.calc_bonuses = _archlich_calc_bonuses;
        me->hooks.get_flags = _archlich_get_flags;
        me->hooks.gain_level = _archlich_gain_level;
        me->hooks.calc_innate_attacks = _calc_innate_attacks;
        me->hooks.calc_innate_bonuses = _calc_innate_bonuses;

        me->pseudo_class_id = CLASS_MAGE;
        me->flags = RACE_MAGE_BONUS;
    }

    me->subname = titles[rank];
    me->stats[A_STR] =  0 - (plr->lev / 10);
    me->stats[A_INT] =  3 + rank;
    me->stats[A_WIS] = -3 - rank;
    me->stats[A_DEX] =  1 + rank;
    me->stats[A_CON] =  0 - (rank+1)/2;
    me->stats[A_CHR] =  0 + rank;
    me->life = 100 - 2*rank;

    return me;
}

/**********************************************************************
 *                     25
 * Monastic Lich (Lich -> Monastic Lich)
 **********************************************************************/
static void _entomb_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Entomb");
        break;
    case SPELL_DESC:
        var_set_string(res, "Entombs chosen foe.");
        break;
    case SPELL_CAST:
        direct_spell(cmd, res, GF_ENTOMB, 0);
        break;
    default:
        default_spell(cmd, res);
    }
}
static void _massacre_living_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Massacre the Living");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack all adjacent living monsters in a fit of wild, uncontrollable fury.");
        break;
    case SPELL_CAST: {
        int dir;
        for (dir = 0; dir < 8; dir++)
        {
            point_t p = point_step(plr->pos, ddd[dir]);
            mon_ptr mon = dun_mon_at(cave, p);

            if (!mon || !mon_is_living(mon)) continue;
            if (mon->ml || cell_project(dun_cell_at(cave, p)))
                plr_attack_normal(p);
        }
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _summon_kin_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Kin");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to summon some monastic friends.");
        break;
    case SPELL_CAST:
        if (!summon_named_creature(who_create_null(), plr->pos, mon_race_parse("L.monk"), PM_FORCE_PET))
            msg_print("No friends arrive.");
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static spell_info _monastic_lich_spells[] = {
    {  1,  2, 20, malediction_spell },
    {  3,  2, 25, phase_door_spell },
    {  6,  4, 30, detect_life_spell },
    {  9,  4, 40, nether_bolt_spell },
    { 11,  5, 50, teleport_spell },
    { 14,  7, 50, slow_spell }, 
    { 17,  9, 50, paralyze_spell },
    { 20, 12, 50, teleport_other_spell },
    { 23, 15, 50, brain_smash_spell },
    { 29, 20, 60, haste_self_spell },
    { 35, 30, 60, monk_double_attack_spell },
    { 40, 70, 70, _summon_kin_spell },
    { 42, 70, 70, _massacre_living_spell },
    { 45, 50, 70, _entomb_spell },
    { -1, -1, -1, NULL}
};
static int _monastic_lich_get_spells(spell_info* spells, int max) {
    return get_spells_aux(spells, max, _monastic_lich_spells);
}

static void _monastic_lich_calc_bonuses(void) {
    if (plr->lev < 25)
        res_add_vuln(GF_LIGHT);

    plr->align -= 200;
    plr->see_nocto = DUN_VIEW_MAX;
    plr->see_inv++;
    plr->slow_digest = TRUE;
    plr->hold_life++;
    res_add(GF_COLD);
    res_add(GF_POIS);
    res_add(GF_NETHER);
    if (plr->lev >= 25)
    {
        plr->monk_lvl = plr->lev;
        plr->monk_tbl = "Monk.Lich";
        monk_ac_bonus();
        if (!heavy_armor())
        {
            plr->pspeed += plr->lev/10; 
            plr->free_act++;
            plr->sh_retaliation = TRUE;
        }
        res_add(GF_CONF);
        res_add(GF_TELEPORT);
        plr->hold_life++;
    }
}
static void _monastic_lich_get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_SEE_INVIS);
    add_flag(flgs, OF_SLOW_DIGEST);
    add_flag(flgs, OF_HOLD_LIFE);
    add_flag(flgs, OF_RES_(GF_COLD));
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_RES_(GF_NETHER));

    if (plr->lev < 25)
        add_flag(flgs, OF_VULN_(GF_LIGHT));

    if (plr->lev >= 25)
    {
        if (!heavy_armor())
        {
            add_flag(flgs, OF_SPEED);
            add_flag(flgs, OF_FREE_ACT);
            add_flag(flgs, OF_AURA_REVENGE);
        }
        add_flag(flgs, OF_RES_(GF_CONF));
    }
}
static caster_info * _monastic_lich_caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "unholy power";
        me.which_stat = A_INT;
        me.encumbrance.max_wgt = 350;
        me.encumbrance.weapon_pct = 100;
        me.encumbrance.enc_wgt = 800;
        me.options = CASTER_GAIN_SKILL;
        init = TRUE;
    }
    return &me;
}
static void _monastic_lich_gain_level(int new_level) {
    if (plr_mon_race_is_("L.lich") && new_level >= 25)
        plr_mon_race_evolve("L.monk");
}

static plr_race_ptr _monastic_lich_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[2] =  {"Lich", "Monastic-Lich"}; /* XXX [EQU $SUBRACE "Monastic Lich"] won't work */
    int           rank = 0;

    if (plr->lev >= 25) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 45,  34,  38,   6,  32,  24,  64,  40};
    skills_t xs = { 75,  55,  60,   0,   0,   0,  90,  75};

        me = plr_race_alloc_aux(RACE_MON_LICH, LICH_MONASTIC);

        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 5;
        me->exp = 275;
        me->life = 100;
        me->base_hp = 20;

        me->hooks.birth = _birth;
        me->hooks.caster_info = _monastic_lich_caster_info;
        me->hooks.get_spells = _monastic_lich_get_spells;
        me->hooks.calc_bonuses = _monastic_lich_calc_bonuses;
        me->hooks.get_flags = _monastic_lich_get_flags;
        me->hooks.gain_level = _monastic_lich_gain_level;

        me->pseudo_class_id = CLASS_MONK;
    }

    if (rank)
    {
        me->hooks.calc_innate_attacks = NULL;
        me->hooks.calc_innate_bonuses = NULL;
        me->flags = RACE_MARTIAL_ARTS;
    }
    else
    {
        me->hooks.calc_innate_attacks = _calc_innate_attacks;
        me->hooks.calc_innate_bonuses = _calc_innate_bonuses;
        me->flags = 0;
    }

    me->subname = titles[rank];
    me->stats[A_STR] =  0 + 2*rank;
    me->stats[A_INT] =  2 + rank;
    me->stats[A_WIS] = -3 - 2*rank;
    me->stats[A_DEX] =  1 + 3*rank;
    me->stats[A_CON] =  0 + rank;
    me->stats[A_CHR] =  0 + rank;

    return me;
}

static name_desc_t _info[LICH_MAX] = {
    { "Archlich",
        "The Archlich is the classic undead sorcerer lich. With strong magic "
        "they attempt to overwhelm all living opponents. They are poor in melee "
        "with normal weapons, but possess a powerful innate touch attack that "
        "rends the souls of their enemies. As the lich drains life from their foes "
        "they grow increasingly more powerful, though this bonus is rather short-lived." },
    { "Monastic Lich",
        "The Monastic Lich began life as a monk in an evil order and now, after "
        "its undeath, it has acquired foul abilities to enhance its martial arts." },
};


/**********************************************************************
 * Public
 **********************************************************************/
plr_race_ptr mon_lich_get_race(int psubrace)
{
    plr_race_ptr result = NULL;

    if (birth_hack && psubrace >= LICH_MAX)
        psubrace = 0;

    assert(0 <= psubrace && psubrace < LICH_MAX);

    switch (psubrace)
    {
    case LICH_ARCH:
        result = _archlich_get_race_t();
        break;

    case LICH_MONASTIC:
        result = _monastic_lich_get_race_t();
        break;

    /* TODO: Add subraces for Reaver (Lich -> Master Lich -> Lesser Black Reaver -> Black Reaver)
             in which case Archlich should lose the Manastorm.
             Also perhaps Iron Lich (Lich -> Master Lich -> Iron Lich)  */
    default: /* Birth Menus */
        result = _archlich_get_race_t();
    }

    result->name = "Lich";
    result->desc = _desc;
    result->flags |= RACE_IS_MONSTER | RACE_IS_NONLIVING | RACE_IS_UNDEAD;
    result->shop_adjust = 135;
    result->boss_r_idx = mon_race_parse("L.Vecna")->id;
    result->equip_template = plr_equip_template();

    if (birth_hack || spoiler_hack)
    {
        result->subname = _info[psubrace].name;
        result->subdesc = _info[psubrace].desc;
    }
    return result;
}
