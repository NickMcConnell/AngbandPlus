#include "angband.h"

static cptr _desc =
    "Angels are heavenly beings on a holy mission. They are winged creatures able "
    "to fly over chasms and pits. They also become resistant to the elements as they "
    "mature and are not fooled by invisibility. Their stats are truly awe-inspiring "
    "making them one of the most powerful monster races.\n \n"
    "Angels are monsters so cannot choose a normal class. Instead, they rely on their "
    "divine powers which function like spells but need not be learned or cast from books. "
    "These powers can be used while blinded, but not while confused and they are also "
    "blocked by Anti-magic. The divine powers of the angel are quite powerful offering good "
    "offense combined with detection, healing and melee enhancement. Wisdom is the "
    "primary spell stat.\n \n"
    "Angels use the same equipment slots as normal player races and have no innate attacks.";

static void _birth(void)
{
    object_type    forge;

    plr_mon_race_set("A.angel");

    object_prep(&forge, lookup_kind(TV_POTION, SV_POTION_HEALING));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_HAFTED, SV_MACE));
    plr_birth_obj(&forge);

    plr_birth_food();
    plr_birth_light();
}

/******************************************************************************
 *              10           20        30        40        45          50
 * Angel: Angel -> Archangel -> Cherub -> Seraph -> Archon -> Planetar -> Solar
 ******************************************************************************/
static void _psycho_spear_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Psycho-Spear");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a beam of pure energy which penetrate the invulnerability barrier.");
        break;
    default:
        beam_spell_aux(cmd, res, GF_PSY_SPEAR, spell_dam_dice(1, 3*plr->lev, 3*plr->lev));
    }
}

static spell_info _spells[] = {
    {  1,  1, 30, punishment_spell},
    {  2,  2, 30, bless_spell},
    {  3,  2, 30, confuse_spell},
    {  5,  3, 30, scare_spell},
    {  7,  4, 30, detect_monsters_spell},
    { 10,  7, 40, heroism_spell},
    { 15, 10, 40, magic_mapping_spell},
    { 20, 15, 50, mana_bolt_I_spell},
    { 23, 15, 50, brain_smash_spell},
    { 25, 17, 50, haste_self_spell},
    { 27, 20, 50, protection_from_evil_spell},
    { 29, 20, 60, dispel_evil_spell},
    { 31, 20, 60, teleport_other_spell},
    { 32, 20, 60, healing_I_spell},
    { 35, 25, 60, destruction_spell},
    { 37, 25, 60, summon_monsters_spell},
    { 40, 30, 65, _psycho_spear_spell},
    { 42, 30, 65, restoration_spell},
    { 45, 80, 65, clairvoyance_spell},
    { 47, 60, 70, summon_angel_spell},
    { 49, 90, 70, summon_hi_dragon_spell},
    { 50, 50, 65, starburst_II_spell},
    { -1, -1, -1, NULL}
};
static int _get_spells(spell_info* spells, int max) {
    return get_spells_aux(spells, max, _spells);
}

static void _calc_bonuses(void) {
    /* cf calc_torch in xtra1.c for the 'extra light' */

    plr->align += 200;
    plr->levitation = TRUE;
    res_add(GF_POIS);

    if (equip_find_art("~.Crusade"))
    {
        plr->dec_mana++;
        plr->easy_spell++;
    }

    if (plr->lev >= 10)
    {
        res_add(GF_FIRE);
        plr->see_inv++;
    }
    if (plr->lev >= 20)
    {
        plr->pspeed += 1;
        res_add(GF_COLD);
    }
    if (plr->lev >= 30)
    {
        plr->pspeed += 1;
        res_add(GF_ACID);
        res_add(GF_ELEC);
        res_add(GF_CONF);
    }
    if (plr->lev >= 40)
    {
        plr->pspeed += 3;
        plr->reflect = TRUE;
        res_add(GF_TELEPORT);
        res_add(GF_LIGHT);
    }
    if (plr->lev >= 45)
    {
        plr->pspeed += 1;
    }
    if (plr->lev >= 50)
    {
        plr->pspeed += 1;
    }
}
static void _get_flags(u32b flgs[OF_ARRAY_SIZE]) {
    add_flag(flgs, OF_LEVITATION);
    add_flag(flgs, OF_RES_(GF_POIS));

    if (plr->lev >= 10)
    {
        add_flag(flgs, OF_RES_(GF_FIRE));
        add_flag(flgs, OF_SEE_INVIS);
    }
    if (plr->lev >= 20)
    {
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_RES_(GF_COLD));
    }
    if (plr->lev >= 30)
    {
        add_flag(flgs, OF_RES_(GF_ACID));
        add_flag(flgs, OF_RES_(GF_ELEC));
        add_flag(flgs, OF_RES_(GF_CONF));
    }
    if (plr->lev >= 40)
    {
        add_flag(flgs, OF_REFLECT);
        add_flag(flgs, OF_RES_(GF_LIGHT));
    }
    if (plr->lev >= 45)
    {
    }
    if (plr->lev >= 50)
    {
    }
}
static void _gain_level(int new_level) {
    if (plr_mon_race_is_("A.angel") && new_level >= 10)
        plr_mon_race_evolve("A.archangel");
    if (plr_mon_race_is_("A.archangel") && new_level >= 20)
        plr_mon_race_evolve("A.cherub");
    if (plr_mon_race_is_("A.cherub") && new_level >= 30)
        plr_mon_race_evolve("A.seraph");
    if (plr_mon_race_is_("A.seraph") && new_level >= 40)
        plr_mon_race_evolve("A.archon");
    if (plr_mon_race_is_("A.archon") && new_level >= 45)
        plr_mon_race_evolve("A.planetar");
    if (plr_mon_race_is_("A.planetar") && new_level >= 50)
        plr_mon_race_evolve("A.solar");
}
static plr_race_ptr _solar_get_race_t(void)
{
    static plr_race_ptr me = NULL;
    static cptr   titles[7] =  {"Angel", "Archangel", "Cherub", "Seraph", "Archon", "Planetar", "Solar"};
    int           rank = 0;

    if (plr->lev >= 10) rank++;
    if (plr->lev >= 20) rank++;
    if (plr->lev >= 30) rank++;
    if (plr->lev >= 40) rank++;
    if (plr->lev >= 45) rank++;
    if (plr->lev >= 50) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  35,  40,   3,  18,  12,  48,  35};
    skills_t xs = { 35,  65,  75,   0,   0,   0,  90,  65};

        me = plr_race_alloc(RACE_MON_ANGEL);
        me->skills = bs;
        me->extra_skills = xs;

        me->infra = 3;
        me->exp = 325; /* 14.6 Mxp */
        me->base_hp = 26;

        me->hooks.get_spells = _get_spells;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.gain_level = _gain_level;
    }

    if (birth_hack || spoiler_hack)
        rank = 0;

    me->subname = titles[rank];
    me->stats[A_STR] =  1 + rank/2;
    me->stats[A_INT] =  1 + rank/2;
    me->stats[A_WIS] =  4 + rank/2;
    me->stats[A_DEX] =  1 + rank/2;
    me->stats[A_CON] =  1 + rank/2;
    me->stats[A_CHR] =  1 + rank;
    me->life = 90 + 3*rank;

    return me;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "divine power";
        me.which_stat = A_WIS;
        me.encumbrance.max_wgt = 450;
        me.encumbrance.weapon_pct = 67;
        me.encumbrance.enc_wgt = 800;
        me.options = CASTER_GAIN_SKILL;
        init = TRUE;
    }
    return &me;
}

/**********************************************************************
 * Public
 **********************************************************************/
plr_race_ptr mon_angel_get_race(void)
{
    plr_race_ptr result = NULL;

    switch (plr->psubrace)
    {
    /* TODO: Fallen Angel ? */
    default: /* Birth Menus */
        result = _solar_get_race_t();
    }

    result->name = "Angel";
    result->desc = _desc;
    result->flags = RACE_IS_MONSTER;
    result->hooks.birth = _birth;
    result->hooks.caster_info = _caster_info;
    result->pseudo_class_id = CLASS_PRIEST;
    result->shop_adjust = 90;

    result->boss_r_idx = mon_race_parse("A.Raphael")->id;

    if (birth_hack || spoiler_hack)
    {
        result->subname = NULL;
        result->subdesc = NULL;
    }

    return result;
}
