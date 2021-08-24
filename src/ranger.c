#include "angband.h"

static void _calc_bonuses(void)
{
    /* rangers are decent shooters all around, but especially good with bows */
    slot_t slot = equip_find_obj(TV_BOW, SV_ANY); /* fyi, shooter_info not set yet ... */
    if (slot) plr->skills.thb += 20 + plr->lev;
    plr->pass_tree = TRUE;
}

static void _calc_shooter_bonuses(object_type *o_ptr, plr_shoot_info_ptr info_ptr)
{
    if (plr->shooter_info.tval_ammo != TV_ARROW )
        plr->shooter_info.base_shot = 100;
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 15;
    spell->cost = 20;
    spell->fail = calculate_fail_rate(spell->level, 90, plr->stat_ind[A_WIS]);
    spell->fn = probing_spell;

    return ct;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "spell";
        me.which_stat = A_INT;
        me.encumbrance.max_wgt = 450;
        me.encumbrance.weapon_pct = 33;
        me.encumbrance.enc_wgt = 1000;
        me.min_level = 3;
        me.min_fail = 5;
        me.options = CASTER_GLOVE_ENCUMBRANCE;
        me.realm1_choices = CH_NATURE;
        me.realm2_choices = CH_SORCERY | CH_CHAOS | CH_DEATH | CH_TRUMP | CH_ARCANE | CH_DAEMON;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_DAGGER, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_obj_aux(TV_BOW, SV_SHORT_BOW, 1);
    plr_birth_obj_aux(TV_ARROW, SV_ARROW, rand_range(20, 40));
    plr_birth_spellbooks();
}

plr_class_ptr ranger_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  37,  36,   3,  24,  16,  56,  50};
    skills_t xs = { 40,  55,  50,   0,   0,   0,  90,  80};

        me = plr_class_alloc(CLASS_RANGER);
        me->name = "Ranger";
        me->desc = "A Ranger is a combination of a warrior and a mage who has "
                    "developed a special affinity for the natural world around him. He "
                    "is a good fighter and also excellent with a bow. A ranger has "
                    "good stealth, perception, searching and magical resistance. Also, "
                    "rangers are familiar with magical devices and use them well. "
                    "Intelligence determines a Ranger's spell casting ability.\n \n"
                    "All rangers are trained in Nature magic, and all of these spells are "
                    "available to them. They even learn these spells almost as fast as "
                    "mages. They can also select a secondary realm (from Sorcery, "
                    "Chaos, Death, Trump, Arcane, and Daemon), but they are slow "
                    "learners here, and may find themselves unable to learn some of the "
                    "highest level spells. They have a class power - 'Probe Monster' - "
                    "which allows them to know a monster's HP, speed, and experience "
                    "required to evolve.";

        me->stats[A_STR] =  2;
        me->stats[A_INT] =  2;
        me->stats[A_WIS] =  0;
        me->stats[A_DEX] =  1;
        me->stats[A_CON] =  1;
        me->stats[A_CHR] =  0;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 106;
        me->base_hp = 8;
        me->exp = 140;
        me->pets = 35;
        me->flags = CLASS_SENSE1_SLOW | CLASS_SENSE1_STRONG |
                    CLASS_SENSE2_SLOW | CLASS_SENSE2_STRONG;
        
        me->hooks.birth = _birth;
        me->hooks.caster_info = _caster_info;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_shooter_bonuses = _calc_shooter_bonuses;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = spellbook_character_dump;
    }

    return me;
}
