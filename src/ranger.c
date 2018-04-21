#include "angband.h"

static void _calc_shooter_bonuses(object_type *o_ptr, shooter_info_t *info_ptr)
{
    if ( !p_ptr->shooter_info.heavy_shoot
      && p_ptr->shooter_info.tval_ammo == TV_ARROW )
    {
        p_ptr->shooter_info.num_fire += p_ptr->lev * 150 / 50;
    }
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 15;
    spell->cost = 20;
    spell->fail = calculate_fail_rate(spell->level, 90, p_ptr->stat_ind[A_WIS]);
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
        me.which_stat = A_WIS;
        me.weight = 450;
        me.min_level = 3;
        me.min_fail = 5;
        me.options = CASTER_GLOVE_ENCUMBRANCE;
        init = TRUE;
    }
    return &me;
}

class_t *ranger_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  37,  36,   3,  24,  16,  56,  72};
    skills_t xs = {  8,  11,  10,   0,   0,   0,  18,  28};

        me.name = "Ranger";
        me.desc = "A Ranger is a combination of a warrior and a mage who has "
                    "developed a special affinity for the natural world around him. He "
                    "is a good fighter and also excellent with a bow. A ranger has "
                    "good stealth, perception, searching and magical resistance. Also, "
                    "rangers are familiar with magical devices and use them well. "
                    "Wisdom determines a Ranger's spell casting ability.\n \n"
                    "All rangers are trained in Nature magic, and all of these spells are "
                    "available to them. They even learn these spells almost as fast as "
                    "mages. They can also select a secondary realm (from Sorcery, "
                    "Chaos, Death, Trump, Arcane, and Daemon), but they are slow "
                    "learners here, and may find themselves unable to learn some of the "
                    "highest level spells. They have a class power - 'Probe Monster' - "
                    "which allows them to know a monster's HP, speed, and experience "
                    "required to evolve.";

        me.stats[A_STR] =  2;
        me.stats[A_INT] =  0;
        me.stats[A_WIS] =  2;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] =  1;
        me.stats[A_CHR] =  0;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 106;
        me.base_hp = 8;
        me.exp = 140;
        me.pets = 35;
        
        me.caster_info = _caster_info;
        me.calc_shooter_bonuses = _calc_shooter_bonuses;
        /* TODO: This class uses spell books, so we are SOL
        me.get_spells = _get_spells;*/
        me.get_powers = _get_powers;
        me.character_dump = spellbook_character_dump;
        init = TRUE;
    }

    return &me;
}
