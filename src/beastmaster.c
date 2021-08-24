#include "angband.h"

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 1;
    spell->cost = 0;
    spell->fail = calculate_fail_rate(spell->level, 70, p_ptr->stat_ind[A_CHR]);
    spell->fn = dominate_living_I_spell;

    spell = &spells[ct++];
    spell->level = 30;
    spell->cost = 0;
    spell->fail = calculate_fail_rate(spell->level, 70, p_ptr->stat_ind[A_CHR]);
    spell->fn = dominate_living_II_spell;

    return ct;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "spell";
        me.which_stat = A_CHR;
        me.encumbrance.max_wgt = 430;
        me.encumbrance.weapon_pct = 50;
        me.encumbrance.enc_wgt = 800;
        me.min_fail = 5;
        me.min_level = 3;
        me.options = CASTER_GLOVE_ENCUMBRANCE;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    py_birth_obj_aux(TV_POLEARM, SV_SPEAR, 1);
    py_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    py_birth_spellbooks();

    p_ptr->proficiency[PROF_POLEARM] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency[PROF_BOW] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency[PROF_RIDING] = WEAPON_EXP_BEGINNER;

    p_ptr->proficiency_cap[PROF_DIGGER] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_BLUNT] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_POLEARM] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_SWORD] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_STAVE] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_AXE] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_DAGGER] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_BOW] = WEAPON_EXP_MASTER;
    p_ptr->proficiency_cap[PROF_CROSSBOW] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_SLING] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_MARTIAL_ARTS] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_DUAL_WIELDING] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_RIDING] = RIDING_EXP_MASTER;
}

class_t *beastmaster_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  25,  32,   2,  18,  16,  52,  63};
    skills_t xs = {  7,  10,  10,   0,   0,   0,  14,  25};

        me.name = "Beastmaster";
        me.desc = "Beastmasters are in tune with the minds of the creatures of the "
                    "world. They are very good at riding, and have enough "
                    "fighting ability. They use monsters which have been summoned or dominated "
                    "as their hands and feet. Beastmasters can cast trump magic, "
                    "and are very good at summoning spells, but they can not summon "
                    "non-living creatures. Charisma determines a Beastmaster's spell "
                    "casting ability.\n \n"
                    "Beastmasters use Trump magic to make good use of their monster "
                    "domination and riding abilities. They are very good at summoning "
                    "living creatures, and they learn summoning spells quicker than "
                    "Mages. However, they cannot summon non-living creatures. They "
                    "have two class powers - 'Dominate a Living Thing' and 'Dominate "
                    "Living Things'.";

        me.stats[A_STR] =  1;
        me.stats[A_INT] = -1;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] =  0;
        me.stats[A_CHR] =  2;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 103;
        me.base_hp = 6;
        me.exp = 120;
        me.pets = 10;
        me.flags = CLASS_SENSE1_SLOW | CLASS_SENSE1_WEAK |
                   CLASS_SENSE2_WEAK;
        
        me.birth = _birth;
        me.caster_info = _caster_info;
        /* TODO: This class uses spell books, so we are SOL
        me.get_spells = _get_spells;*/
        me.get_powers = _get_powers;
        me.character_dump = spellbook_character_dump;
        init = TRUE;
    }

    return &me;
}
