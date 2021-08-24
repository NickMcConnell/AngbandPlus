#include "angband.h"

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 25;
    spell->cost = 0;
    spell->fail = calculate_fail_rate(spell->level, 50, p_ptr->stat_ind[A_INT]);
    spell->fn = hp_to_sp_spell;

    spell = &spells[ct++];
    spell->level = 25;
    spell->cost = 0;
    spell->fail = calculate_fail_rate(spell->level, 50, p_ptr->stat_ind[A_INT]);
    spell->fn = sp_to_hp_spell;

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
        me.encumbrance.max_wgt = 430;
        me.encumbrance.weapon_pct = 33;
        me.encumbrance.enc_wgt = 1200;
        me.options = CASTER_GLOVE_ENCUMBRANCE;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    p_ptr->proficiency[PROF_SWORD] = WEAPON_EXP_BEGINNER;

    py_birth_obj_aux(TV_SWORD, SV_SHORT_SWORD, 1);
    py_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    py_birth_spellbooks();

    p_ptr->proficiency_cap[PROF_DIGGER] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_BLUNT] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_POLEARM] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_SWORD] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_STAVE] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_AXE] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_DAGGER] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_BOW] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_CROSSBOW] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_SLING] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_MARTIAL_ARTS] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_DUAL_WIELDING] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_RIDING] = RIDING_EXP_SKILLED;
}

class_t *warrior_mage_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  37,  36,   2,  18,  16,  56,  50};
    skills_t xs = {  7,  11,  10,   0,   0,   0,  18,  15};

        me.name = "Warrior-Mage";
        me.desc = "A Warrior-Mage is precisely what the name suggests: a cross "
                    "between the warrior and mage classes. While their brothers, the "
                    "rangers, specialize in Nature magic and survival skills, true "
                    "Warrior-Mages attempt to reach the best of both worlds. As "
                    "warriors they are much superior to the usual Mage class. "
                    "Intelligence determines a Warrior-Mage's spell casting ability.\n \n"
                    "Warrior-mages begin the game with Arcane magic, and they can "
                    "freely select another realm of magic. Although they do not gain "
                    "new spells as fast as regular mages, they will eventually learn "
                    "every spell in both realms, thus making them a very competitive "
                    "choice for players who appreciate Arcane magic. They have two "
                    "class powers - 'Convert HP to SP' and 'Convert SP to HP' - which "
                    "allow them to heal HP using mana or gain mana using HP.";

        me.stats[A_STR] =  2;
        me.stats[A_INT] =  2;
        me.stats[A_WIS] =  0;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] =  0;
        me.stats[A_CHR] =  0;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 106;
        me.base_hp = 8;
        me.exp = 140;
        me.pets = 35;
        me.flags = CLASS_SENSE1_MED | CLASS_SENSE1_WEAK |
                   CLASS_SENSE2_MED | CLASS_SENSE2_STRONG;
        
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
