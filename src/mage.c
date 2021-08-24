#include "angband.h"

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    /* Eat magic */
    spell_info* spell = &spells[ct++];
    spell->level = 25;
    spell->cost = 1;
    spell->fail = calculate_fail_rate(spell->level, 90, p_ptr->stat_ind[A_INT]);
    spell->fn = eat_magic_spell;

    /* Change second magic realm, but only once since there is no more learning limits */
    if (!p_ptr->old_realm)
    {
        spell_info* spell2 = &spells[ct++];
        spell2->level = 1;
        spell2->cost = 1;
        spell2->fail = 0;
        spell2->fn = change_realm_power;
    }
	
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
        me.encumbrance.weapon_pct = 100;
        me.encumbrance.enc_wgt = 600;
        me.options = CASTER_ALLOW_DEC_MANA | CASTER_GLOVE_ENCUMBRANCE;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    py_birth_obj_aux(TV_DAGGER, SV_DAGGER, 1);
    py_birth_obj_aux(TV_SOFT_ARMOR, SV_ROBE, 1);
    py_birth_spellbooks();

    p_ptr->proficiency[PROF_DAGGER] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency[PROF_SLING] = WEAPON_EXP_BEGINNER;

    p_ptr->proficiency_cap[PROF_DIGGER] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_BLUNT] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_POLEARM] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_SWORD] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_STAVE] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_AXE] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_DAGGER] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_BOW] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_CROSSBOW] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_SLING] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_MARTIAL_ARTS] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency_cap[PROF_DUAL_WIELDING] = WEAPON_EXP_UNSKILLED;
    p_ptr->proficiency_cap[PROF_RIDING] = RIDING_EXP_UNSKILLED;
}

class_t *mage_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  40,  38,   3,  16,  20,  34,  20};
    skills_t xs = {  7,  15,  11,   0,   0,   0,   6,   7};

        me.name = "Mage";
        me.desc = "A Mage is a spellcaster who must live by his wits, as he cannot "
                    "hope to simply hack his way through the dungeon like a warrior. "
                    "A Mage always carries his spellbooks with him, but also relies "
                    "on magical devices, which he can master easily. The primary "
                    "spellcasting statistic of a Mage is Intelligence.\n\n"
                    "Mages have few restrictions in choosing and learning spells; "
                    "they can freely choose any two realms when a character is created, "
                    "although their natural inclinations make Life magic fairly hard to learn. "
                    "See <link:magic.txt> for more information on magic, realms and book spellcasting.";

        me.stats[A_STR] = -4;
        me.stats[A_INT] =  3;
        me.stats[A_WIS] =  0;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] = -2;
        me.stats[A_CHR] =  1;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 95;
        me.base_hp = 0;
        me.exp = 130;
        me.pets = 30;
        me.flags = CLASS_SENSE1_MED | CLASS_SENSE1_WEAK |
                   CLASS_SENSE2_FAST | CLASS_SENSE2_STRONG;
        
        me.birth = _birth;
        me.caster_info = _caster_info;
        /* TODO: This class uses spell books, so we are SOL
        me.get_spells = _get_spells;*/
        me.character_dump = spellbook_character_dump;
        me.get_powers = _get_powers;
        init = TRUE;
    }

    return &me;
}
