#include "angband.h"

void _double_magic_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Double Magic");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!can_do_cmd_cast()) return;
        handle_stuff();
        do_cmd_cast();
        handle_stuff();
        if (!p_ptr->paralyzed && can_do_cmd_cast())
            do_cmd_cast();
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 48;
    spell->cost = 20;
    spell->fail = 0;
    spell->fn = _double_magic_spell;

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
        me.weight = 430;
        me.min_fail = 5;
        me.options = CASTER_GLOVE_ENCUMBRANCE;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    int i;
    for (i = 0; i < 64; i++)
        p_ptr->spell_exp[i] = SPELL_EXP_EXPERT;

    py_birth_obj_aux(TV_SWORD, SV_SHORT_SWORD, 1);
    py_birth_obj_aux(TV_SOFT_ARMOR, SV_HARD_LEATHER_ARMOR, 1);
    py_birth_obj_aux(TV_ARCANE_BOOK, 0, 1);
}

class_t *red_mage_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  34,  34,   1,  16,  10,  56,  25};
    skills_t xs = {  7,  11,  11,   0,   0,   0,  18,  11};

        me.name = "Red-Mage";
        me.desc = "Red Mages are similar to Warrior-Mage; they are decent fighters "
                    "and spellcasters. Red-Mages can use almost all spells from lower "
                    "rank spellbooks of all realms, but they cannot cast spells from "
                    "higher rank spellbooks, and they are extremely slow learners in "
                    "them. They are not bad at using magical devices and magic "
                    "resistance, but are bad at other skills. A red-mage's prime "
                    "statistic is intelligence.\n \n"
                    "Red-Mages can use almost all spells from lower rank spellbooks of "
                    "all realms: first and second spellbooks of all realms and third "
                    "and fourth Arcane spellbooks, without having to learn it, but they "
                    "cannot cast spells from higher rank spellbooks; third and fourth "
                    "spellbooks for all realms other than Arcane. Since they use all "
                    "realms at once, they have large penalties in the mana costs, "
                    "minimum levels, and failure rates of spells. They have a class "
                    "power - 'Double Magic' - which allows them to cast two spells at "
                    "once.";

        me.stats[A_STR] =  2;
        me.stats[A_INT] =  2;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] =  0;
        me.stats[A_CHR] = -1;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 106;
        me.base_hp = 8;
        me.exp = 140;
        me.pets = 40;
        
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
