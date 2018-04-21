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
        me.weight = 430;
        me.options = CASTER_GLOVE_ENCUMBRANCE;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    py_birth_obj_aux(TV_SWORD, SV_SHORT_SWORD, 1);
    py_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    py_birth_spellbooks();
}

class_t *warrior_mage_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  35,  36,   2,  18,  16,  50,  25};
    skills_t xs = {  7,  10,  10,   0,   0,   0,  15,  11};

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
        me.stats[A_CHR] =  1;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 106;
        me.base_hp = 8;
        me.exp = 140;
        me.pets = 35;
        
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
