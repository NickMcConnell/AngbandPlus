#include "angband.h"

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 25;
    spell->cost = 1;
    spell->fail = calculate_fail_rate(spell->level, 90, p_ptr->stat_ind[A_INT]);
    spell->fn = eat_magic_spell;

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
        me.options = CASTER_ALLOW_DEC_MANA | CASTER_GLOVE_ENCUMBRANCE;
        init = TRUE;
    }
    return &me;
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
        me.desc = "A Mage is a spell caster that must live by his wits as he cannot "
                    "hope to simply hack his way through the dungeon like a warrior. "
                    "In addition to his spellbooks, a Mage should carry a range of "
                    "magical devices to help him in his endeavors which he can master "
                    "far more easily than anyone else. A Mage's prime statistic is "
                    "Intelligence as this determines his spell casting ability.\n \n"
                    "Mages have the least restrictions in choosing and learning spells. "
                    "They can freely choose any two realms when a character is created. "
                    "Their natural inclination makes Life magic fairly hard to learn. "
                    "Otherwise, a mage tends to learn and cast all the spells in his or "
                    "her realms better than any other character. The ability to choose "
                    "second realm of magic has a special meaning: Only the "
                    "second realm can be changed in the middle of the game. You can "
                    "change second realm by studying ('G') from a spellbook of new "
                    "realm. They have a class power - 'Eat Magic' - which absorbs mana "
                    "from wands, staves or rods.";

        me.stats[A_STR] = -4;
        me.stats[A_INT] =  3;
        me.stats[A_WIS] =  0;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] = -2;
        me.stats[A_CHR] = -2;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 95;
        me.base_hp = 0;
        me.exp = 130;
        me.pets = 30;
        
        me.caster_info = _caster_info;
        /* TODO: This class uses spell books, so we are SOL
        me.get_spells = _get_spells;*/
        me.character_dump = spellbook_character_dump;
        me.get_powers = _get_powers;
        init = TRUE;
    }

    return &me;
}
