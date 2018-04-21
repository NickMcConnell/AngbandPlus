#include "angband.h"

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 25;
    spell->cost = 1;
    spell->fail = calculate_fail_rate(spell->level, 90, p_ptr->stat_ind[A_CHR]);
    spell->fn = eat_magic_spell;

    return ct;
}

static void _calc_bonuses(void)
{
    p_ptr->to_a -= 50;
    p_ptr->dis_to_a -= 50;
}

static void _calc_weapon_bonuses(object_type *o_ptr, weapon_info_t *info_ptr)
{
    if ( object_is_(o_ptr, TV_HAFTED, SV_WIZSTAFF)
      || object_is_(o_ptr, TV_HAFTED, SV_NAMAKE_HAMMER) )
    {
        info_ptr->to_h -= 30;
        info_ptr->to_d -= 10;
        info_ptr->dis_to_h -= 30;
        info_ptr->dis_to_d -= 10;
    }
    else
    {
        info_ptr->to_h -= 200;
        info_ptr->to_d -= 200;
        info_ptr->dis_to_h -= 200;
        info_ptr->dis_to_d -= 200;
        info_ptr->icky_wield = TRUE;
    }
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "spell";
        me.which_stat = A_CHR;
        me.weight = 40;
        me.options = CASTER_ALLOW_DEC_MANA | CASTER_GLOVE_ENCUMBRANCE;
        init = TRUE;
    }
    return &me;
}

class_t *sorcerer_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  48,  75,   2,  12,  22,   0,   0};
    skills_t xs = {  7,  18,  13,   0,   0,   0,   0,   0};

        me.name = "Sorcerer";
        me.desc = "Sorcerers are the all-around best magicians, being able to cast "
                    "any spell from most magic realms without having to learn it. On "
                    "the downside, they are the worst fighters in the dungeon, being "
                    "unable to use any weapon but a Wizardstaff.\n \n"
                    "Sorcerers can cast any spell from any spellbooks of all magic "
                    "realms with 'Master' proficiency level without having to learn it. "
                    "They have a class power - 'Eat Magic' - which absorbs mana from "
                    "wands, staves or rods.";

        me.stats[A_STR] = -5;
        me.stats[A_INT] =  0;
        me.stats[A_WIS] = -2;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] =  0;
        me.stats[A_CHR] =  6;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 65;
        me.base_hp = 0;
        me.exp = 160;
        me.pets = 25;
        
        me.calc_bonuses = _calc_bonuses;
        me.calc_weapon_bonuses = _calc_weapon_bonuses;
        me.caster_info = _caster_info;
        /* TODO: This class uses spell books, so we are SOL
        me.get_spells = _get_spells;*/
        me.get_powers = _get_powers;
        me.character_dump = spellbook_character_dump;
        init = TRUE;
    }

    return &me;
}
