#include "angband.h"

static void _take_photo_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Take Photograph");
        break;
    case SPELL_DESC:
        var_set_string(res, "Creates something to show the kids back home!");
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, TRUE);
        if (!get_aim_dir(&dir)) return;
        project_length = 1;
        fire_beam(GF_PHOTO, dir, 1);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 1;
    spell->cost = 0;
    spell->fail = 0;
    spell->fn = _take_photo_spell;

    spell = &spells[ct++];
    spell->level = 25;
    spell->cost = 20;
    spell->fail = calculate_fail_rate(spell->level, 30, p_ptr->stat_ind[A_INT]);
    spell->fn = identify_fully_spell;

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
        me.weight = 450;
        me.min_fail = 5;
        me.min_level = 5;
        me.options = CASTER_GLOVE_ENCUMBRANCE;
        init = TRUE;
    }
    return &me;
}

class_t *tourist_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 15,  18,  28,   1,  12,   2,  48,  20};
    skills_t xs = {  5,   7,   9,   0,   0,   0,  13,  11};

        me.name = "Tourist";
        me.desc = "Tourists have visited this world for the purpose of sightseeing. "
                    "Their fighting skills are bad and they cannot cast powerful "
                    "spells. They are the most difficult class to win the game with. "
                    "Intelligence determines a tourist's spell casting ability.\n \n"
                    "Tourists are always seeing more of the world to add to their stock "
                    "of information; no other class can compete with their "
                    "identification skills. They have two class powers - 'Take a "
                    "Photograph' and 'Identify True'. Their magic is based on Arcane, "
                    "and - aside from identify - is very weak indeed.";

        me.stats[A_STR] = -1;
        me.stats[A_INT] = -1;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] = -1;
        me.stats[A_CON] = -1;
        me.stats[A_CHR] = -3;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 95;
        me.base_hp = 0;
        me.exp = 70;
        me.pets = 40;
        
        me.caster_info = _caster_info;
        /* TODO: This class uses spell books, so we are SOL
        me.get_spells = _get_spells;*/
        me.get_powers = _get_powers;
        me.character_dump = spellbook_character_dump;
        init = TRUE;
    }

    return &me;
}
