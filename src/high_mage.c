#include "angband.h"

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];

    if (p_ptr->realm1 == REALM_HEX)
    {
        spell->level = 1;
        spell->cost = 0;
        spell->fail = 0;
        spell->fn = hex_stop_spelling_spell;
    }
    else
    {
        spell->level = 25;
        spell->cost = 1;
        spell->fail = calculate_fail_rate(spell->level, 90, p_ptr->stat_ind[A_INT]);
        spell->fn = eat_magic_spell;
    }
    return ct;
}

static void _calc_bonuses(void)
{
    p_ptr->spell_cap += 3;
    p_ptr->to_d_spell += 5 + p_ptr->lev/5;
/*  p_ptr->spell_power += 2; 
    p_ptr->device_power += 2; */
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SPELL_CAP);
/*  add_flag(flgs, TR_SPELL_POWER);
    add_flag(flgs, TR_MAGIC_MASTERY); */
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

class_t *high_mage_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  40,  38,   3,  16,  20,  34,  20};
    skills_t xs = {  7,  15,  11,   0,   0,   0,   6,   7};

        me.name = "High-Mage";
        me.desc = "High-mages are mages who specialize in one particular field of "
                    "magic and learn it very well - much better than the ordinary mage. "
                    "A high mage's prime statistic is intelligence as this determines "
                    "his spell casting ability.\n \n"
                    "For the price of giving up a second realm of magic, High-mages "
                    "gain substantial benefits in the mana costs, power, minimum levels, and "
                    "failure rates of the spells in their speciality realm. They have "
                    "a class power - 'Eat Magic' - which absorbs mana from wands, "
                    "staves, or rods.\n \n"
                    "And then, only High-Mages are able to cast Hex spells.";

        me.stats[A_STR] = -4;
        me.stats[A_INT] =  4;
        me.stats[A_WIS] =  0;
        me.stats[A_DEX] =  0;
        me.stats[A_CON] = -2;
        me.stats[A_CHR] = -2;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 94;
        me.base_hp = 0;
        me.exp = 130;
        me.pets = 25;
        
        me.calc_bonuses = _calc_bonuses;
        me.get_flags = _get_flags;
        me.caster_info = _caster_info;
        /* TODO: This class uses spell books, so we are SOL
        me.get_spells = _get_spells;*/
        me.get_powers = _get_powers;
        me.character_dump = spellbook_character_dump;
        init = TRUE;
    }

    return &me;
}
