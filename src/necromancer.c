#include "angband.h"

/* Stub ... At the moment, all the spells are in do-spell.c */

static void _calc_bonuses(void)
{
    p_ptr->align -= 200;
    p_ptr->spell_cap += 2;
    if (p_ptr->lev >= 5) res_add(RES_COLD);
    if (p_ptr->lev >= 15) p_ptr->see_inv = TRUE;
    if (p_ptr->lev >= 25) p_ptr->hold_life = TRUE;
    if (p_ptr->lev >= 35) res_add(RES_POIS);
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 1;
    spell->cost = 1;
    spell->fail = calculate_fail_rate(spell->level, 30, p_ptr->stat_ind[A_INT]);
    spell->fn = animate_dead_spell;

    spell = &spells[ct++];
    spell->level = 5;
    spell->cost = 5;
    spell->fail = calculate_fail_rate(spell->level, 30, p_ptr->stat_ind[A_INT]);
    spell->fn = enslave_undead_spell;

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

class_t *necromancer_get_class_t(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  40,  38,   4,  16,  20,  34,  20};
    skills_t xs = {  7,  15,  11,   0,   0,   0,   6,   7};

        me.name = "Necromancer";
        me.desc = "A Necromancer attempts to gain both power and knowledge through "
                  "communion with the dead. They use powerful necromancy magic to "
                  "summon aid from the dead, whether directly in terms of undead "
                  "servitude, or indirectly through other-worldly knowledge. Necromancy "
                  "also offers myriad foul offensive spells, but all of these require "
                  "the Necromancer to physically touch his foe. To do so, the Necromancer "
                  "may wield neither weapon, nor gloves. But a powerful necromancer is truly "
                  "awe inspiring, and may even kill foes with a single deadly touch! "
                  "In addition, they forever hunt for the legendary Eye and Hand of Vecna in "
                  "order to complete their power.",
        
        me.stats[A_STR] = -2;
        me.stats[A_INT] =  3;
        me.stats[A_WIS] = -4;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] = -1;
        me.stats[A_CHR] = -2;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 95;
        me.base_hp = 2;
        me.exp = 125;
        me.pets = 10;

        me.caster_info = _caster_info;
        me.calc_bonuses = _calc_bonuses;
        me.get_powers = _get_powers;
        me.character_dump = spellbook_character_dump;
        init = TRUE;
    }

    return &me;
}