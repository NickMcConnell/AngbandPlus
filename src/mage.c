#include "angband.h"

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 25;
    spell->cost = 1;
    spell->fail = calculate_fail_rate(spell->level, 90, plr->stat_ind[A_INT]);
    spell->fn = eat_magic_spell;

    return ct;
}

void _calc_bonuses(void)
{
    if (plr->lev >= 30)
        plr->wizard_sight = TRUE;
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
        me.realm1_choices = CH_LIFE | CH_SORCERY | CH_NATURE | CH_CHAOS | CH_DEATH | CH_TRUMP |
            CH_ARCANE | CH_ENCHANT | CH_DAEMON | CH_CRUSADE | CH_ARMAGEDDON | CH_ILLUSION;
        me.realm2_choices = me.realm1_choices;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_DAGGER, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_ROBE, 1);
    plr_birth_spellbooks();
}

plr_class_ptr mage_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  40,  38,   3,  16,  20,  34,  20};
    skills_t xs = { 35,  75,  55,   0,   0,   0,  30,  35};

        me = plr_class_alloc(CLASS_MAGE);
        me->name = "Mage";
        me->desc = "A Mage is a spell caster that must live by his wits as he cannot "
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

        me->stats[A_STR] = -4;
        me->stats[A_INT] =  3;
        me->stats[A_WIS] =  0;
        me->stats[A_DEX] =  1;
        me->stats[A_CON] = -2;
        me->stats[A_CHR] = -2;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 95;
        me->base_hp = 0;
        me->exp = 130;
        me->pets = 30;
        me->flags = CLASS_SENSE1_MED | CLASS_SENSE1_WEAK |
                    CLASS_SENSE2_FAST | CLASS_SENSE2_STRONG | CLASS_MAGE_BONUS;
        
        me->hooks.birth = _birth;
        me->hooks.caster_info = _caster_info;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.character_dump = spellbook_character_dump;
        me->hooks.get_powers = _get_powers;
    }

    return me;
}
