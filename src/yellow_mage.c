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
            CH_ARCANE | CH_ENCHANT | CH_DAEMON | CH_CRUSADE | CH_ARMAGEDDON;
        me.realm2_choices = me.realm1_choices;
        init = TRUE;
    }
    return &me;
}

static void _calc_bonuses(void)
{
    plr->spells_per_round += plr_prorata_level(100);
    if (plr->lev >= 30)
        plr->wizard_sight = TRUE;
}


static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_DAGGER, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_ROBE, 1);
    plr_birth_spellbooks();
}

plr_class_ptr yellow_mage_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  40,  38,   3,  16,  20,  34,  20};
    skills_t xs = { 35,  75,  55,   0,   0,   0,  30,  35};

        me = plr_class_alloc(CLASS_YELLOW_MAGE);
        me->name = "Yellow-Mage";
        me->desc = "A Yellow Mage is a type of Mage who focuses on rapid spell casting. "
                    "Much as the Warrior gets multiple attacks per round, the Yellow Mage "
                    "may cast multiple spells per round. Their spell speed increases with "
                    "level. In addition, they may also cast low level spells more quickly "
                    "as they gain in experience (For example, a CL50 Yellow Mage casting a "
                    "L35 spell only requires 85% of the normal time, and this bonus applies "
                    "before their spells per round bonus).\n \n"
                    "In all other respects, the Yellow Mage is similar to the Mage. They "
                    "may learn spells from two spell realms and use Intelligence as their "
                    "primary spell statistic. However, due to their focus upon hasty "
                    "casting, they are unable to learn the mightiest of spells in most realms, "
                    "especially as regards powerful offensive spells like Mana Storm.";

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
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.caster_info = _caster_info;
        me->hooks.character_dump = spellbook_character_dump;
        me->hooks.get_powers = _get_powers;
    }

    return me;
}
