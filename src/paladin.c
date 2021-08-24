#include "angband.h"

static void _calc_bonuses(void)
{
    if (plr->lev >= 40)
        res_add(GF_FEAR);
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (plr->lev >= 40)
        add_flag(flgs, OF_RES_(GF_FEAR));
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;
    spell_info* spell = &spells[ct++];

    if (is_good_realm(plr->realm1))
    {
        spell->level = 30;
        spell->cost = 30;
        spell->fail = calculate_fail_rate(spell->level, 70, plr->stat_ind[A_WIS]);
        spell->fn = holy_lance_spell;
    }
    else
    {
        spell->level = 30;
        spell->cost = 30;
        spell->fail = calculate_fail_rate(spell->level, 70, plr->stat_ind[A_WIS]);
        spell->fn = hell_lance_spell;
    }

    return ct;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "prayer";
        me.which_stat = A_WIS;
        me.encumbrance.max_wgt = 450;
        me.encumbrance.weapon_pct = 20;
        me.encumbrance.enc_wgt = 1200;
        me.min_fail = 5;
        me.realm1_choices = CH_CRUSADE | CH_DEATH | CH_LIFE | CH_DAEMON;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_BROAD_SWORD, 1);
    plr_birth_obj_aux(TV_HARD_ARMOR, SV_RING_MAIL, 1);
    plr_birth_spellbooks();
}

plr_class_ptr paladin_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  24,  34,   1,  12,   2,  68,  40};
    skills_t xs = { 35,  50,  55,   0,   0,   0, 105,  90};

        me = plr_class_alloc(CLASS_PALADIN);
        me->name = "Paladin";
        me->desc = "A Paladin is a combination of a warrior and a priest. Paladins "
                    "are very good fighters, but not very good at missile weapons. A "
                    "paladin lacks much in the way of abilities. He is poor at "
                    "stealth, perception, searching, and magical devices but has a "
                    "decent saving throw due to his divine alliance. Wisdom determines "
                    "a Paladin's success at praying to his deity.\n \n"
                    "Paladins can select a realm from Life, Crusade, Daemon and Death. "
                    "Like priests, they cannot select which prayers to learn, but are "
                    "rewarded with new prayers by their deities. They can learn all spells, "
                    "but not as fast as priests. They detest paganism so strongly that they "
                    "will even gain experience for destroying high-level pagan spellbooks: "
                    "'pagan' means Life or Crusade spellbooks for a Death/Daemon "
                    "Paladin and all spellbooks other than Life or Crusade for a "
                    "Life/Crusade Paladin. Depending on their realm, they have a class "
                    "power - 'Holy Lance' or 'Hell Lance'.";

        me->stats[A_STR] =  3;
        me->stats[A_INT] = -3;
        me->stats[A_WIS] =  1;
        me->stats[A_DEX] =  0;
        me->stats[A_CON] =  2;
        me->stats[A_CHR] =  2;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 111;
        me->base_hp = 12;
        me->exp = 135;
        me->pets = 40;
        me->flags = CLASS_SENSE1_SLOW | CLASS_SENSE1_STRONG |
                    CLASS_SENSE2_STRONG;
        
        me->hooks.birth = _birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = spellbook_character_dump;
        me->hooks.get_flags = _get_flags;
    }

    return me;
}
