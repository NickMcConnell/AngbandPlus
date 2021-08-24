#include "angband.h"

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 25;
    spell->cost = 1;
    spell->fail = calculate_fail_rate(spell->level, 90, plr->stat_ind[A_CHR]);
    spell->fn = eat_magic_spell;

    return ct;
}

static void _calc_bonuses(void)
{
    plr->to_a -= 50;
    plr->dis_to_a -= 50;
    plr->wizard_sight = TRUE;
}

static void _calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    if (object_is_(obj, TV_HAFTED, SV_WIZSTAFF))
    {
        info->to_h -= 30;
        info->to_d -= 10;
        info->dis_to_h -= 30;
        info->dis_to_d -= 10;
    }
    else
    {
        info->to_h -= 200;
        info->to_d -= 200;
        info->dis_to_h -= 200;
        info->dis_to_d -= 200;
        add_flag(info->paf_flags, PAF_ICKY);
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
        me.encumbrance.max_wgt = 40;
        me.encumbrance.weapon_pct = 100;
        me.encumbrance.enc_wgt = 900;
        me.options = CASTER_ALLOW_DEC_MANA | CASTER_GLOVE_ENCUMBRANCE;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    int i;
    for (i = 0; i < 64; i++)
        plr->spell_exp[i] = SPELL_EXP_MASTER;

    plr_birth_obj_aux(TV_HAFTED, SV_WIZSTAFF, 1);
    plr_birth_obj_aux(TV_WAND, EFFECT_BOLT_MISSILE, 1);
    plr_birth_obj_aux(TV_POTION, SV_POTION_CLARITY, rand_range(10, 20));

    for (i = TV_LIFE_BOOK; i < TV_LIFE_BOOK + MAX_MAGIC; i++)
    {
        if (i == TV_NECROMANCY_BOOK) continue;
        plr_birth_obj_aux(i, 0, 1);
    }
}

plr_class_ptr sorcerer_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  48,  75,   2,  12,  22,   0,   0};
    skills_t xs = { 35,  90,  65,   0,   0,   0,   0,   0};

        me = plr_class_alloc(CLASS_SORCERER);
        me->name = "Sorcerer";
        me->desc = "Sorcerers are the all-around best magicians, being able to cast "
                    "any spell from most magic realms without having to learn it. On "
                    "the downside, they are the worst fighters in the dungeon, being "
                    "unable to use any weapon but a Wizardstaff.\n \n"
                    "Sorcerers can cast any spell from any spellbooks of all magic "
                    "realms with 'Master' proficiency level without having to learn it. "
                    "They have a class power - 'Eat Magic' - which absorbs mana from "
                    "wands, staves or rods.";

        me->stats[A_STR] = -5;
        me->stats[A_INT] =  0;
        me->stats[A_WIS] = -2;
        me->stats[A_DEX] =  2;
        me->stats[A_CON] =  0;
        me->stats[A_CHR] =  6;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 65;
        me->base_hp = 0;
        me->exp = 160;
        me->pets = 25;
        me->flags = CLASS_SENSE1_MED | CLASS_SENSE1_WEAK |
                    CLASS_SENSE2_FAST | CLASS_SENSE2_STRONG | CLASS_MAGE_BONUS;
        
        me->hooks.birth = _birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = spellbook_character_dump;
    }

    return me;
}
