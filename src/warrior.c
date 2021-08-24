#include "angband.h"

static void _calc_bonuses(void)
{
    if (plr->lev >= 30)
        res_add(GF_FEAR);
    plr->regen += 2 * plr->lev;
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (plr->lev >= 30)
        add_flag(flgs, OF_RES_(GF_FEAR));
    add_flag(flgs, OF_REGEN);
}

static void _calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    info->to_d += plr->lev/5;
    info->dis_to_d += plr->lev/5;
    info->xtra_blow += plr_prorata_level_aux(100, 0, 1, 1);
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 30;
    spell->cost = 25;
    spell->fail = calculate_fail_rate(spell->level, 80, plr->stat_ind[A_DEX]);
    spell->fn = sword_dance_spell;

    return ct;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_BROAD_SWORD, 1);
    plr_birth_obj_aux(TV_HARD_ARMOR, SV_CHAIN_MAIL, 1);
    plr_birth_obj_aux(TV_BOW, SV_SHORT_BOW, 1);
    plr_birth_obj_aux(TV_ARROW, SV_ARROW, rand_range(15, 30));
}

plr_class_ptr warrior_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  18,  31,   1,  14,   2,  70,  55};
    skills_t xs = { 35,  35,  50,   0,   0,   0, 150, 150};

        me = plr_class_alloc(CLASS_WARRIOR);
        me->name = "Warrior";
        me->desc = "A Warrior is a hack-and-slash character, who solves most of his "
                    "problems by cutting them to pieces, but will occasionally fall "
                    "back on the help of a magical device. Unfortunately, many "
                    "high-level devices may be forever beyond their use.\n \n"
                    "Warriors cast no spells. They hate magic. In fact, they even "
                    "gain experience for destroying high level spellbooks. They have a "
                    "class power - 'Sword Dancing' - which allows them to conduct a "
                    "melee attack in six random directions.";

        me->stats[A_STR] =  4;
        me->stats[A_INT] = -2;
        me->stats[A_WIS] = -2;
        me->stats[A_DEX] =  2;
        me->stats[A_CON] =  2;
        me->stats[A_CHR] =  1;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 115;
        me->base_hp = 18;
        me->exp = 100;
        me->pets = 40;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_STRONG;

        me->hooks.birth = _birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.get_powers = _get_powers;
        me->hooks.get_flags = _get_flags;
    }

    return me;
}
