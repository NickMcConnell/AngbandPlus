#include "angband.h"

static void _calc_bonuses(void)
{
    if (p_ptr->lev >= 30)
        res_add(RES_FEAR);
    p_ptr->regen += 2 * p_ptr->lev;
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (p_ptr->lev >= 30)
        add_flag(flgs, OF_RES_FEAR);
    if (p_ptr->lev >= 45)
        add_flag(flgs, OF_REGEN);
}

static void _calc_weapon_bonuses(object_type *o_ptr, weapon_info_t *info_ptr)
{
    info_ptr->to_d += p_ptr->lev/5;
    info_ptr->dis_to_d += p_ptr->lev/5;
    info_ptr->xtra_blow += p_ptr->lev*2;
}

static void _calc_shooter_bonuses(object_type *o_ptr, shooter_info_t *info_ptr)
{
    if ( !p_ptr->shooter_info.heavy_shoot
      && info_ptr->tval_ammo <= TV_BOLT 
      && info_ptr->tval_ammo >= TV_SHOT )
    {
        p_ptr->shooter_info.num_fire += p_ptr->lev * 100 / 50;
    }
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 40;
    spell->cost = 75;
    spell->fail = calculate_fail_rate(spell->level, 80, p_ptr->stat_ind[A_DEX]);
    spell->fn = sword_dance_spell;

    return ct;
}

class_t *warrior_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  31,   1,  14,   2,  70,  55};
    skills_t xs = { 12,   7,  10,   0,   0,   0,  30,  30};

        me.name = "Warrior";
        me.desc = "A Warrior is a hack-and-slash character, who solves most of his "
                    "problems by cutting them to pieces, but will occasionally fall "
                    "back on the help of a magical device. Unfortunately, many "
                    "high-level devices may be forever beyond their use.\n \n"
                    "Warriors cast no spells. They hate magic. In fact, they even "
                    "gain experience for destroying high level spellbooks. They have a "
                    "class power - 'Sword Dancing' - which allows them to conduct a "
                    "melee attack in six random directions.";

        me.stats[A_STR] =  4;
        me.stats[A_INT] = -2;
        me.stats[A_WIS] = -2;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] =  2;
        me.stats[A_CHR] =  1;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 115;
        me.base_hp = 18;
        me.exp = 100;
        me.pets = 40;
        
        me.calc_bonuses = _calc_bonuses;
        me.calc_weapon_bonuses = _calc_weapon_bonuses;
        me.calc_shooter_bonuses = _calc_shooter_bonuses;
        me.get_powers = _get_powers;
        me.get_flags = _get_flags;
        init = TRUE;
    }

    return &me;
}
