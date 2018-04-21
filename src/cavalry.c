#include "angband.h"

void _rodeo_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Rodeo");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
    {
        char m_name[80];
        monster_type *m_ptr;
        monster_race *r_ptr;
        int rlev;

        var_set_bool(res, FALSE);
        if (p_ptr->riding)
        {
            msg_print("You are already riding.");
            return;
        }
        if (!do_riding(TRUE)) return;
        
        var_set_bool(res, TRUE);

        m_ptr = &m_list[p_ptr->riding];
        r_ptr = &r_info[m_ptr->r_idx];
        monster_desc(m_name, m_ptr, 0);
        cmsg_format(TERM_L_GREEN, "You ride on %s.", m_name);
        if (is_pet(m_ptr)) break;
        rlev = r_ptr->level;
        if (r_ptr->flags1 & RF1_UNIQUE) rlev = rlev * 3 / 2;
        if (rlev > 60) rlev = 60+(rlev-60)/2;
        if ( randint1(skills_riding_current() / 120 + p_ptr->lev * 2 / 3) > rlev
          && one_in_(2) 
          && !p_ptr->inside_arena 
          && !p_ptr->inside_battle
          && !(r_ptr->flags7 & RF7_GUARDIAN) 
          && !(r_ptr->flags1 & RF1_QUESTOR)
          && rlev < p_ptr->lev * 3 / 2 + randint0(p_ptr->lev / 5) )
        {
            cmsg_format(TERM_L_GREEN, "You tame %s.", m_name);
            set_pet(m_ptr);
        }
        else
        {
            cmsg_format(TERM_VIOLET, "You have been thrown off %s.", m_name);
            rakuba(1,TRUE);
            p_ptr->riding = 0;
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _calc_bonuses(void)
{
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
}

static void _calc_weapon_bonuses(object_type *o_ptr, weapon_info_t *info_ptr)
{
}

static void _calc_shooter_bonuses(object_type *o_ptr, shooter_info_t *info_ptr)
{
    if ( !p_ptr->shooter_info.heavy_shoot
      && p_ptr->shooter_info.tval_ammo == TV_ARROW )
    {
        p_ptr->shooter_info.num_fire += p_ptr->lev * 150 / 50;
    }
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 10;
    spell->cost = 0;
    spell->fail = calculate_fail_rate(spell->level, 50, p_ptr->stat_ind[A_STR]);
    spell->fn = _rodeo_spell;

    return ct;
}

class_t *cavalry_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  18,  32,   1,  16,  10,  60,  66};
    skills_t xs = { 10,   7,  10,   0,   0,   0,  22,  26};

        me.name = "Cavalry";
        me.desc = "Cavalry ride on horses into battle. Although they cannot cast "
                    "spells, they are proud of their overwhelming offensive strength on "
                    "horseback. They are good at shooting. At high levels, they learn "
                    "to forcibly saddle and tame wild monsters. Since they take pride "
                    "in the body and the soul, they don't use magical devices well.\n \n"
                    "Like Warriors and Archers, the cavalry don't use magic. Since "
                    "they are very good at riding, they have a class power - 'Rodeo' - "
                    "which allows them to forcibly saddle and tame wild monsters.";

        me.stats[A_STR] =  2;
        me.stats[A_INT] = -2;
        me.stats[A_WIS] = -2;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] =  2;
        me.stats[A_CHR] =  1;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 111;
        me.base_hp = 10;
        me.exp = 120;
        me.pets = 35;
        
        me.calc_bonuses = _calc_bonuses;
        me.calc_weapon_bonuses = _calc_weapon_bonuses;
        me.calc_shooter_bonuses = _calc_shooter_bonuses;
        me.get_powers = _get_powers;
        me.get_flags = _get_flags;
        init = TRUE;
    }

    return &me;
}
