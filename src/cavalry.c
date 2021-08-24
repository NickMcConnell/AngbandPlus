#include "angband.h"

#include <assert.h>

void rodeo_spell(int cmd, var_ptr res)
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
        bool tame_success = FALSE;

        var_set_bool(res, FALSE);
        if (p_ptr->riding)
        {
            msg_print("You are already riding.");
            return;
        }
        if (!do_riding(TRUE)) return;
        
        var_set_bool(res, TRUE);
        assert(p_ptr->riding);

        m_ptr = plr_riding_mon();
        assert(m_ptr);
        r_ptr = mon_race(m_ptr);

        monster_desc(m_name, m_ptr, 0);
        cmsg_format(TERM_L_GREEN, "You ride on %s.", m_name);
        if (is_pet(m_ptr)) break;
        rlev = r_ptr->level;
        if (r_ptr->flags1 & RF1_UNIQUE) rlev = rlev * 3 / 2;
        if (rlev > 60) rlev = 60+(rlev-60)/2;

        if ((r_ptr->flags7 & RF7_GUARDIAN) || (r_ptr->flagsx & (RFX_QUESTOR | RFX_GUARDIAN)))
        {
            cmsg_format(TERM_RED, "It is impossible to tame %s!", m_name);
            tame_success = FALSE;
        }
        else if (!((skills_riding_current() / 120 + p_ptr->lev * 2 / 3) > rlev
          && rlev < p_ptr->lev * 3 / 2 + (p_ptr->lev / 5)))
        {
            cmsg_format(TERM_RED, "You are not powerful enough to tame %s.", m_name);
            tame_success = FALSE;
        }
        else
        {
            if (0 || p_ptr->wizard)
            {
                int r1 = skills_riding_current()/120 * p_ptr->lev*2/3;
                double p1 = 1.0 - (double)rlev/r1;
                double p2 = 0.5;
                double p3 = 1.0;
                if (rlev < p_ptr->lev*3/2)
                    p3 = 1.0;
                else if (rlev >= p_ptr->lev*3/2 + p_ptr->lev/5 - 1)
                    p3 = 0.0;
                else
                {
                    int b3 = rlev - p_ptr->lev*3/2;
                    int r3 = p_ptr->lev/5;
                    p3 = (double)b3/r3;
                }
                /* all 3 tests need to pass for rodeo to succeed */
                msg_format("<color:D>Rodeo = %.2f%% * %.2f%% * %.2f%% = %.2f%%.</color>",
                    p1*100., p2*100., p3*100., p1*p2*p3*100.);
            }
            if (!( randint1(skills_riding_current() / 120 + p_ptr->lev * 2 / 3) > rlev
                && one_in_(2) 
                && rlev < p_ptr->lev * 3 / 2 + randint0(p_ptr->lev / 5) ))
            {
                // No message here, but still the "you have been thrown off" later down.
                tame_success = FALSE;
            }
            else
            {
                tame_success = TRUE;
            }
        }
        if (tame_success)
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

static void _calc_shooter_bonuses(object_type *o_ptr, shooter_info_t *info_ptr)
{
    if (p_ptr->shooter_info.tval_ammo != TV_ARROW )
        p_ptr->shooter_info.base_shot = 100;
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 10;
    spell->cost = 0;
    spell->fail = calculate_fail_rate(spell->level, 50, p_ptr->stat_ind[A_STR]);
    spell->fn = rodeo_spell;

    return ct;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_POLEARM, SV_BROAD_SPEAR, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL, 1);
    plr_birth_obj_aux(TV_BOW, SV_SHORT_BOW, 1);
    plr_birth_obj_aux(TV_ARROW, SV_ARROW, rand_range(15, 25));
}

plr_class_ptr cavalry_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  18,  32,   1,  16,  10,  60,  66};
    skills_t xs = { 10,   7,  10,   0,   0,   0,  22,  26};

        me = plr_class_alloc(CLASS_CAVALRY);
        me->name = "Cavalry";
        me->desc = "Cavalry ride on horses into battle. Although they cannot cast "
                    "spells, they are proud of their overwhelming offensive strength on "
                    "horseback. They are good at shooting. At high levels, they learn "
                    "to forcibly saddle and tame wild monsters. Since they take pride "
                    "in the body and the soul, they don't use magical devices well.\n \n"
                    "Like Warriors and Archers, the cavalry don't use magic. Since "
                    "they are very good at riding, they have a class power - 'Rodeo' - "
                    "which allows them to forcibly saddle and tame wild monsters.";

        me->stats[A_STR] =  2;
        me->stats[A_INT] = -2;
        me->stats[A_WIS] = -2;
        me->stats[A_DEX] =  2;
        me->stats[A_CON] =  2;
        me->stats[A_CHR] =  1;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 111;
        me->base_hp = 10;
        me->exp = 120;
        me->pets = 35;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_STRONG;
        
        me->hooks.birth = _birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_shooter_bonuses = _calc_shooter_bonuses;
        me->hooks.get_powers = _get_powers;
        me->hooks.get_flags = _get_flags;
    }

    return me;
}
