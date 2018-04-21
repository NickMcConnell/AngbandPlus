#include "angband.h"

/****************************************************************
 * Spells
 ****************************************************************/
static void _kiss_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Kiss");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to charm an adjacent monster.");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, p_ptr->lev * 2);
        break;
    case SPELL_CAST:
    {
        int y, x, dir = 0, m_idx;
        var_set_bool(res, FALSE);
        if (!get_rep_dir2(&dir)) return;
        if (dir == 5) return;

        y = py + ddy[dir];
        x = px + ddx[dir];

        m_idx = cave[y][x].m_idx;
        if (m_idx)
        {
            monster_type *m_ptr = &m_list[m_idx];
            monster_race *r_ptr = &r_info[m_ptr->r_idx];
            char desc[MAX_NLEN];
            monster_desc(desc, m_ptr, 0);
            if ((r_ptr->flags1 & RF1_UNIQUE) || mon_save_p(m_ptr->r_idx, A_CHR))
            {
                set_monster_csleep(m_idx, 0);
                if (is_hostile(m_ptr))
                {
                    switch (randint1(10))
                    {
                    case 1:
                        msg_format("%^s says 'Impudent Strumpet!'", desc);
                        break;
                    case 2:
                        msg_format("%^s says 'Ewwww! Gross!!'", desc);
                        break;
                    case 3:
                        msg_format("%^s says 'You ain't my type!'", desc);
                        break;
                    default:
                        msg_format("%^s resists your charms.", desc);
                    }

                    if (allow_ticked_off(r_ptr))
                    {
                        m_ptr->anger_ct++;
                    }

                }
                else
                    msg_format("%^s ignores you.", desc);
            }
            else
            {
                if (is_pet(m_ptr))
                    msg_format("%^s slobbers on you affectionately.", desc);
                else if (is_friendly(m_ptr))
                {
                    set_pet(m_ptr);
                    msg_format("%^s is charmed!", desc);
                }
                else
                {
                    set_friendly(m_ptr);
                    msg_format("%^s suddenly becomes friendly.", desc);
                }
            }
            var_set_bool(res, TRUE);
        }
        else
        {
            msg_print("There is no monster.");
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _demeter_clw_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Cure Wounds");
        break;
    case SPELL_DESC:
        var_set_string(res, "Heals cut and HP a little.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_heal(p_ptr->lev/12 + 1, 10, 0));
        break;
    case SPELL_CAST:
        if (p_ptr->pclass == CLASS_BLOOD_MAGE)
            msg_print("There is no effect.");
        else
        {
            hp_player(damroll(p_ptr->lev/12 + 1, 10));
            set_cut(p_ptr->cut - 10, TRUE);
        }
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _shine_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shine");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generates a large ball of sunlight.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, p_ptr->lev * 3));
        break;
    case SPELL_CAST:
        fire_ball(GF_LITE, 0, p_ptr->lev * 3 * 2, 3);
        var_set_bool(res, TRUE);
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, (p_ptr->lev - 20)/2);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

/****************************************************************
 * Demigod
 ****************************************************************/
static void _gain_power(int which)
{
    if (p_ptr->demigod_power[which] < 0)
    {
        int idx = mut_gain_choice(mut_demigod_pred);
        mut_lock(idx);
        p_ptr->demigod_power[which] = idx;
    }
    else if (!mut_present(p_ptr->demigod_power[which]))
    {
        mut_gain(p_ptr->demigod_power[which]);
        mut_lock(p_ptr->demigod_power[which]);
    }
}
static void _gain_level(int new_level)
{
    if (new_level >= 20)
        _gain_power(0);
    if (new_level >= 40)
        _gain_power(1);
}

void demigod_rechoose_powers(void)
{
    int i, idx;
    for (i = 0; i < MAX_DEMIGOD_POWERS; i++)
    {
        idx = p_ptr->demigod_power[i];
        if (idx >= 0)
        {
            mut_unlock(idx);
            mut_lose(idx);
            p_ptr->demigod_power[i] = -1;
        }
    }
    _gain_level(p_ptr->lev);
}

/****************************************************************
 * Aphrodite
 ****************************************************************/
static power_info _aphrodite_powers[] =
{
    { A_CHR, {1, 10, 50, _kiss_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static void _aphrodite_calc_bonuses(void)
{
    p_ptr->sustain_chr = TRUE;
}
static int _aphrodite_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _aphrodite_powers);
}
static void _aphrodite_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_SUST_CHR);
}

/****************************************************************
 * Apollo
 ****************************************************************/
static power_info _apollo_powers[] =
{
    { A_INT, { 5,  3, 50, light_area_spell}},
    { A_WIS, {12,  7, 60, ray_of_sunlight_spell}},
    { A_CHR, {20, 10, 40, _shine_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static void _apollo_calc_bonuses(void)
{
    res_add_immune(RES_LITE);
    res_add(RES_BLIND);
    /* cf calc_torch in xtra1.c for the 'extra light' */
}
static int _apollo_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _apollo_powers);
}
static void _apollo_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_BLIND);
    add_flag(flgs, TR_LITE);
}
static void _apollo_get_immunities(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_LITE);
}

/****************************************************************
 * Ares
 ****************************************************************/
static power_info _ares_powers[] =
{
    { A_STR, {10, 10, 30, berserk_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static void _ares_calc_bonuses(void)
{
    int dam = 1 + p_ptr->lev/7;
    int ac = 1 + p_ptr->lev/5;
    int hand;

    p_ptr->sustain_str = TRUE;

    p_ptr->to_a += ac;
    p_ptr->dis_to_a += ac;

    p_ptr->to_d_m  += dam;
    for (hand = 0; hand < MAX_HANDS; hand++)
    {
        if (p_ptr->weapon_info[hand].wield_how != WIELD_NONE)
        {
            p_ptr->weapon_info[hand].to_d += dam / p_ptr->weapon_ct;
            p_ptr->weapon_info[hand].dis_to_d += dam / p_ptr->weapon_ct;
        }
    }
}
static int _ares_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _ares_powers);
}
static void _ares_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_SUST_STR);
}

/****************************************************************
 * Artemis
 ****************************************************************/
static void _artemis_calc_bonuses(void)
{
    p_ptr->shooter_info.to_d += 1 + p_ptr->lev/7;
    p_ptr->shooter_info.dis_to_d += 1 + p_ptr->lev/7;
    p_ptr->sustain_dex = TRUE;
}
static void _artemis_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_SUST_DEX);
}

/****************************************************************
 * Athena
 ****************************************************************/
static void _athena_calc_bonuses(void)
{
    p_ptr->sustain_int = TRUE;
}
static void _athena_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_SUST_INT);
}

/****************************************************************
 * Demeter
 ****************************************************************/
static power_info _demeter_powers[] =
{
    { A_WIS, {5, 0, 60, _demeter_clw_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static void _demeter_calc_bonuses(void)
{
    p_ptr->regenerate = TRUE;
    p_ptr->slow_digest = TRUE;
    if (p_ptr->lev >= 40)
        res_add(RES_TIME);
}
static int _demeter_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _demeter_powers);
}
static void _demeter_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_REGEN);
    add_flag(flgs, TR_SLOW_DIGEST);
    if (p_ptr->lev >= 40)
        add_flag(flgs, TR_RES_TIME);
}

/****************************************************************
 * Hades
 ****************************************************************/
static void _hades_calc_bonuses(void)
{
    res_add(RES_NETHER);
    p_ptr->hold_life = TRUE;
    p_ptr->sustain_con = TRUE;
}
static void _hades_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_NETHER);
    add_flag(flgs, TR_HOLD_LIFE);
    add_flag(flgs, TR_SUST_CON);
}

/****************************************************************
 * Hephaestus
 ****************************************************************/
static void _hephaestus_calc_bonuses(void)
{
    res_add(RES_DISEN);
}
static void _hephaestus_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_DISEN);
}

/****************************************************************
 * Hera
 ****************************************************************/
static power_info _hera_powers[] =
{
    { A_WIS, {15, 0, 30, clear_mind_spell}},
    { -1, {-1, -1, -1, NULL} }
};
static void _hera_calc_bonuses(void)
{
    p_ptr->spell_cap += 2;
    if (p_ptr->lev >= 15)
        p_ptr->clear_mind = TRUE;
}
static int _hera_get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _hera_powers);
}
static void _hera_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_SPELL_CAP);
}

/****************************************************************
 * Hermes
 ****************************************************************/
static void _hermes_calc_bonuses(void)
{
    p_ptr->pspeed += 5 * p_ptr->lev/50;
}
static void _hermes_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    if (p_ptr->lev >= 10)
        add_flag(flgs, TR_SPEED);
}

/****************************************************************
 * Poseidon
 ****************************************************************/
static void _poseidon_calc_bonuses(void)
{
    p_ptr->melt_armor = TRUE;
    if (p_ptr->lev >= 5)
        res_add(RES_ACID);
    if (p_ptr->lev >= 10)
        res_add(RES_COLD);
    if (p_ptr->lev >= 20)
        res_add(RES_ELEC);
}
static void _poseidon_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    if (p_ptr->lev >= 5)
        add_flag(flgs, TR_RES_ACID);
    if (p_ptr->lev >= 10)
        add_flag(flgs, TR_RES_COLD);
    if (p_ptr->lev >= 20)
        add_flag(flgs, TR_RES_ELEC);
}
/****************************************************************
 * Zeus
 ****************************************************************/
static void _zeus_calc_bonuses(void)
{
    res_add(RES_ELEC);
    p_ptr->sh_elec = TRUE;
}
static void _zeus_get_flags(u32b flgs[TR_FLAG_SIZE])
{
    add_flag(flgs, TR_RES_ELEC);
    add_flag(flgs, TR_SH_ELEC);
}

race_t *demigod_get_race_t(int psubrace)
{
    static race_t me = {0};
    static bool init = FALSE;
    static int subrace_init = -1;

    /* static info never changes */
    if (!init)
    {
        me.name = "Demigod";
        me.desc = "The term demigod is commonly used to describe mythological figures whose one "
                    "parent was a god and whose other parent was human; as such, demigods are "
                    "human-god hybrids and are quite powerful. Demigods receive special abilities "
                    "depending on their parentage.";
        
        me.infra = 0;

        me.gain_level = _gain_level;
        init = TRUE;
    }

    if (subrace_init != psubrace)
    {
        int i;
        
        /* Reset to Minor God */
        me.stats[A_STR] =  1;
        me.stats[A_INT] =  1;
        me.stats[A_WIS] =  1;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] =  1;
        me.stats[A_CHR] =  1;
        
        me.skills.dis =  2;
        me.skills.dev =  3;
        me.skills.sav =  1;
        me.skills.stl = -1;
        me.skills.srh =  0;
        me.skills.fos =  7;
        me.skills.thn = 10;
        me.skills.thb =  5;

        me.life = 100;
        me.base_hp = 20;
        me.exp = 180;

        me.calc_bonuses = NULL;
        me.get_powers = NULL;
        me.get_flags = NULL;
        me.get_immunities = NULL;

        /* Override with New Type */
        switch (psubrace)
        {
        case DEMIGOD_APHRODITE:
            me.stats[A_CHR] += 2;
            me.exp += 40;
            me.skills.dev += 2;
            me.calc_bonuses = _aphrodite_calc_bonuses;
            me.get_powers = _aphrodite_get_powers;
            me.get_flags = _aphrodite_get_flags;
            break;
        case DEMIGOD_APOLLO:
            me.exp += 50;
            me.skills.dev += 3;
            me.calc_bonuses = _apollo_calc_bonuses;
            me.get_powers = _apollo_get_powers;
            me.get_flags = _apollo_get_flags;
            me.get_immunities = _apollo_get_immunities;
            break;
        case DEMIGOD_ARES:
            me.stats[A_STR] += 2;
            me.skills.dev -= 3;
            me.skills.sav -= 5;
            me.skills.stl -= 1;
            me.skills.thn += 15;
            me.exp += 60;
            me.calc_bonuses = _ares_calc_bonuses;
            me.get_powers = _ares_get_powers;
            me.get_flags = _ares_get_flags;
            break;
        case DEMIGOD_ARTEMIS:
            me.stats[A_DEX] += 2;
            me.skills.thb += 15;
            me.exp += 50;
            me.calc_bonuses = _artemis_calc_bonuses;
            me.get_flags = _artemis_get_flags;
            break;
        case DEMIGOD_ATHENA:
            me.stats[A_INT] += 2;
            me.exp += 60;
            me.skills.dev += 10;
            me.skills.sav += 2;
            me.calc_bonuses = _athena_calc_bonuses;
            me.get_flags = _athena_get_flags;
            break;
        case DEMIGOD_DEMETER:
            me.exp += 40;
            me.skills.sav += 5;
            me.calc_bonuses = _demeter_calc_bonuses;
            me.get_powers = _demeter_get_powers;
            me.get_flags = _demeter_get_flags;
            break;
        case DEMIGOD_HADES:
            me.stats[A_CON] += 2;
            me.skills.sav += 10;
            me.skills.thn += 5;
            me.skills.stl += 1;
            me.life += 7;
            me.exp += 60;
            me.calc_bonuses = _hades_calc_bonuses;
            me.get_flags = _hades_get_flags;
            break;
        case DEMIGOD_HEPHAESTUS:
            me.exp += 50;
            me.skills.thn += 10;
            me.skills.sav += 2;
            me.calc_bonuses = _hephaestus_calc_bonuses;
            me.get_flags = _hephaestus_get_flags;
            break;
        case DEMIGOD_HERA:
            me.stats[A_WIS] += 2;
            me.exp += 40;
            me.skills.sav += 5;
            me.skills.dev += 5;
            me.calc_bonuses = _hera_calc_bonuses;
            me.get_powers = _hera_get_powers;
            me.get_flags = _hera_get_flags;
            break;
        case DEMIGOD_HERMES:
            me.skills.dis += 10;
            me.skills.stl += 3;
            me.skills.srh += 5;
            me.skills.fos += 5;
            me.exp += 60;
            me.calc_bonuses = _hermes_calc_bonuses;
            me.get_flags = _hermes_get_flags;
            break;
        case DEMIGOD_POSEIDON:
            me.stats[A_STR] += 1;
            me.stats[A_DEX] += 1;
            me.skills.thn += 7;
            me.exp += 60;
            me.calc_bonuses = _poseidon_calc_bonuses;
            me.get_flags = _poseidon_get_flags;
            break;
        case DEMIGOD_ZEUS:
            me.skills.dis += 2;
            me.skills.dev += 5;
            me.skills.sav += 5;
            me.skills.thn += 10;
            me.skills.thb += 5;
            for (i = 0; i < 6; i++)
                me.stats[i]++;
            me.exp += 70;
            me.calc_bonuses = _zeus_calc_bonuses;
            me.get_flags = _zeus_get_flags;
            break;
        }
        
        subrace_init = psubrace;
    }

    return &me;
}


