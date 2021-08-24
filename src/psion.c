#include "angband.h"

#define _MAX_POWER  5

static int _spell_stat(void)
{
    int result = A_INT;
    int max = plr->stat_ind[A_INT];
    
    if (plr->stat_ind[A_WIS] > max)
    {
        result = A_WIS;
        max = plr->stat_ind[A_WIS];
    }

    if (plr->stat_ind[A_CHR] > max)
    {
        result = A_CHR;
        max = plr->stat_ind[A_CHR];
    }
    return result;
}

static int _spell_stat_idx(void)
{
    return plr->stat_ind[_spell_stat()];
}

/************************************************************************
 * Private Timers
 ************************************************************************/
enum {
    _ARCHERY = T_CUSTOM,
    _BLENDING,
    _CLARITY,
    _COMBAT,
    _DISRUPTION,
    _DRAIN,
    _FORESIGHT,
    _FORTRESS,
    _MINDSPRING,
    _SHIELDING,
    _SPEED,
    _WEAPON_GRAFT,
    _EGO_WHIP,
};

/* _ARCHERY */
static bool _archery_on(plr_tim_ptr timer)
{
    plr_tim_remove(_COMBAT);
    msg_print("You transform into a shooting machine!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _archery_off(plr_tim_ptr timer)
{
    msg_print("Your archery transformation expires.");    
    plr->update |= PU_BONUS;
}
static void _archery_bonus(plr_tim_ptr timer)
{
    /* Note: This also increases shots per round ... cf calc_bonuses in xtra1.c */
    plr->skills.thb += 20 * timer->parm;
}
static status_display_t _archery_display(plr_tim_ptr timer)
{
    return status_display_create("Archery", "Ay", TERM_RED);
}
static plr_tim_info_ptr _archery(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_ARCHERY, "Archery Transformation");
    info->desc = "Your archery skills are greatly enhanced.";
    info->on_f = _archery_on;
    info->off_f = _archery_off;
    info->calc_bonuses_f = _archery_bonus;
    info->status_display_f = _archery_display;
    return info;
}

/* _BLENDING */
static bool _blending_on(plr_tim_ptr timer)
{
    msg_print("You blend into your surroundings.");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _blending_off(plr_tim_ptr timer)
{
    msg_print("You no longer blend into your surroundings.");    
    plr->update |= PU_BONUS;
}
static void _blending_bonus(plr_tim_ptr timer)
{
    plr->skills.stl += 5 * timer->parm;
    if ((plr->cursed & OFC_AGGRAVATE) && timer->parm == 5)
    {
        plr->cursed &= ~(OFC_AGGRAVATE);
        plr->skills.stl = MIN(plr->skills.stl - 3, (plr->skills.stl + 2) / 2);
    }
}
static void _blending_flags(plr_tim_ptr timer, u32b flags[OF_ARRAY_SIZE])
{
    add_flag(flags, OF_STEALTH);
}
static status_display_t _blending_display(plr_tim_ptr timer)
{
    return status_display_create("Blending", "Bl", TERM_L_DARK);
}
static plr_tim_info_ptr _blending(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_BLENDING, "Blending");
    info->desc = "You are blending into your surroundings.";
    info->on_f = _blending_on;
    info->off_f = _blending_off;
    info->calc_bonuses_f = _blending_bonus;
    info->flags_f = _blending_flags;
    info->status_display_f = _blending_display;
    return info;
}

/* _CLARITY */
static bool _clarity_on(plr_tim_ptr timer)
{
    msg_print("You focus your mind.");
    return TRUE;
}
static void _clarity_off(plr_tim_ptr timer)
{
    msg_print("You lose your mental focus.");    
}
static status_display_t _clarity_display(plr_tim_ptr timer)
{
    return status_display_create("Clarity", "Cl", TERM_YELLOW);
}
static plr_tim_info_ptr _clarity(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_CLARITY, "Clarity");
    info->desc = "You have psionic clarity.";
    info->on_f = _clarity_on;
    info->off_f = _clarity_off;
    info->status_display_f = _clarity_display;
    return info;
}

/* _COMBAT */
static bool _combat_on(plr_tim_ptr timer)
{
    plr_tim_remove(_ARCHERY);
    msg_print("You transform into a fighting machine!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _combat_off(plr_tim_ptr timer)
{
    msg_print("Your combat transformation expires.");    
    plr->update |= PU_BONUS;
}
static void _combat_bonus(plr_tim_ptr timer)
{
    plr->skills.thn += 20 * timer->parm;
}
static void _combat_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    info->xtra_blow += timer->parm * 50;
}
static status_display_t _combat_display(plr_tim_ptr timer)
{
    return status_display_create("Combat", "Ct", TERM_RED);
}
static plr_tim_info_ptr _combat(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_COMBAT, "Combat Transformation");
    info->desc = "Your combat skills are greatly enhanced.";
    info->on_f = _combat_on;
    info->off_f = _combat_off;
    info->calc_bonuses_f = _combat_bonus;
    info->calc_weapon_bonuses_f = _combat_weapon_bonus;
    info->status_display_f = _combat_display;
    return info;
}

/* _DISRUPTION */
static bool _disruption_on(plr_tim_ptr timer)
{
    msg_print("You project disrupting thoughts!");
    return TRUE;
}
static void _disruption_off(plr_tim_ptr timer)
{
    msg_print("Your mental disruption vanishes.");
}
static status_display_t _disruption_display(plr_tim_ptr timer)
{
    return status_display_create("Disruption", "[M", TERM_RED);
}
static plr_tim_info_ptr _disruption(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_DISRUPTION, "Disruption");
    info->desc = "You are projecting disrupting thoughts.";
    info->on_f = _disruption_on;
    info->off_f = _disruption_off;
    info->status_display_f = _disruption_display;
    info->dispel_prob = 100;
    return info;
}

/* _DRAIN */
static bool _drain_on(plr_tim_ptr timer)
{
    msg_print("You prepare to draw power from surrounding magics.");
    return TRUE;
}
static void _drain_off(plr_tim_ptr timer)
{
    msg_print("You no longer drain power from surrounding magics.");
}
static status_display_t _drain_display(plr_tim_ptr timer)
{
    return status_display_create("Drain", "Dr", TERM_YELLOW);
}
static plr_tim_info_ptr _drain(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_DRAIN, "Drain");
    info->desc = "You are draining power from surrounding magics.";
    info->on_f = _drain_on;
    info->off_f = _drain_off;
    info->status_display_f = _drain_display;
    info->dispel_prob = 100;
    return info;
}

/* _EGO_WHIP */
static cptr _mon_name(mon_ptr mon)
{
    static char buf[80];
    monster_desc(buf, mon, 0);
    return buf;
}
static void _ego_whip_on(mon_ptr mon, mon_tim_ptr timer)
{
    msg_format("%^s is lashed by an ego whip!", _mon_name(mon));
    plr->redraw |= PR_HEALTH_BARS;
}
static void _ego_whip_off(mon_ptr mon, mon_tim_ptr timer)
{
    plr->redraw |= PR_HEALTH_BARS;
}
static void _ego_whip_tick(mon_ptr mon, mon_tim_ptr timer)
{
    anger_monster(mon);
    if (psion_mon_save_p(mon->race->id, timer->parm))
    {
        msg_format("%^s shakes off your ego whip!", _mon_name(mon));
        timer->count = 0;
        plr->redraw |= PR_HEALTH_BARS;
    }
    else
    {
        bool fear = FALSE;
        if (mon->ml) msg_format("Your ego whip lashes %s!", _mon_name(mon));
        if (mon_take_hit(mon, spell_power(40*timer->parm), &fear, NULL)) return;
        timer->count--;
        if (!plr_project(mon->pos))
            mon_anger(mon);
        if (timer->count <= 0)
        {
            if (mon->ml) msg_format("Your ego whip on %s disappears.", _mon_name(mon));
            plr->redraw |= PR_HEALTH_BARS;
        }
    }
}
static void _ego_whip_display(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_insert_char(doc, TERM_L_BLUE, 'W');
}
static void _ego_whip_probe(mon_ptr mon, mon_tim_ptr timer, doc_ptr doc)
{
    doc_printf(doc, "<color:B>Ego Whip (<color:w>%d</color>)</color>\n", timer->count);
}
static mon_tim_info_ptr _ego_whip(void)
{
    mon_tim_info_ptr info = mon_tim_info_alloc(_EGO_WHIP, "Ego Whip");
    info->on_f = _ego_whip_on;
    info->off_f = _ego_whip_off;
    info->tick_f = _ego_whip_tick;
    info->display_f = _ego_whip_display;
    info->probe_f = _ego_whip_probe;
    info->flags = TF_NO_DISPEL | TF_IGNORE | TF_FAST_TICK;
    return info;
}

/* _FORESIGHT */
static bool _foresight_on(plr_tim_ptr timer)
{
    msg_print("You can see the future!");
    return TRUE;
}
static void _foresight_off(plr_tim_ptr timer)
{
    msg_print("Your foresight fades.");    
}
static status_display_t _foresight_display(plr_tim_ptr timer)
{
    return status_display_create("Foresight", "Fs", TERM_YELLOW);
}
static plr_tim_info_ptr _foresight(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_FORESIGHT, "Foresight");
    info->desc = "You have psionic foresight.";
    info->on_f = _foresight_on;
    info->off_f = _foresight_off;
    info->status_display_f = _foresight_display;
    info->dispel_prob = 100;
    return info;
}

/* _FORTRESS */
static bool _fortress_on(plr_tim_ptr timer)
{
    msg_print("You erect a mental fortress.");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _fortress_off(plr_tim_ptr timer)
{
    msg_print("Your mental fortress collapses.");    
    plr->update |= PU_BONUS;
}
static void _fortress_bonus(plr_tim_ptr timer)
{
    plr->spell_power += timer->parm;
    res_add(GF_TIME);
    plr->sustain_str = TRUE;
    plr->sustain_int = TRUE;
    plr->sustain_wis = TRUE;
    plr->sustain_dex = TRUE;
    plr->sustain_con = TRUE;
    plr->sustain_chr = TRUE;
    plr->hold_life++;
    res_add_immune(GF_STUN);
}
static void _fortress_flags(plr_tim_ptr timer, u32b flags[OF_ARRAY_SIZE])
{
    add_flag(flags, OF_SPELL_POWER);
    add_flag(flags, OF_RES_(GF_TIME));
    add_flag(flags, OF_SUST_STR);
    add_flag(flags, OF_SUST_INT);
    add_flag(flags, OF_SUST_WIS);
    add_flag(flags, OF_SUST_DEX);
    add_flag(flags, OF_SUST_CON);
    add_flag(flags, OF_SUST_CHR);
    add_flag(flags, OF_HOLD_LIFE);
}
static status_display_t _fortress_display(plr_tim_ptr timer)
{
    return status_display_create("Fortress", "Ft", TERM_VIOLET);
}
static plr_tim_info_ptr _fortress(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_FORTRESS, "Fortress");
    info->desc = "You are protected by a mental fortress.";
    info->on_f = _fortress_on;
    info->off_f = _fortress_off;
    info->calc_bonuses_f = _fortress_bonus;
    info->flags_f = _fortress_flags;
    info->status_display_f = _fortress_display;
    info->dispel_prob = 100;
    return info;
}

/* _MINDSPRING */
static bool _mindspring_on(plr_tim_ptr timer)
{
    msg_print("Your mindspring flows.");
    return TRUE;
}
static void _mindspring_off(plr_tim_ptr timer)
{
    msg_print("Your mindspring dries up.");    
}
static status_display_t _mindspring_display(plr_tim_ptr timer)
{
    return status_display_create("Mindspring", "Ms", TERM_GREEN);
}
static plr_tim_info_ptr _mindspring(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_MINDSPRING, "Mindspring");
    info->desc = "You are recovering mana unbelievably quickly.";
    info->on_f = _mindspring_on;
    info->off_f = _mindspring_off;
    info->status_display_f = _mindspring_display;
    return info;
}

/* _SHIELDING */
static bool _shielding_on(plr_tim_ptr timer)
{
    msg_print("You create a psionic shield.");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _shielding_off(plr_tim_ptr timer)
{
    msg_print("Your psionic shield disappears.");    
    plr->update |= PU_BONUS;
}
static void _shielding_bonus(plr_tim_ptr timer)
{
    plr->free_act++;
    plr_bonus_ac(15 * timer->parm);
}
static void _shielding_flags(plr_tim_ptr timer, u32b flags[OF_ARRAY_SIZE])
{
    add_flag(flags, OF_FREE_ACT);
}
static status_display_t _shielding_display(plr_tim_ptr timer)
{
    return status_display_create("Shielding", "Sh", TERM_ORANGE);
}
static plr_tim_info_ptr _shielding(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_SHIELDING, "Shielding");
    info->desc = "You are protected by a psionic shield.";
    info->on_f = _shielding_on;
    info->off_f = _shielding_off;
    info->calc_bonuses_f = _shielding_bonus;
    info->flags_f = _shielding_flags;
    info->status_display_f = _shielding_display;
    info->dispel_prob = 100;
    return info;
}

/* _SPEED */
static bool _speed_on(plr_tim_ptr timer)
{
    msg_print("You gain psionic speed.");
    virtue_add(VIRTUE_PATIENCE, -1);
    virtue_add(VIRTUE_DILIGENCE, 1);
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _speed_off(plr_tim_ptr timer)
{
    if (plr_tim_find(T_LIGHT_SPEED)) return;
    msg_print("Your psionic speed fades.");
    plr->update |= PU_BONUS;
}
static void _speed_bonus(plr_tim_ptr timer)
{
    if (!plr->riding)
        plr_bonus_speed(4 * timer->parm);
}
static void _speed_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SPEED);
}
static bool _speed_dispel(plr_tim_ptr timer, mon_ptr mon)
{
    return plr->pspeed < 35 && timer->parm > 2;
}
static status_display_t _speed_display(plr_tim_ptr timer)
{
    return status_display_create("Speed", "Sp", TERM_YELLOW);
}
static plr_tim_info_ptr _speed(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_SPEED, "Speed");
    info->desc = "You are moving faster.";
    info->on_f = _speed_on;
    info->off_f = _speed_off;
    info->calc_bonuses_f = _speed_bonus;
    info->flags_f = _speed_flags;
    info->dispel_check_f = _speed_dispel;
    info->status_display_f = _speed_display;
    return info;
}

/* _WEAPON_GRAFT */
static bool _weapon_graft_on(plr_tim_ptr timer)
{
    msg_print("Your weapon fuses to your arm!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _weapon_graft_off(plr_tim_ptr timer)
{
    msg_print("Your melee weapon is no longer fused to your arm.");    
    plr->update |= PU_BONUS;
}
static void _weapon_graft_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    if (!obj) return;
    info->to_h += timer->parm * 6;
    info->dis_to_h += timer->parm * 6;
    info->to_d += timer->parm * 4;
    info->dis_to_d += timer->parm * 4;
}
static status_display_t _weapon_graft_display(plr_tim_ptr timer)
{
    return status_display_create("Graft", "Gft", TERM_WHITE);
}
static plr_tim_info_ptr _weapon_graft(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_WEAPON_GRAFT, "Weapon Graft");
    info->desc = "Your weapon is grafted to your arm.";
    info->on_f = _weapon_graft_on;
    info->off_f = _weapon_graft_off;
    info->calc_weapon_bonuses_f = _weapon_graft_weapon_bonus;
    info->status_display_f = _weapon_graft_display;
    return info;
}

static void _register_timers(void)
{
    plr_tim_register(_archery());
    plr_tim_register(_blending());
    plr_tim_register(_clarity());
    plr_tim_register(_combat());
    plr_tim_register(_disruption());
    plr_tim_register(_drain());
    plr_tim_register(_foresight());
    plr_tim_register(_fortress());
    plr_tim_register(_mindspring());
    plr_tim_register(_shielding());
    plr_tim_register(_speed());
    plr_tim_register(_weapon_graft());

    mon_tim_register(_ego_whip());
}

/************************************************************************
 * Public
 ************************************************************************/
bool psion_can_wield(object_type *o_ptr)
{
    if ( obj_is_weapon(o_ptr) 
      && plr->pclass == CLASS_PSION
      && plr_tim_find(_WEAPON_GRAFT) )
    {
        msg_print("Failed! Your weapon is currently grafted to your arm!");
        return FALSE;
    }
    return TRUE;
}

int psion_backlash_dam(int dam)
{
    int power = plr_tim_parm(T_REVENGE);
    if (!power) return dam; /* not a psion, or ?Revenge */
    return dam * (25 + 35*power) / 100;
}

bool psion_mental_fortress(void)
{
    if (plr->pclass != CLASS_PSION) return FALSE;
    return plr_tim_find(_FORTRESS);
}

void _do_mindspring(int energy)
{
    if (!plr_tim_find(_MINDSPRING)) return;
    plr->csp += 20 * plr_tim_parm(_MINDSPRING) * energy / 100;
    if (plr->csp >= plr->msp)
    {
        plr->csp = plr->msp;
        plr->csp_frac = 0;
    }
    plr->redraw |= PR_MANA;
}

bool psion_disruption(void)
{
    if (plr->pclass != CLASS_PSION) return FALSE;
    return plr_tim_find(_DISRUPTION);
}

bool psion_check_disruption(int m_idx)
{
    monster_type *m_ptr = dun_mon(cave, m_idx);
    return psion_check_disruption_aux(m_ptr);
}
bool psion_check_disruption_aux(mon_ptr m_ptr)
{
    if (psion_disruption())
    {
        monster_race *r_ptr = m_ptr->race;
        int           pl = plr->lev + 8*plr_tim_parm(_DISRUPTION);

        if (randint0(r_ptr->alloc.lvl) < pl) 
            return TRUE;
    }
    return FALSE;
}

bool psion_drain(void)
{
    if (plr->pclass != CLASS_PSION) return FALSE;
    return plr_tim_find(_DRAIN);
}

int psion_do_drain(int dam)
{
    int result = dam, drain, power;
    mon_spell_cast_ptr cast = mon_spell_current();

    if (!psion_drain()) return result;
    if (!cast) return result;
    if (cast->spell->flags & MSF_INNATE) return result;

    power = plr_tim_parm(_DRAIN);
    drain = dam * 5 * power / 100;
    result -= drain;
    sp_player(MAX(drain, 3 * power));
    if (disturb_minor)
        msg_print("You draw power from the magics around you!");
    return result;
}

bool psion_foresight(void)
{
    if (plr->pclass != CLASS_PSION) return FALSE;
    return plr_tim_find(_FORESIGHT);
}

bool psion_check_foresight(void)
{
    if (!psion_foresight()) return FALSE;
    if (randint1(100) <= 12*plr_tim_parm(_FORESIGHT) + 7)
    {
        msg_print("You saw that one coming!");
        return TRUE;
    }
    return FALSE;
}

bool psion_mon_save_p(int r_idx, int power)
{
    int pl = plr->lev;
    int ml = mon_race_lookup(r_idx)->alloc.lvl;
    int s = _spell_stat_idx() + 3;

    if (ml + randint1(100) > pl + s + power*14) return TRUE;
    return FALSE;
}

/***************************************************************************
   For spells, I would prefer to choose the spell first and be presented
   with a sublist for all available power options, rather than choosing the
   power level (blindly) up front. This is a bit harder to implement as
   it requires 5 separate spell functions for each psionic power.
   For example, I know I want to blast an enemy with a Mana Thrust, but
   I don't know how much I can afford or what the various damage levels
   are.
 ***************************************************************************/

static cptr _roman_numeral[_MAX_POWER + 1] = { "", "I", "II", "III", "IV", "V" };

/* Archery Transformation */
static void _archery_transformation_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Archery %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "For a short while, you focus your mental powers on effective shooting.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Shots: +%d.%02d", power*25/100, (power*25)%100));
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr_tim_find(_ARCHERY))
        {
            msg_print("You are already transformed into a shooting machine.");
            return;
        }
        plr_tim_add_aux(_ARCHERY, spell_power(power*8 + 20), power);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _archery_transformation1_spell(int cmd, var_ptr res) { _archery_transformation_spell(1, cmd, res); }
static void _archery_transformation2_spell(int cmd, var_ptr res) { _archery_transformation_spell(2, cmd, res); }
static void _archery_transformation3_spell(int cmd, var_ptr res) { _archery_transformation_spell(3, cmd, res); }
static void _archery_transformation4_spell(int cmd, var_ptr res) { _archery_transformation_spell(4, cmd, res); }
static void _archery_transformation5_spell(int cmd, var_ptr res) { _archery_transformation_spell(5, cmd, res); }

/* Brain Smash */
static void _brain_smash_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Brain Smash %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "Pummel the minds of your foes.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_radius(2));
        break;
    default:
        ball_spell(cmd, res, 2, GF_PSI_BRAIN_SMASH, power);
    }
}
static void _brain_smash1_spell(int cmd, var_ptr res) { _brain_smash_spell(1, cmd, res); }
static void _brain_smash2_spell(int cmd, var_ptr res) { _brain_smash_spell(2, cmd, res); }
static void _brain_smash3_spell(int cmd, var_ptr res) { _brain_smash_spell(3, cmd, res); }
static void _brain_smash4_spell(int cmd, var_ptr res) { _brain_smash_spell(4, cmd, res); }
static void _brain_smash5_spell(int cmd, var_ptr res) { _brain_smash_spell(5, cmd, res); }

/* Combat Transformation */
static void _combat_transformation_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Combat %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "For a short while, you focus your mental powers on effective combat.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Blows: +%d.%d", power/2, (power % 2)*5));
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr_tim_find(_COMBAT))
        {
            msg_print("You are already transformed into a fighting machine.");
            return;
        }
        plr_tim_add_aux(_COMBAT, spell_power(power*8 + 20), power);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _combat_transformation1_spell(int cmd, var_ptr res) { _combat_transformation_spell(1, cmd, res); }
static void _combat_transformation2_spell(int cmd, var_ptr res) { _combat_transformation_spell(2, cmd, res); }
static void _combat_transformation3_spell(int cmd, var_ptr res) { _combat_transformation_spell(3, cmd, res); }
static void _combat_transformation4_spell(int cmd, var_ptr res) { _combat_transformation_spell(4, cmd, res); }
static void _combat_transformation5_spell(int cmd, var_ptr res) { _combat_transformation_spell(5, cmd, res); }

/* Ego Whip */
static void _ego_whip_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Ego Whip %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "Lash out against a single monster with psychic energy.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, spell_power(power*40)));
        break;
    case SPELL_CAST:
    {
        mon_ptr mon = plr_target_mon();
        var_set_bool(res, FALSE);
        if (!mon) return;

        mon_tim_remove(mon, MT_SLEEP);
        mon_tim_add_aux(mon, _EGO_WHIP, 5, power);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _ego_whip1_spell(int cmd, var_ptr res) { _ego_whip_spell(1, cmd, res); }
static void _ego_whip2_spell(int cmd, var_ptr res) { _ego_whip_spell(2, cmd, res); }
static void _ego_whip3_spell(int cmd, var_ptr res) { _ego_whip_spell(3, cmd, res); }
static void _ego_whip4_spell(int cmd, var_ptr res) { _ego_whip_spell(4, cmd, res); }
static void _ego_whip5_spell(int cmd, var_ptr res) { _ego_whip_spell(5, cmd, res); }

/* Energy Blast */
typedef struct {
    cptr name;
    int type;
} _blast_t;
static _blast_t _blasts[_MAX_POWER] = {
    {"Fire", GF_FIRE},
    {"Cold", GF_COLD},
    {"Poison", GF_POIS},
    {"Acid", GF_ACID},
    {"Lightning", GF_ELEC},
};
static void _energy_blast_menu_fn(int cmd, int which, vptr cookie, var_ptr res)
{
    switch (cmd)
    {
    case MENU_KEY:
        var_set_int(res, _blasts[which].name[0]);
        break;
    case MENU_TEXT:
        var_set_string(res, _blasts[which].name);
        break;
    default:
        default_menu(cmd, which, cookie, res);
    }
}

static int _get_energy_blast_type(int power)
{
    if (power == 1) return GF_FIRE;
    else
    { 
        int i;
        menu_t menu = { "Choose which effect?", NULL, NULL,
                        _energy_blast_menu_fn, NULL, power};
        
        i = menu_choose(&menu);
        if (i >= 0)
            i = _blasts[i].type;
        return i;
    }
}

static void _energy_blast_spell(int power, int cmd, var_ptr res)
{
    dice_t dice = spell_dam_dice(4*power, 4*power, 0);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Blast %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires an elemental ball.");
        break;
    case SPELL_INFO:
        var_printf(res, "dam ~%d", dice_avg_roll(dice));
        break;
    case SPELL_CAST: {
        int gf = _get_energy_blast_type(power); /* menu */
        int rad = (power + 1)/2;
        
        var_set_bool(res, FALSE);
        if (gf < 0) return; /* cancelled menu */
        var_set_bool(res, plr_cast_ball(rad, gf, dice));
        /* XXX PROJECT_FULL_DAM ... I want to kill this. */
        break; }
    default:
        default_spell(cmd, res);
    }
}

static void _energy_blast1_spell(int cmd, var_ptr res) { _energy_blast_spell(1, cmd, res); }
static void _energy_blast2_spell(int cmd, var_ptr res) { _energy_blast_spell(2, cmd, res); }
static void _energy_blast3_spell(int cmd, var_ptr res) { _energy_blast_spell(3, cmd, res); }
static void _energy_blast4_spell(int cmd, var_ptr res) { _energy_blast_spell(4, cmd, res); }
static void _energy_blast5_spell(int cmd, var_ptr res) { _energy_blast_spell(5, cmd, res); }

/* Graft Weapon */
static void _graft_weapon_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Graft Weapon %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "Fuses your melee weapon to your arms and gain combat bonuses. For the duration of this power, you cannot unequip/swap your weapon.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("(+%2d,+%2d) melee", 6*power, 4*power));
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr_tim_find(_WEAPON_GRAFT))
        {
            msg_print("Your weapon is already grafted!");
            return;
        }
        plr_tim_add_aux(_WEAPON_GRAFT, spell_power(8*power + 20), power);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _graft_weapon1_spell(int cmd, var_ptr res) { _graft_weapon_spell(1, cmd, res); }
static void _graft_weapon2_spell(int cmd, var_ptr res) { _graft_weapon_spell(2, cmd, res); }
static void _graft_weapon3_spell(int cmd, var_ptr res) { _graft_weapon_spell(3, cmd, res); }
static void _graft_weapon4_spell(int cmd, var_ptr res) { _graft_weapon_spell(4, cmd, res); }
static void _graft_weapon5_spell(int cmd, var_ptr res) { _graft_weapon_spell(5, cmd, res); }

/* Mana Thrust */
static void _mana_thrust_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Mana Thrust %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a bolt of pure mana.");
        break;
    default:
        bolt_spell(cmd, res, GF_MANA, 4*power, 4*power);
    }
}

static void _mana_thrust1_spell(int cmd, var_ptr res) { _mana_thrust_spell(1, cmd, res); }
static void _mana_thrust2_spell(int cmd, var_ptr res) { _mana_thrust_spell(2, cmd, res); }
static void _mana_thrust3_spell(int cmd, var_ptr res) { _mana_thrust_spell(3, cmd, res); }
static void _mana_thrust4_spell(int cmd, var_ptr res) { _mana_thrust_spell(4, cmd, res); }
static void _mana_thrust5_spell(int cmd, var_ptr res) { _mana_thrust_spell(5, cmd, res); }

/* Mental Fortress */
static void _mental_fortress_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Mental Fortress %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "For a short time, you become resistant to anti-magic, dispel magic and any attack that drains mana.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Spell Power: +%d%%", spell_power_aux(100, power) - 100));
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr_tim_find(_FORTRESS))
        {
            msg_print("You already have a mental fortress.");
            return;
        }
        plr_tim_add_aux(_FORTRESS, spell_power(power + 3), power);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _mental_fortress1_spell(int cmd, var_ptr res) { _mental_fortress_spell(1, cmd, res); }
static void _mental_fortress2_spell(int cmd, var_ptr res) { _mental_fortress_spell(2, cmd, res); }
static void _mental_fortress3_spell(int cmd, var_ptr res) { _mental_fortress_spell(3, cmd, res); }
static void _mental_fortress4_spell(int cmd, var_ptr res) { _mental_fortress_spell(4, cmd, res); }
static void _mental_fortress5_spell(int cmd, var_ptr res) { _mental_fortress_spell(5, cmd, res); }

/* Mindspring */
static void _mindspring_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Mindspring %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "For a short time, you regain mana with every action.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Recover %d sp/rnd", 20*power));
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr_tim_find(_MINDSPRING))
        {
            msg_print("Your mindspring is already flowing.");
            return;
        }
        plr_tim_add_aux(_MINDSPRING, spell_power(power*2 + 3), power);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _mindspring1_spell(int cmd, var_ptr res) { _mindspring_spell(1, cmd, res); }
static void _mindspring2_spell(int cmd, var_ptr res) { _mindspring_spell(2, cmd, res); }
static void _mindspring3_spell(int cmd, var_ptr res) { _mindspring_spell(3, cmd, res); }
static void _mindspring4_spell(int cmd, var_ptr res) { _mindspring_spell(4, cmd, res); }
static void _mindspring5_spell(int cmd, var_ptr res) { _mindspring_spell(5, cmd, res); }

/* Psionic Backlash */
static void _psionic_backlash_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Backlash %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "For a short while, monsters are damaged whenever they hurt you.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Revenge: %d%%", 25 + 35*power));
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr_tim_parm(T_REVENGE)) /* XXX replace ?Vengeance */
        {
            msg_print("Your psionic revenge is already active.");
            return;
        }
        plr_tim_add_aux(T_REVENGE, spell_power(power*5 + 5), power);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _psionic_backlash1_spell(int cmd, var_ptr res) { _psionic_backlash_spell(1, cmd, res); }
static void _psionic_backlash2_spell(int cmd, var_ptr res) { _psionic_backlash_spell(2, cmd, res); }
static void _psionic_backlash3_spell(int cmd, var_ptr res) { _psionic_backlash_spell(3, cmd, res); }
static void _psionic_backlash4_spell(int cmd, var_ptr res) { _psionic_backlash_spell(4, cmd, res); }
static void _psionic_backlash5_spell(int cmd, var_ptr res) { _psionic_backlash_spell(5, cmd, res); }

/* Psionic Blending */
static void _psionic_blending_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Blending %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "You will temporarily blend into your surroundings, gaining increased stealth.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("+%d stealth", 5*power));
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr_tim_find(_BLENDING))
        {
            msg_print("You are already blending into your surroundings.");
            return;
        }
        plr_tim_add_aux(_BLENDING, spell_power(power*25 + 50), power);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _psionic_blending1_spell(int cmd, var_ptr res) { _psionic_blending_spell(1, cmd, res); }
static void _psionic_blending2_spell(int cmd, var_ptr res) { _psionic_blending_spell(2, cmd, res); }
static void _psionic_blending3_spell(int cmd, var_ptr res) { _psionic_blending_spell(3, cmd, res); }
static void _psionic_blending4_spell(int cmd, var_ptr res) { _psionic_blending_spell(4, cmd, res); }
static void _psionic_blending5_spell(int cmd, var_ptr res) { _psionic_blending_spell(5, cmd, res); }

/* Psionic Clarity */
static void _psionic_clarity_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Clarity %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "For the duration of this power, you gain increased mental focus. Your psionic powers become cheaper to cast.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Costs: %d%%", 85-7*power));
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr_tim_find(_CLARITY))
        {
            msg_print("Your mind is already focused.");
            return;
        }
        plr_tim_add_aux(_CLARITY, spell_power(2*power + 5), power);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _psionic_clarity1_spell(int cmd, var_ptr res) { _psionic_clarity_spell(1, cmd, res); }
static void _psionic_clarity2_spell(int cmd, var_ptr res) { _psionic_clarity_spell(2, cmd, res); }
static void _psionic_clarity3_spell(int cmd, var_ptr res) { _psionic_clarity_spell(3, cmd, res); }
static void _psionic_clarity4_spell(int cmd, var_ptr res) { _psionic_clarity_spell(4, cmd, res); }
static void _psionic_clarity5_spell(int cmd, var_ptr res) { _psionic_clarity_spell(5, cmd, res); }

/* Psionic Crafting */
static int _enchant_power = 0;
int psion_enchant_power(void) { 
    if (plr->pclass == CLASS_PSION)
        return _enchant_power;
    return 0;
}
void _psionic_crafting_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Crafting %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to enchant a weapon, ammo or armor.");
        break;
    case SPELL_CAST:
    {
        obj_prompt_t prompt = {0};
        bool         okay = FALSE;
        char         o_name[MAX_NLEN];

        var_set_bool(res, FALSE);

        prompt.prompt = "Enchant which item?";
        prompt.error = "You have nothing to enchant.";
        prompt.filter = object_is_weapon_armour_ammo;
        prompt.where[0] = INV_PACK;
        prompt.where[1] = INV_EQUIP;
        prompt.where[2] = INV_QUIVER;
        prompt.where[3] = INV_FLOOR;

        obj_prompt(&prompt);
        if (!prompt.obj) return;

        object_desc(o_name, prompt.obj, (OD_OMIT_PREFIX | OD_NAME_ONLY));

        _enchant_power = power; /* Hack for enchant(), which I'm too lazy to rewrite ... */
        if (power == 5 && object_is_nameless(prompt.obj) && prompt.obj->number == 1)
        {
            if (obj_is_weapon(prompt.obj))
            {
                if (brand_weapon_aux(prompt.obj))
                {
                    prompt.obj->discount = 99;
                    okay = TRUE;
                }
            }
            else if (obj_is_armor(prompt.obj))
            {
                if (brand_armour_aux(prompt.obj))
                {
                    prompt.obj->discount = 99;
                    okay = TRUE;
                }
            }
        }

        if (!okay)
        {
            if (obj_is_weapon_ammo(prompt.obj))
            {
                if (enchant(prompt.obj, randint0(4) + 1, ENCH_TOHIT | ENCH_PSI_HACK)) okay = TRUE;
                if (enchant(prompt.obj, randint0(4) + 1, ENCH_TODAM | ENCH_PSI_HACK)) okay = TRUE;
            }
            else
            {
                if (enchant(prompt.obj, randint0(3) + 2, ENCH_TOAC | ENCH_PSI_HACK)) okay = TRUE;            
            }
        }

        msg_format("The %s glow%s brightly!", o_name,
                ((prompt.obj->number > 1) ? "" : "s"));

        if (!okay)
        {
            if (flush_failure) flush();
            msg_print("The enchantment failed.");
            if (one_in_(3)) virtue_add(VIRTUE_ENCHANTMENT, -1);
        }
        else
        {
            virtue_add(VIRTUE_ENCHANTMENT, 1);
            android_calc_exp();
        }
        _enchant_power = 0;
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
void _psionic_crafting1_spell(int cmd, var_ptr res) { _psionic_crafting_spell(1, cmd, res); }
void _psionic_crafting2_spell(int cmd, var_ptr res) { _psionic_crafting_spell(2, cmd, res); }
void _psionic_crafting3_spell(int cmd, var_ptr res) { _psionic_crafting_spell(3, cmd, res); }
void _psionic_crafting4_spell(int cmd, var_ptr res) { _psionic_crafting_spell(4, cmd, res); }
void _psionic_crafting5_spell(int cmd, var_ptr res) { _psionic_crafting_spell(5, cmd, res); }

/* Psionic Disruption */
static void _psionic_disruption_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Disruption %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "For a short while, your mental focus will disrupt the minds of others.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Power: %d", plr->lev + 8*power));
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr_tim_find(_DISRUPTION))
        {
            msg_print("Your disruption is already active.");
            return;
        }
        plr_tim_add_aux(_DISRUPTION, spell_power(power*2 + 3), power);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _psionic_disruption1_spell(int cmd, var_ptr res) { _psionic_disruption_spell(1, cmd, res); }
static void _psionic_disruption2_spell(int cmd, var_ptr res) { _psionic_disruption_spell(2, cmd, res); }
static void _psionic_disruption3_spell(int cmd, var_ptr res) { _psionic_disruption_spell(3, cmd, res); }
static void _psionic_disruption4_spell(int cmd, var_ptr res) { _psionic_disruption_spell(4, cmd, res); }
static void _psionic_disruption5_spell(int cmd, var_ptr res) { _psionic_disruption_spell(5, cmd, res); }

/* Psionic Drain */
static void _psionic_drain_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Drain %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "For a short while you will draw mental energy from enemy magic spells, reducing their damage in the process.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Drain: %d%%", 5*power));
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr_tim_find(_DRAIN))
        {
            msg_print("Your drain is already active.");
            return;
        }
        plr_tim_add_aux(_DRAIN, spell_power(power*5 + 10), power);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _psionic_drain1_spell(int cmd, var_ptr res) { _psionic_drain_spell(1, cmd, res); }
static void _psionic_drain2_spell(int cmd, var_ptr res) { _psionic_drain_spell(2, cmd, res); }
static void _psionic_drain3_spell(int cmd, var_ptr res) { _psionic_drain_spell(3, cmd, res); }
static void _psionic_drain4_spell(int cmd, var_ptr res) { _psionic_drain_spell(4, cmd, res); }
static void _psionic_drain5_spell(int cmd, var_ptr res) { _psionic_drain_spell(5, cmd, res); }

/* Psionic Foresight */
static void _psionic_foresight_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Foresight %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "For a short while, you can see the future and may be able to avoid damage.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Avoidance: %d%%", 7 + 12*power));
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr_tim_find(_FORESIGHT))
        {
            msg_print("Your foresight is already active.");
            return;
        }
        plr_tim_add_aux(_FORESIGHT, spell_power(power*2 + 3), power);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _psionic_foresight1_spell(int cmd, var_ptr res) { _psionic_foresight_spell(1, cmd, res); }
static void _psionic_foresight2_spell(int cmd, var_ptr res) { _psionic_foresight_spell(2, cmd, res); }
static void _psionic_foresight3_spell(int cmd, var_ptr res) { _psionic_foresight_spell(3, cmd, res); }
static void _psionic_foresight4_spell(int cmd, var_ptr res) { _psionic_foresight_spell(4, cmd, res); }
static void _psionic_foresight5_spell(int cmd, var_ptr res) { _psionic_foresight_spell(5, cmd, res); }

/* Psionic Healing */
static void _psionic_healing_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Healing %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "Use your mental powers to repair your body.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_heal(0, 0, spell_power(120*power - 50)));
        break;
    case SPELL_CAST:
        hp_player(spell_power(120*power - 50));
        
        plr_tim_remove(T_BLIND);
        plr_tim_remove(T_CONFUSED);
        plr_tim_remove(T_STUN);
        plr_tim_remove(T_CUT);
        plr_tim_remove(T_BERSERK);

        if (power >= 3)
            plr_tim_remove(T_HALLUCINATE);

        if (power == 5)
        {
            do_res_stat(A_STR);
            do_res_stat(A_INT);
            do_res_stat(A_WIS);
            do_res_stat(A_DEX);
            do_res_stat(A_CON);
            do_res_stat(A_CHR);
            restore_level();
        }
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _psionic_healing1_spell(int cmd, var_ptr res) { _psionic_healing_spell(1, cmd, res); }
static void _psionic_healing2_spell(int cmd, var_ptr res) { _psionic_healing_spell(2, cmd, res); }
static void _psionic_healing3_spell(int cmd, var_ptr res) { _psionic_healing_spell(3, cmd, res); }
static void _psionic_healing4_spell(int cmd, var_ptr res) { _psionic_healing_spell(4, cmd, res); }
static void _psionic_healing5_spell(int cmd, var_ptr res) { _psionic_healing_spell(5, cmd, res); }

/* Psionic Protection */
static void _psionic_protection_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
    {
        const cptr _names[_MAX_POWER] = {
            "Resist Fire and Cold", "Resist Environment", "Resistance", "Elemental Protection", "Immunity"};
        var_set_string(res, _names[power-1]);
        break;
    }
    case SPELL_DESC:
    {
        const cptr _descriptions[_MAX_POWER] = {
            "Gain temporary resistance to fire and cold.",
            "Gain temporary resistance to fire, cold and lightning.",
            "Gain temporary resistance to fire, cold, lightning, acid and poison.",
            "Gain temporary resistance to fire, cold, lightning, acid and poison. Gain temporary elemental auras.",
            "Gain temporary immunity to the element of your choice."
            };
        var_set_string(res, _descriptions[power-1]);
        break;
    }
    case SPELL_CAST:
    {
        int dur = spell_power(10*power + 25);
        var_set_bool(res, FALSE);
        if (power >= 5)
        {
            if (!choose_ele_immune(dur)) return;
        }
        else
        {
            plr_tim_add(T_RES_FIRE, dur);
            plr_tim_add(T_RES_COLD, dur);
            if (power >= 2) 
                plr_tim_add(T_RES_ELEC, dur);
            if (power >= 3) 
            {
                plr_tim_add(T_RES_ACID, dur);
                plr_tim_add(T_RES_POIS, dur);
            }
            if (power >= 4)
            {
                plr_tim_add(T_AURA_FIRE, dur);
                if (plr->lev >= 25)
                    plr_tim_add(T_AURA_COLD, dur);
                if (plr->lev >= 35)
                    plr_tim_add(T_AURA_ELEC, dur);
            }
        }
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _psionic_protection1_spell(int cmd, var_ptr res) { _psionic_protection_spell(1, cmd, res); }
static void _psionic_protection2_spell(int cmd, var_ptr res) { _psionic_protection_spell(2, cmd, res); }
static void _psionic_protection3_spell(int cmd, var_ptr res) { _psionic_protection_spell(3, cmd, res); }
static void _psionic_protection4_spell(int cmd, var_ptr res) { _psionic_protection_spell(4, cmd, res); }
static void _psionic_protection5_spell(int cmd, var_ptr res) { _psionic_protection_spell(5, cmd, res); }

/* Psionic Seeing */
static void _psionic_seeing_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
    {
        const cptr _names[_MAX_POWER] = {
            "Detect Monsters", "and Traps, Objects", "and Surroundings", "and Telepathy", "and Clairvoyance"};
        var_set_string(res, _names[power-1]);
        break;
    }
    case SPELL_DESC:
    {
        const cptr _descriptions[_MAX_POWER] = {
            "Detects monsters.", 
            "Detects monsters, doors, stairs, traps and objects.",
            "Detects monsters, doors, stairs, traps and objects. Maps nearby area.",
            "Detects monsters, doors, stairs, traps and objects. Maps nearby area and grants temporary telepathy.",
            "Detects monsters, doors, stairs, traps and objects. Maps entire level and grants temporary telepathy.",
            };
        var_set_string(res, _descriptions[power-1]);
        break;
    }
    case SPELL_CAST:
        detect_monsters_normal(DETECT_RAD_DEFAULT);
        if (power >= 4)
        {
            plr_tim_add(T_TELEPATHY, spell_power(randint1(30) + 25));
            plr->wizard_sight = TRUE;
        }

        if (power >= 5)
        {
            virtue_add(VIRTUE_KNOWLEDGE, 1);
            virtue_add(VIRTUE_ENLIGHTENMENT, 1);
            wiz_lite();
        }
        else if (power >= 3)
            map_area(DETECT_RAD_MAP);

        if (power >= 2)
        {
            detect_traps(DETECT_RAD_DEFAULT, TRUE);
            detect_doors(DETECT_RAD_DEFAULT);
            detect_stairs(DETECT_RAD_DEFAULT);
            detect_recall(DETECT_RAD_DEFAULT);
            detect_objects_normal(DETECT_RAD_DEFAULT);
        }

        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _psionic_seeing1_spell(int cmd, var_ptr res) { _psionic_seeing_spell(1, cmd, res); }
static void _psionic_seeing2_spell(int cmd, var_ptr res) { _psionic_seeing_spell(2, cmd, res); }
static void _psionic_seeing3_spell(int cmd, var_ptr res) { _psionic_seeing_spell(3, cmd, res); }
static void _psionic_seeing4_spell(int cmd, var_ptr res) { _psionic_seeing_spell(4, cmd, res); }
static void _psionic_seeing5_spell(int cmd, var_ptr res) { _psionic_seeing_spell(5, cmd, res); }

/* Psionic Shielding */
static void _psionic_shielding_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Shielding %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "You gain physical protection from your mental fortitude.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("AC: +%d", 15*power));
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr_tim_find(_SHIELDING))
        {
            msg_print("You already have a psionic shield.");
            return;
        }
        plr_tim_add_aux(_SHIELDING, spell_power(power*8 + 20), power);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _psionic_shielding1_spell(int cmd, var_ptr res) { _psionic_shielding_spell(1, cmd, res); }
static void _psionic_shielding2_spell(int cmd, var_ptr res) { _psionic_shielding_spell(2, cmd, res); }
static void _psionic_shielding3_spell(int cmd, var_ptr res) { _psionic_shielding_spell(3, cmd, res); }
static void _psionic_shielding4_spell(int cmd, var_ptr res) { _psionic_shielding_spell(4, cmd, res); }
static void _psionic_shielding5_spell(int cmd, var_ptr res) { _psionic_shielding_spell(5, cmd, res); }

/* Psionic Speed */
static void _psionic_speed_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Speed %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "You focus your mental energy on quickness of motion.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Speed: +%d", 4*power));
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr_tim_find(_SPEED))
        {
            msg_print("You are already fast.");
            return;
        }
        plr_tim_add_aux(_SPEED, spell_power(power*10 + 20), power);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _psionic_speed1_spell(int cmd, var_ptr res) { _psionic_speed_spell(1, cmd, res); }
static void _psionic_speed2_spell(int cmd, var_ptr res) { _psionic_speed_spell(2, cmd, res); }
static void _psionic_speed3_spell(int cmd, var_ptr res) { _psionic_speed_spell(3, cmd, res); }
static void _psionic_speed4_spell(int cmd, var_ptr res) { _psionic_speed_spell(4, cmd, res); }
static void _psionic_speed5_spell(int cmd, var_ptr res) { _psionic_speed_spell(5, cmd, res); }

/* Psionic Storm */
static void _psionic_storm_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Storm %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a ball of psionic energy.");
        break;
    default:
        ball_spell(cmd, res, 2 + power/5, GF_PSI_STORM, 125*power - 25);
    }
}
static void _psionic_storm1_spell(int cmd, var_ptr res) { _psionic_storm_spell(1, cmd, res); }
static void _psionic_storm2_spell(int cmd, var_ptr res) { _psionic_storm_spell(2, cmd, res); }
static void _psionic_storm3_spell(int cmd, var_ptr res) { _psionic_storm_spell(3, cmd, res); }
static void _psionic_storm4_spell(int cmd, var_ptr res) { _psionic_storm_spell(4, cmd, res); }
static void _psionic_storm5_spell(int cmd, var_ptr res) { _psionic_storm_spell(5, cmd, res); }

/* Psionic Travel */
static void _psionic_travel_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
    {
        const cptr _names[_MAX_POWER] = {
            "Phase Door", "Portal", "Teleport", "Dimension Door", "Psionic Portal"};
        var_set_string(res, _names[power-1]);
        break;
    }
    case SPELL_DESC:
    {
        const cptr _descriptions[_MAX_POWER] = {
            "Short Range Teleport", "Medium Range Teleport", "Long Range Teleport", 
            "Teleport to specified location", "Teleport to specified location"};
        var_set_string(res, _descriptions[power-1]);
        break;
    }
    case SPELL_INFO:
        if (power == 1)
            var_set_string(res, info_range(10));
        else if (power == 2)
            var_set_string(res, info_range(25 + plr->lev / 2));
        else if (power == 3)
            var_set_string(res, info_range(plr->lev * 4));
        else
            var_set_string(res, info_range(15*(power - 3)));
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);

        if (power == 1)
            teleport_player(10, 0L);
        else if (power == 2)
            teleport_player(25 + plr->lev/2, 0L);
        else if (power == 3)
            teleport_player(plr->lev * 4, 0L);
        else
            dimension_door(15*(power-3));

        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        if (mut_present(MUT_ASTRAL_GUIDE))
            var_set_int(res, 30);
        else
            var_set_int(res, 100);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _psionic_travel1_spell(int cmd, var_ptr res) { _psionic_travel_spell(1, cmd, res); }
static void _psionic_travel2_spell(int cmd, var_ptr res) { _psionic_travel_spell(2, cmd, res); }
static void _psionic_travel3_spell(int cmd, var_ptr res) { _psionic_travel_spell(3, cmd, res); }
static void _psionic_travel4_spell(int cmd, var_ptr res) { _psionic_travel_spell(4, cmd, res); }
static void _psionic_travel5_spell(int cmd, var_ptr res) { _psionic_travel_spell(5, cmd, res); }

/* Psionic Wave */
static void _psionic_wave_spell(int power, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, format("Mind Wave %s", _roman_numeral[power]));
        break;
    case SPELL_DESC:
        var_set_string(res, "Inflict mental damage on all visible monsters.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, spell_power(power*50)));
        break;
    case SPELL_CAST:
        plr_project_los(GF_PSI_STORM, spell_power(power*50));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _psionic_wave1_spell(int cmd, var_ptr res) { _psionic_wave_spell(1, cmd, res); }
static void _psionic_wave2_spell(int cmd, var_ptr res) { _psionic_wave_spell(2, cmd, res); }
static void _psionic_wave3_spell(int cmd, var_ptr res) { _psionic_wave_spell(3, cmd, res); }
static void _psionic_wave4_spell(int cmd, var_ptr res) { _psionic_wave_spell(4, cmd, res); }
static void _psionic_wave5_spell(int cmd, var_ptr res) { _psionic_wave_spell(5, cmd, res); }

/****************************************************************
 * Spell Table and Exports
 ****************************************************************/

typedef struct {
    int cost;
    int fail;
    ang_spell fn;
} _spell_info_t;

typedef struct {
    cptr name;
    int id;
    int level;
    _spell_info_t info[_MAX_POWER];
    cptr desc;
} _spell_t, *_spell_ptr;

/* Here are the unique spell ids, which can never change */
enum {
    _PSION_MANA_THRUST = 0,
    _PSION_ENERGY_BLAST,
    _PSION_SEEING,
    _PSION_GRAFT_WEAPON,
    _PSION_CLARITY,
    _PSION_BLENDING,
    _PSION_SHIELDING,
    _PSION_TRAVEL,
    _PSION_PROTECTION,
    _PSION_COMBAT_TRANSFORMATION,
    _PSION_ARCHERY_TRANSFORMATION,
    _PSION_EGO_WHIP,
    _PSION_SPEED,
    _PSION_HEALING,
    _PSION_BRAIN_SMASH,
    _PSION_CRAFTING,
    _PSION_STORM,
    _PSION_BACKLASH,
    _PSION_FORTRESS,
    _PSION_MINDSPRING,
    _PSION_FORESIGHT,
    _PSION_DISRUPTION,
    _PSION_DRAIN,
    _PSION_WAVE,
};

/* Here are the spells: Use _get_spell(id) to find the correct spell. */
static _spell_t __spells[] = 
{
    { "Mana Thrust", _PSION_MANA_THRUST, 1, {  
        {  1,  20, _mana_thrust1_spell },
        {  5,  70, _mana_thrust2_spell },
        { 13, 120, _mana_thrust3_spell },
        { 25, 155, _mana_thrust4_spell },
        { 40, 180, _mana_thrust5_spell }},
        "Mana Thrust grants you an offensive ranged attack. "
          "With this power, you will have good early offense as well as the "
          "ability to scale the damage of the mana thrust quite considerably. "
          "No monster can resist the Mana Thrust but this attack only effects "
          "a single monster at a time and some monsters might reflect the spell."
    },
    { "Energy Blast", _PSION_ENERGY_BLAST, 1, {  
        {  1,  20, _energy_blast1_spell },
        {  5,  70, _energy_blast2_spell },
        { 13, 120, _energy_blast3_spell },
        { 25, 155, _energy_blast4_spell },
        { 40, 180, _energy_blast5_spell }},
        "Energy Blast grants you an offensive ranged attack. "
          "With this power, you will have good early offense as well as the "
          "ability to scale the damage of the blast quite considerably. "
          "The blast will produce an elemental ball whose type is of your "
          "choosing, although the range of choices will depend on how "
          "much focus you invest in the blast. This is also an area based "
          "attack so you may damage multiple monsters at a time."
    },
    { "Psionic Seeing", _PSION_SEEING, 1, {  
        {  1,  20, _psionic_seeing1_spell },
        {  7,  50, _psionic_seeing2_spell },
        { 15, 100, _psionic_seeing3_spell },
        { 25, 130, _psionic_seeing4_spell },
        { 50, 180, _psionic_seeing5_spell }},
        "Psionic Seeing grants you considerable powers of detection. "
          "Depending on how hard you focus, you may detect monsters, traps, "
          "doors, stairs, and objects. Concentrate even more and you can "
          "map your surroundings, gain temporary powers of telepathy or even "
          "map the entire level!"
    },
    { "Graft Weapon", _PSION_GRAFT_WEAPON, 1, {  
        {  5,  20, _graft_weapon1_spell },
        { 15,  70, _graft_weapon2_spell },
        { 30, 120, _graft_weapon3_spell },
        { 50, 155, _graft_weapon4_spell },
        { 75, 180, _graft_weapon5_spell }},
        "Weapon Grafting is a power unique to the psion. Invoking this power "
        "fuses your current weapon to your arm for a limited time. In a sense, "
        "your weapon becomes an extension of you and may be wielded much more "
        "effectively. However, while this spell is active, you will not be "
        "able to remove your weapon."
    },
    { "Psionic Clarity", _PSION_CLARITY, 1, {  
        {  6,  50, _psionic_clarity1_spell },
        { 18,  70, _psionic_clarity2_spell },
        { 36, 120, _psionic_clarity3_spell },
        { 60, 160, _psionic_clarity4_spell },
        { 90, 200, _psionic_clarity5_spell }},
        "Psionic Clarity focuses the mind of the psion. While only active for "
        "a short while, this power lowers the casting costs of all other psionic "
        "powers and can be quite useful. However, the utility of Psionic Clarity "
        "may only become manifest late in the game, so it is not recommended as "
        "an early choice."
    },
    { "Psionic Blending", _PSION_BLENDING, 10, {  
        {  4,  40, _psionic_blending1_spell },
        { 12,  55, _psionic_blending2_spell },
        { 24,  70, _psionic_blending3_spell },
        { 40,  85, _psionic_blending4_spell },
        { 60, 100, _psionic_blending5_spell }},
        "Psionic Blending grants great powers of stealth. While active, you "
        "will be able to blend in with your surroundings and sneak up on many foes. "
        "With maximal focus, you may even suppress aggravation (from your equipment), "
        "though your stealth will still be somewhat disrupted."
    },
    { "Psionic Shielding", _PSION_SHIELDING, 10, {  
        {  7,  40, _psionic_shielding1_spell },
        { 21,  60, _psionic_shielding2_spell },
        { 42,  80, _psionic_shielding3_spell },
        { 70, 100, _psionic_shielding4_spell },
        {105, 125, _psionic_shielding5_spell }},
        "Psionic Shielding is a defensive power. While active you will gain Free Action "
          "as well as increased Armor Class. With the former, you will be immune to deadly "
          "paralyzation attacks from your enemies. With the latter, monsters will have "
          "a much harder time hitting you."
    },
    { "Psionic Travel", _PSION_TRAVEL, 10, {  
        {  2,  25, _psionic_travel1_spell },
        {  7,  35, _psionic_travel2_spell },
        {  9,  50, _psionic_travel3_spell },
        { 30, 120, _psionic_travel4_spell },
        { 40, 170, _psionic_travel5_spell }},
        "Psionic Travel grants a wide array of teleportation powers. From inexpensive short "
          "ranged 'blinking' to long ranged escapes, you will have it all. Indeed, with great "
          "focus you will even be able to control your teleportation and choose where you land!"
    },
    { "Psionic Protection", _PSION_PROTECTION, 20, {  
        {  5,  25, _psionic_protection1_spell },
        { 10,  35, _psionic_protection2_spell },
        { 20,  55, _psionic_protection3_spell },
        { 30,  75, _psionic_protection4_spell },
        { 70, 125, _psionic_protection5_spell }},
        "Psionic Protection is a defensive power. While active you will gain resistance to "
          "elemental attacks (Fire, Cold, Lightning, Acid and Poison). The more you mental "
          "focus you devote to this power, the more of the elements you will resist."
    },
    { "Combat Transformation", _PSION_COMBAT_TRANSFORMATION, 20, {  
        { 13,  50, _combat_transformation1_spell },
        { 49,  65, _combat_transformation2_spell },
        { 78,  80, _combat_transformation3_spell },
        {130,  95, _combat_transformation4_spell },
        {195, 110, _combat_transformation5_spell }},
        "Combat Transformation is an offensive power, channelling your mental focus into "
          "enhanced melee fighting. While active, your skills in combat will improve affecting "
          "the accuracy of your blows. Also, your reflexes will quicken in line with your mental "
          "accuity affecting the speed of your attacks. This power requires your constant "
          "focus and as a result, increases the casting costs of all other psionic powers."
    },
    { "Archery Transformation", _PSION_ARCHERY_TRANSFORMATION, 20, {  
        { 13,  50, _archery_transformation1_spell },
        { 49,  65, _archery_transformation2_spell },
        { 78,  80, _archery_transformation3_spell },
        {130,  95, _archery_transformation4_spell },
        {195, 110, _archery_transformation5_spell }},
        "Archery Transformation is an offensive power, channelling your mental focus into "
          "enhanced shooting. While active, your skills with all missile weapons will improve "
          "affecting the accuracy of your shots. Also, you will shoot with increased speed. "
          "This power requires your constant focus and as a result, increases the casting "
          "costs of all other psionic powers."
    },
    { "Ego Whip", _PSION_EGO_WHIP, 20, {  
        {  6,  40, _ego_whip1_spell },
        { 18,  65, _ego_whip2_spell },
        { 36,  80, _ego_whip3_spell },
        { 60,  95, _ego_whip4_spell },
        { 90, 110, _ego_whip5_spell }},
        "Ego Whip is an offensive power which lashes a targetted monster repeatedly with your "
          "psychic energy. The effect lasts for multiple rounds during which time you may "
          "perform other separate actions. However, monsters do get a saving throw against "
          "the ego whip every turn, and if they make a save, they are able to shake off the "
          "whip completely."
    },
    { "Psionic Speed", _PSION_SPEED, 30, {  
        {  6,  40, _psionic_speed1_spell },
        { 18,  65, _psionic_speed2_spell },
        { 36,  80, _psionic_speed3_spell },
        { 60,  95, _psionic_speed4_spell },
        { 90, 110, _psionic_speed5_spell }},
        "Psionic Speed channels your mental energy into speed of motion, granting great "
          "powers of haste. With increased focus come increased speed, and the total "
          "amount of haste can greatly exceed what is possible for other classes."
    },
    { "Psionic Healing", _PSION_HEALING, 30, {  
        {  7,  40, _psionic_healing1_spell }, /*  70hp */
        { 21,  60, _psionic_healing2_spell }, /* 190hp */
        { 42,  80, _psionic_healing3_spell }, /* 310hp */
        { 65, 100, _psionic_healing4_spell }, /* 430hp */
        { 90, 125, _psionic_healing5_spell }},/* 550hp */
        "Psionic Healing is a recovery spell. By focusing your mind, you will be able "
          "to heal your wounds. In addition, cuts, stuns and poison will be cured. With "
          "total focus, you can even restore your stats."
    },
    { "Brain Smash", _PSION_BRAIN_SMASH, 30, {  
        { 10,  60, _brain_smash1_spell },
        { 20,  75, _brain_smash2_spell },
        { 40,  90, _brain_smash3_spell },
        { 70, 105, _brain_smash4_spell },
        {100, 120, _brain_smash5_spell }},
        "Brain Smash is an offensive spell. Although it does no physical damage, it inflicts "
          "a powerful mental attack on your foes which may confuse, stun or slow them."
    },
    { "Mind Wave", _PSION_WAVE, 30, {  
        { 10,  60, _psionic_wave1_spell }, /*  50hp */
        { 20,  75, _psionic_wave2_spell }, /* 100hp */
        { 40,  90, _psionic_wave3_spell }, /* 150hp */
        { 70, 105, _psionic_wave4_spell }, /* 200hp */
        {100, 125, _psionic_wave5_spell }},/* 250hp */
        "Mind Wave unleashes the effects of your mental focus on all visible monsters. The damage is not "
        "as great as Psionic Storm but the ability to affect many monsters at once compensates for this."
    },
    { "Psionic Crafting", _PSION_CRAFTING, 40, {  
        { 10,  50, _psionic_crafting1_spell },
        { 30,  65, _psionic_crafting2_spell },
        { 60,  80, _psionic_crafting3_spell },
        {100,  95, _psionic_crafting4_spell },
        {150, 110, _psionic_crafting5_spell }},
        "Psionic Crafting channels your mental focus into an object, enchanting it in the "
          "process. With maximal focus, you can even craft excellent items!"
    },
    { "Psionic Storm", _PSION_STORM, 40, {  
        { 12,  50, _psionic_storm1_spell }, /* 100hp */
        { 35,  65, _psionic_storm2_spell }, /* 225hp */
        { 65,  80, _psionic_storm3_spell }, /* 350hp */
        {100,  95, _psionic_storm4_spell }, /* 475hp */
        {135, 110, _psionic_storm5_spell }},/* 600hp */
        "Psionic Storm unleashes your mental focus in a large, powerful ball of mana."
    },
    { "Psionic Backlash", _PSION_BACKLASH, 40, {  
        { 24,  50, _psionic_backlash1_spell },
        { 40,  65, _psionic_backlash2_spell },
        { 60,  80, _psionic_backlash3_spell },
        { 90,  95, _psionic_backlash4_spell },
        {130, 110, _psionic_backlash5_spell }},
        "Psionic Backlash is a defensive spell. While active, any enemy that damages you will "
          "take a proportional amount of damage in retaliation. The greater your focus, the "
          "greater the retaliatory damage."
    },
    { "Psychic Drain", _PSION_DRAIN, 40, {  
        { 24,  50, _psionic_drain1_spell },
        { 40,  65, _psionic_drain2_spell },
        { 60,  80, _psionic_drain3_spell },
        { 90,  95, _psionic_drain4_spell },
        {130, 110, _psionic_drain5_spell }},
        "Psychic Drain allows you to draw mental energy and focus from the magic around you. "
        "Whenever you are hit by a magic spell you will convert some of the damage into mana. This "
        "power has no effect on non-magical damage like breaths, rockets or melee."
    },
    { "Psionic Disruption", _PSION_DISRUPTION, 50, {  
        { 40,  40, _psionic_disruption1_spell },
        {120,  55, _psionic_disruption2_spell },
        {240,  70, _psionic_disruption3_spell },
        {400,  85, _psionic_disruption4_spell },
        {600, 100, _psionic_disruption5_spell }},
        "Psionic Disruption allows you to block the minds of others hindering their ability "
        "to cast spells. But be warned: innate monster attacks (breaths and rockets) will not "
        "be affected!"
    },
    { "Mental Fortress", _PSION_FORTRESS, 50, {  
        { 40,  40, _mental_fortress1_spell },
        {120,  55, _mental_fortress2_spell },
        {240,  70, _mental_fortress3_spell },
        {400,  85, _mental_fortress4_spell },
        {600, 100, _mental_fortress5_spell }},
        "Mental Fortress grants immunity to Dispel Magic and Anti-magic. In addition, it "
          "increases the power of your spells."
    },
    { "Mindspring", _PSION_MINDSPRING, 50, {  
        { 40,  40, _mindspring1_spell },
        {120,  55, _mindspring2_spell },
        {240,  70, _mindspring3_spell },
        {400,  85, _mindspring4_spell },
        {600, 100, _mindspring5_spell }},
        "Mindspring greatly enhances your mana recovery."
    },
    { "Psionic Foresight", _PSION_FORESIGHT, 50, {  
        { 40,  40, _psionic_foresight1_spell },
        {120,  55, _psionic_foresight2_spell },
        {240,  70, _psionic_foresight3_spell },
        {400,  85, _psionic_foresight4_spell },
        {600, 100, _psionic_foresight5_spell }},
        "Psionic Foresight allows you to see into the future. With knowledge "
          "of events before they take place, you will be able to avoid many attacks "
          "altogether!"
    },
    { 0 }
};

static _spell_ptr _get_spell(int id)
{
    int i;
    for (i = 0; ; i++)
    {
        _spell_ptr current = &__spells[i];
        if (!current->level)
            break;
        if (current->id == id)
            return current;
    }
    msg_format("Software Bug: Invalid psionic spell id = %d.", id);
    return &__spells[0];
}

static int _num_spells_learned(void)
{
    int i;
    for (i = 0; i < 64; i++) 
    {
        if (plr->spell_order[i] == 99) break;
    }
    return i;
}

static bool _spell_is_known(int idx)
{
    int i;
    for (i = 0; i < 64; i++) 
    {
        if (plr->spell_order[i] == idx) return TRUE;
        if (plr->spell_order[i] == 99) break;
    }
    return FALSE;
}

typedef struct {
    int lvl;
    int color;
} _spell_rank_t;
static _spell_rank_t _spell_ranks[] = {
    {  1, TERM_WHITE },
    { 10, TERM_L_WHITE },
    { 15, TERM_L_UMBER },
    { 20, TERM_YELLOW },
    { 30, TERM_ORANGE },
    { 35, TERM_L_RED },
    { 40, TERM_RED },
    { 50, TERM_VIOLET },
    {  0, TERM_WHITE },
};

static int _num_spells_allowed(void)
{
    int ct = 0, i;
    for (i = 0; ; i++)
    {
        if (_spell_ranks[i].lvl <= 0) break;
        if (plr->lev >= _spell_ranks[i].lvl)
            ct++;
    }
    return ct;
}

bool _can_study(void)
{
    int num = _num_spells_allowed() - _num_spells_learned();
    if (num <= 0) return FALSE;
    return TRUE;
}

static void _study_menu_fn(int cmd, int which, vptr cookie, var_ptr res)
{
    int id = ((int*)cookie)[which];
    _spell_ptr spell = _get_spell(id);
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, spell->name);
        break;
    case MENU_HELP:
        var_set_string(res, spell->desc);
        break;
    case MENU_COLOR:
    {
        int lvl = spell->level;
        int i;
        var_set_int(res, TERM_WHITE);
        for (i = 0; ; i++)
        {
            if (_spell_ranks[i].lvl <= 0) break;
            if (lvl == _spell_ranks[i].lvl)
            {
                var_set_int(res, _spell_ranks[i].color);
                break;
            }
        }
        break;
    }
    default:
        default_menu(cmd, which, cookie, res);
    }
}

static void _study(int level)
{
    int choices[100];
    int i;
    int ct = 0;
    menu_t menu = { "Gain which power?", "Browse which power?", NULL,
                    _study_menu_fn, choices, 0};

    for (i = 0; ; i++)
    {
        _spell_t *spell = &__spells[i];
        if (!spell->level) break;
        if (spell->level <= level && !_spell_is_known(spell->id))
        {
            choices[ct] = spell->id;
            ct++;
        }
    }

    menu.count = ct;
    for (;;)
    {
        i = menu_choose(&menu);
        if (i >= 0)
        {
            char prompt[1024];
            char desc[255*10];
            int id = choices[i];
            _spell_ptr spell = _get_spell(id);
            int j;
            cptr t;

            screen_save();
            for (j = 0; j < 10+1; j++)
                Term_erase(13, 1+j, 255);
            
            roff_to_buf(spell->desc, 80-13, desc, sizeof(desc));
            for (t = desc, j = 0; t[0]; t += strlen(t) + 1, j++)
                prt(t, 2+j, 13);

            sprintf(prompt, "You will learn %s. Are you sure?", spell->name);
            if (get_check(prompt))
            {
                screen_load();
                plr->spell_order[_num_spells_learned()] = spell->id;
                plr->redraw |= PR_EFFECTS;
                msg_format("You have gained %s.", spell->name);
                break;
            }
            screen_load();
        }
        msg_print("Please make a choice!");
    }
}

static void _gain_level(int new_level)
{
    while (_can_study())
        _study(new_level);
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 15;
    spell->cost = 0;
    spell->fail = calculate_fail_rate(spell->level, 30, _spell_stat_idx());
    spell->fn = clear_mind_spell;

    return ct;
}

static void _choose_menu_fn(int cmd, int which, vptr cookie, var_ptr res)
{
    _spell_ptr spell = _get_spell(plr->spell_order[which]);
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, spell->name);
        break;
    case MENU_HELP:
        spell->info[0].fn(SPELL_DESC, res);
        break;
    default:
        default_menu(cmd, which, cookie, res);
    }
}

static int _choose_spell(void)
{
    int i;
    menu_t menu = { "Use which power?", "Browse which power?", NULL,
                    _choose_menu_fn, NULL, _num_spells_learned()};

    i = menu_choose(&menu);
    if (i >= 0)
        i = plr->spell_order[i];
    return i;
}

static int _get_spells(spell_info* spells, int max)
{
    int       i, id, stat, ct = 0;
    _spell_t *base;

    /* First Choose which Psionic Spell to use */
    id = _choose_spell();
    if (id < 0) return 0;

    stat = _spell_stat_idx();
    base = _get_spell(id);

    /* Then Choose which power level of that spell to use */
    for (i = 0; i < _MAX_POWER; i++)
    {
        if (ct >= max) break;
        if (base->level <= plr->lev)
        {
            spell_info* current = &spells[ct];
            int fail = base->info[i].fail;
            int cost = base->info[i].cost;

            current->level = base->level;
            current->fn = base->info[i].fn;

            if (plr_tim_find(_CLARITY))
            {
                cost = cost * (85 - 7 * plr_tim_parm(_CLARITY)) / 100;
                if (cost < 1)
                    cost = 1;
            }

            if (plr_tim_find(_COMBAT) || plr_tim_find(_ARCHERY))
            {
                cost = cost * 3 / 2;
            }

            current->cost = cost;
            current->fail = calculate_fail_rate(base->level, fail, stat);
            ct++;
        }
    }
    return ct;
}

static void _calc_bonuses(void)
{
    if (equip_find_art("~.Mind"))
    {
        plr->dec_mana++;
        plr->easy_spell++;
    }

    if (plr->lev >= 15)
        plr->clear_mind = TRUE;

}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "focus";
        me.encumbrance.max_wgt = 400;
        me.encumbrance.weapon_pct = 50;
        me.encumbrance.enc_wgt = 800;
        init = TRUE;
    }
    me.which_stat = _spell_stat();
    return &me;
}

static void _character_dump(doc_ptr doc)
{
    int     i, j;
    int     stat = _spell_stat_idx();
    int     num_learned = _num_spells_learned();
    var_t   name = var_create();
    var_t   info = var_create();

    doc_printf(doc, "<topic:Spells>=================================== <color:keypress>S</color>pells ====================================\n");

    for (i = 0; i < num_learned; i++)
    {
        _spell_t *power = _get_spell(plr->spell_order[i]);

        doc_printf(doc, "\n<color:G>%-23.23s Cost Fail %-15.15s Cast Fail</color>\n", power->name, "Info");
        for (j = 0; j < _MAX_POWER; j++)
        {
            _spell_info_t  *spell = &power->info[j];
            int             fail = spell->fail;
            int             cost = spell->cost;
            spell_stats_ptr stats = NULL;

            if (plr_tim_find(_CLARITY))
            {
                cost = cost * (85 - 7 * plr_tim_parm(_CLARITY)) / 100;
                if (cost < 1)
                    cost = 1;
            }

            if (plr_tim_find(_COMBAT) || plr_tim_find(_ARCHERY))
            {
                cost = cost * 3 / 2;
            }

            fail = calculate_fail_rate(power->level, fail, stat);

            (spell->fn)(SPELL_NAME, &name);
            stats = spell_stats_aux(var_get_string(&name));

            (spell->fn)(SPELL_INFO, &info);
            doc_printf(doc, "%-23.23s %4d %3d%% %-15.15s %4d %4d %3d%%\n",
                            var_get_string(&name),
                            cost,
                            fail,
                            var_get_string(&info),
                            stats->ct_cast, stats->ct_fail,
                            spell_stats_fail(stats)
            );

        }
    }

    var_destroy(&name);
    var_destroy(&info);
    doc_newline(doc);
}

static void _player_action(void)
{
    _do_mindspring(energy_use);
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_SMALL_SWORD, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_obj_aux(TV_POTION, SV_POTION_CLARITY, rand_range(5, 10));
}

plr_class_ptr psion_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  35,  40,   2,  16,   8,  48,  35};
    skills_t xs = { 35,  55,  60,   0,   0,   0,  65,  55};

        me = plr_class_alloc(CLASS_PSION);
        me->name = "Psion";
        me->desc = "The Psion is like a Mindcrafter, and uses innate mental powers. "
                    "Unlike the Mindcrafter, however, Psions have the freedom to learn "
                    "powers that enforce their own styles. They learn very few powers, "
                    "but they can scale their powers to determine the SP cost and the "
                    "powers' potency. Psionic powers require great concentration, however, "
                    "and psions do not have the mind to spare to care for others. "
                    "The Psion gains powers a the following levels: 1, 10, 15, 20, 30, 35, 40 and 50. "
                    "The Psion uses Intelligence, Wisdom or Charisma as their primary "
                    "spell stat, which ever is currently highest. In this respect, Psions "
                    "are truly unique!";

        me->stats[A_STR] = -1;
        me->stats[A_INT] =  2;
        me->stats[A_WIS] =  2;
        me->stats[A_DEX] = -1;
        me->stats[A_CON] = -1;
        me->stats[A_CHR] =  2;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 100;
        me->base_hp = 4;
        me->exp = 150;
        me->pets = 35;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_WEAK |
                    CLASS_SENSE2_MED | CLASS_SENSE2_STRONG;

        me->hooks.birth = _birth;
        me->hooks.register_timers = _register_timers;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_spells = _get_spells;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = _character_dump;
        me->hooks.gain_level = _gain_level;
        me->hooks.player_action = _player_action;
    }

    return me;
}

void psion_relearn_powers(void)
{
    int i;
    for (i = 0; i < 64; i++) 
        plr->spell_order[i] = 99;

    for (i = 0; ; i++)
    {
        if (_spell_ranks[i].lvl <= 0) break;
        if (plr->lev >= _spell_ranks[i].lvl)
            _study(_spell_ranks[i].lvl);
    }
}
