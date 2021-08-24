#include "angband.h"

/************************************************************************
 * Timers
 ************************************************************************/
enum {
    _SIGHT = T_CUSTOM,
    _SHIELD,
    _SEEK,
    _FEAST,
    _REVENGE,
};

/* _SIGHT */
static bool _sight_on(plr_tim_ptr timer)
{
    msg_print("You sense life!");
    plr->update |= PU_BONUS | PU_MONSTERS;
    return TRUE;
}
static void _sight_off(plr_tim_ptr timer)
{
    msg_print("You no longer sense life.");
    plr->update |= PU_BONUS | PU_MONSTERS;
}
static void _sight_bonus(plr_tim_ptr timer)
{
    plr->esp_living = TRUE;
}
static void _sight_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    /*add_flag(flgs, OF_ESP_LIVING);*/
}
static status_display_t _sight_display(plr_tim_ptr timer)
{
    return status_display_create("Sight", "Si", TERM_L_BLUE);
}
static plr_tim_info_ptr _sight_timer(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_SIGHT, "Blood Sight");
    info->desc = "You sense the presence of living monsters around you.";
    info->on_f = _sight_on;
    info->off_f = _sight_off;
    info->calc_bonuses_f = _sight_bonus;
    info->flags_f = _sight_flags;
    info->status_display_f = _sight_display;
    return info;
}

/* _SHIELD */
static bool _shield_on(plr_tim_ptr timer)
{
    msg_print("You are shielded in blood!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _shield_off(plr_tim_ptr timer)
{
    msg_print("Your blood shield vanishes.");
    plr->update |= PU_BONUS;
}
static void _shield_bonus(plr_tim_ptr timer)
{
    int amt = 100 * (plr->mhp - plr->chp) / plr->mhp; 
    plr_bonus_ac(amt);
    if (amt > 60)
        plr->reflect = TRUE;
}
static void _shield_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    int amt = 100 * (plr->mhp - plr->chp) / plr->mhp; 
    if (amt > 60)
        add_flag(flgs, OF_REFLECT);
}
static status_display_t _shield_display(plr_tim_ptr timer)
{
    return status_display_create("Shield", "Sh", TERM_ORANGE);
}
static plr_tim_info_ptr _shield_timer(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_SHIELD, "Blood Shield");
    info->desc = "You are protected by a blood shield.";
    info->on_f = _shield_on;
    info->off_f = _shield_off;
    info->calc_bonuses_f = _shield_bonus;
    info->flags_f = _shield_flags;
    info->status_display_f = _shield_display;
    info->dispel_prob = 100;
    return info;
}

/* _SEEK */
static bool _seek_on(plr_tim_ptr timer)
{
    msg_print("Your weapon thirsts for life!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _seek_off(plr_tim_ptr timer)
{
    msg_print("Your weapon no longer thirsts for life.");
    plr->update |= PU_BONUS;
}
static void _seek_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    if (info->type != PAT_WEAPON) return;
    add_flag(info->obj_flags, OF_SLAY_LIVING);
    add_flag(info->obj_known_flags, OF_SLAY_LIVING);
}
static void _seek_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SLAY_LIVING);
}
static status_display_t _seek_display(plr_tim_ptr timer)
{
    return status_display_create("Seek", "Sk", TERM_YELLOW);
}
static plr_tim_info_ptr _seek_timer(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_SEEK, "Blood Seeking");
    info->desc = "Your weapon thirsts for life.";
    info->on_f = _seek_on;
    info->off_f = _seek_off;
    info->calc_weapon_bonuses_f = _seek_weapon_bonus;
    info->flags_f = _seek_flags;
    info->status_display_f = _seek_display;
    return info;
}

/* _FEAST */
static bool _feast_on(plr_tim_ptr timer)
{
    msg_print("Let the feast begin!!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _feast_off(plr_tim_ptr timer)
{
    msg_print("The feast has ended!");
    plr->update |= PU_BONUS;
}
static void _feast_weapon_bonus(plr_tim_ptr timer, obj_ptr obj, plr_attack_info_ptr info)
{
    if (info->type != PAT_WEAPON) return;
    info->to_d += 35;
    info->dis_to_d += 35;
}
static status_display_t _feast_display(plr_tim_ptr timer)
{
    return status_display_create("Feast", "Fs", TERM_WHITE);
}
static plr_tim_info_ptr _feast_timer(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_FEAST, "Blood Feast");
    info->desc = "Your weapon feasts on blood, granting extra damage but at a cost to your own health.";
    info->on_f = _feast_on;
    info->off_f = _feast_off;
    info->calc_weapon_bonuses_f = _feast_weapon_bonus;
    info->status_display_f = _feast_display;
    info->dispel_prob = 100;
    return info;
}

/* _REVENGE */
static bool _revenge_on(plr_tim_ptr timer)
{
    msg_print("Its time for bloody revenge!");
    return TRUE;
}
static void _revenge_off(plr_tim_ptr timer)
{
    msg_print("The time for bloody revenge has passed.");
}
static status_display_t _revenge_display(plr_tim_ptr timer)
{
    return status_display_create("Revenge", "Rv", TERM_RED);
}
static plr_tim_info_ptr _revenge_timer(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_REVENGE, "Bloody Revenge");
    info->desc = "You exact retribution on those that dare attack you.";
    info->on_f = _revenge_on;
    info->off_f = _revenge_off;
    info->status_display_f = _revenge_display;
    info->dispel_prob = 100;
    return info;
}

static void _register_timers(void)
{
    plr_tim_register(_sight_timer());
    plr_tim_register(_shield_timer());
    plr_tim_register(_seek_timer());
    plr_tim_register(_feast_timer());
    plr_tim_register(_revenge_timer());
}

/************************************************************************
 * Melee
 ************************************************************************/
static void _feast_hit(plr_attack_ptr context)
{
    if (context->info.type != PAT_WEAPON) return;
    take_hit(DAMAGE_ATTACK, 15, "blood feast");
}
static void _attack_init(plr_attack_ptr context)
{
    if (plr_tim_find(_FEAST))
        context->hooks.after_hit_f = _feast_hit;
}
static void _revenge_hit(mon_attack_ptr context)
{
    int dam;
    if (context->stop) return;
    if (!mon_is_living(context->mon)) return;

    /* Scale the damage based on cuts and monster deadliness */
    dam = context->dam * plr_tim_amount(T_CUT) / CUT_SEVERE;

    /* Balance out a weak melee attack */
    if (dam < plr_tim_amount(T_CUT) / 10)
        dam = plr_tim_amount(T_CUT) / 10;

    /* Not too powerful */
    if (dam > 50)
        dam = 50;

    dam = mon_damage_mod(context->mon, dam, FALSE);
    if (dam > 0)
    {
        msg_format("%^s feels your bloody revenge!", context->mon_name);
        if (mon_take_hit(context->mon, dam, &context->fear, " turns into a pool of blood."))
            context->stop = STOP_MON_DEAD;
    }
}
static void _mon_attack_init(mon_attack_ptr context)
{
    if (plr_tim_find(_REVENGE))
        context->after_hit_f = _revenge_hit;
}
/************************************************************************
 * Spells
 ************************************************************************/
static void _flow_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Cut Self");
        break;
    case SPELL_DESC:
        var_set_string(res, "Cut yourself to improve your melee prowess.");
        break;
    case SPELL_CAST:
    {
        int cut = plr_tim_amount(T_CUT);
        cut += cut/5;
        if (cut < CUT_LIGHT)
            cut = CUT_LIGHT;

        plr_tim_add(T_CUT, cut);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _sight_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Living");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects living creatures in the vicinity.");
        break;
    case SPELL_CAST:
    {
        if (plr->lev < 30)
            detect_monsters_living(DETECT_RAD_DEFAULT, "You sense potential blood!");
        else
            plr_tim_add(_SIGHT, randint1(30) + 30);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _spray_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Gouge Self");
        break;
    case SPELL_DESC:
        var_set_string(res, "Inflict a deep wound, spraying your own blood to damage nearby monsters.");
        break;
    default:
        burst_spell_aux(cmd, res, 3 + plr->lev/30, GF_BLOOD, dice_create(3, 5, 5*plr->lev/4));
    }
}

static void _bath_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blood Bath");
        break;
    case SPELL_DESC:
        var_set_string(res, "Restores constitution and cures poison.");
        break;
    case SPELL_CAST:
    {
        bool chg = FALSE;
        if (do_res_stat(A_CON)) chg = TRUE;
        if (plr_tim_recover(T_POISON, 80, 100)) chg = TRUE;
        if (!chg) msg_print("You don't need a bath just yet.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _shield_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Crimson Shield");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives bonus to AC depending on how wounded you are. Grants reflection if you are really hurting.");
        break;

    case SPELL_CAST:
    {
        plr_tim_add(_SHIELD, randint1(20) + 30);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _seeking_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Slay Living");
        break;
    case SPELL_DESC:
        var_set_string(res, "Temporarily grants slay living to your weapon.");
        break;
    case SPELL_CAST:
    {
        plr_tim_add(_SEEK, randint1(30) + 30);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _rage_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blood Lust");
        break;
    case SPELL_DESC:
        var_set_string(res, "Enter a frenzied state granting enhanced speed and big bonuses to hit and damage.");
        break;
    case SPELL_CAST:
    {
        int dur = randint1(plr->lev/2) + plr->lev/2;
        plr_tim_add(T_FAST, dur);
        plr_tim_add(T_BERSERK, dur);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _feast_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Banquet of Azathoth");
        break;
    case SPELL_DESC:
        var_set_string(res, "You begin to feast on your opponents blood, doing extra damage but at a cost to your own health.");
        break;
    case SPELL_CAST:
    {
        var_set_bool(res, FALSE);
        if (plr_tim_find(_FEAST))
        {
            if (!get_check("Cancel the Blood Feast? ")) return;
            plr_tim_remove(_FEAST);
        }
        else
        {
            plr_tim_add(_FEAST, randint1(5) + 5);
            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _revenge_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Revenge");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives an aura of bloody revenge. Living monsters take damage based on your cut status.");
        break;
    case SPELL_CAST:
        plr_tim_add(_REVENGE, randint1(5) + 5);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static bool _is_blood_potion(obj_ptr obj)
    { return obj->tval == TV_POTION && obj->sval == SV_POTION_BLOOD; }
static int _count_blood_potions(void)
    { return pack_count(_is_blood_potion); }

static void _pool_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "The Flow of Life");
        break;
    case SPELL_DESC:
        var_set_string(res, "Creates a macabre Potion of Healing made of your own blood.");
        break;
    case SPELL_CAST:
    {
        object_type forge;
        int ct = _count_blood_potions();

        if (ct >= 20)
        {
            msg_print("You have too many blood potions at the moment. Why not drink some?");
            var_set_bool(res, FALSE);
            return;
        }

        msg_print("You feel light headed.");
        object_prep(&forge, lookup_kind(TV_POTION, SV_POTION_BLOOD));

        pack_carry(&forge);
        msg_print("You store your blood for future use.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _explosion_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Red Death");
        break;
    case SPELL_DESC:
        var_set_string(res, "Damages all living creatures in sight at tremendous cost to your own health.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, 500));
        break;
    case SPELL_CAST:
    {
        msg_print("You cut too deep ... Your blood explodes!");
        plr_project_los(GF_DISP_LIVING, 500);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _cauterize_wounds_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Cauterize Wounds");
        break;
    case SPELL_DESC:
        var_set_string(res, "Cures cuts");
        break;
    case SPELL_CAST:
        plr_tim_remove(T_CUT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

/************************************************************************
 * Class Hooks
 ************************************************************************/
static power_info _powers[] =
{
    { A_CON, {30, 20, 50, _cauterize_wounds_spell} }, 
    { -1, { -1, -1, -1, NULL} }
};

static spell_info _spells[] = 
{
    /*lvl cst fail spell */
    {  1,   1, 20, _flow_spell },
    {  5,   5, 30, _sight_spell},
    { 10,  10, 30, _spray_spell},
    { 15,  20, 30, _bath_spell},
    { 20,  30, 30, _shield_spell},
    { 25,  50, 40, _seeking_spell},
    { 30,  60, 40, _rage_spell},
    { 40,  60, 50, _feast_spell},
    { 42,  60,  0, _revenge_spell},
    { 45, 200,  0, _pool_spell},
    { 50, 500, 60, _explosion_spell},
    { -1, -1,  -1, NULL}
}; 

static int _get_spells(spell_info* spells, int max)
{
    return get_spells_aux(spells, max, _spells);
}

static int _get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _powers);
}

static void _character_dump(doc_ptr doc)
{
    spell_info spells[MAX_SPELLS];
    int        ct = _get_spells(spells, MAX_SPELLS);

    plr_display_spells(doc, spells, ct);
}

static void _calc_bonuses(void)
{
    int cut = plr_tim_amount(T_CUT);
    plr->regen += 100 + 2*plr->lev;
    if (plr->lev >= 30) res_add(GF_FEAR);

    if (cut > 0)
    {
        int to_stealth = 0;
        if (cut >= CUT_MORTAL_WOUND)
            to_stealth = -10;
        else if (cut >= CUT_DEEP_GASH)
            to_stealth = -3;
        else if (cut >= CUT_SEVERE)
            to_stealth = -2;
        else if (cut >= CUT_NASTY)
            to_stealth = -2;
        else if (cut >= CUT_BAD)
            to_stealth = -1;
        else if (cut >= CUT_LIGHT)
            to_stealth = -1;
        else
            to_stealth = -1;
        plr->skills.stl += to_stealth;
    }

}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_REGEN);
    if (plr->lev >= 30) add_flag(flgs, OF_RES_(GF_FEAR));
}

static void _calc_weapon_bonuses(object_type *o_ptr, plr_attack_info_ptr info)
{
    int cut = plr_tim_amount(T_CUT);
    if (plr->chp < plr->mhp)
    {
        int frac = plr->chp * 100 / plr->mhp;
        int max = plr_prorata_level(800);
        int blows = (100 - frac) * max / 90; /* XXX overcharge blows below 10% health (unwise) */

        info->xtra_blow += 100;   /* 3.00x to 4.00x base blows */
        info->xtra_blow += blows; /* more blows based on pct wounded (up to 12.00x) */
    }
    if (cut > 0)
    {
        int to_h = 0;
        int to_d = 0;
        if (cut >= CUT_MORTAL_WOUND)
        {
            to_h = 25;
            to_d = 25;
        }
        else if (cut >= CUT_DEEP_GASH)
        {
            to_h = 15;
            to_d = 15;
        }
        else if (cut >= CUT_SEVERE)
        {
            to_h = 8;
            to_d = 8;
        }
        else if (cut >= CUT_NASTY)
        {
            to_h = 6;
            to_d = 6;
        }
        else if (cut >= CUT_BAD)
        {
            to_h = 4;
            to_d = 4;
        }
        else if (cut >= CUT_LIGHT)
        {
            to_h = 2;
            to_d = 2;
        }
        else
        {
            to_h = 1;
            to_d = 1;
        }
        info->to_h += to_h;
        info->dis_to_h += to_h;

        info->to_d += to_d;
        info->dis_to_d += to_d;
    }
}

static void _on_cast(const spell_info *spell)
{
    plr_tim_add(T_CUT, spell->level);
    plr->update |= PU_BONUS;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "bloodcraft";
        me.options = CASTER_USE_HP;
        me.which_stat = A_CON;
        me.on_cast = _on_cast;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_BROAD_SWORD, 1);
    plr_birth_obj_aux(TV_HARD_ARMOR, SV_CHAIN_MAIL, 1);
    plr_birth_obj_aux(TV_POTION, SV_POTION_CURE_CRITICAL, rand_range(2, 5));
}

/************************************************************************
 * Public
 ************************************************************************/
plr_class_ptr blood_knight_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  32,   2,  16,   6,  70,  20};
    skills_t xs = { 60,  35,  50,   0,   0,   0, 115,  75};

        me = plr_class_alloc(CLASS_BLOOD_KNIGHT);
        me->name = "Blood-Knight";
        me->desc = "A Blood-Knight is a fighter who has delved into the dark arts and can perform "
                  "a limited number of offensive effects using his own health. In addition to the "
                  "HP cost, using an ability also causes bleeding/wounds, with an amount proportional "
                  "to the cost of the ability. Their primary stat for abilities is Con.\n \n"
                  "Blood-Knights are strong in melee, but are very unusual in the fact that the more "
                  "wounded they are, the stronger they become. When damaged, they gain additional "
                  "melee attacks and damage, and moreso the more wounded and cut they become. Indeed, when on the "
                  "brink of death they are the strongest fighters imaginable, and stories of their "
                  "legendary feats abound. Of course, with great power comes great risk of death, "
                  "and you don't recall ever meeting one of these heroes of legend in person!\n \n"
                  "Since the Blood-Knight relies on their own blood for their power, they are restricted "
                  "to only certain races. No non-living race may walk the red path.";

        me->stats[A_STR] =  2;
        me->stats[A_INT] = -2;
        me->stats[A_WIS] = -2;
        me->stats[A_DEX] =  0;
        me->stats[A_CON] =  3;
        me->stats[A_CHR] =  2;
        
        me->skills = bs;
        me->extra_skills = xs;
        
        me->life = 120;
        me->base_hp = 20;
        me->exp = 150;
        me->pets = 40;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_STRONG;

        me->hooks.birth = _birth;
        me->hooks.attack_init = _attack_init;
        me->hooks.mon_attack_init = _mon_attack_init;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_spells = _get_spells;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = _character_dump;
        me->hooks.register_timers = _register_timers;
    }

    return me;
}

