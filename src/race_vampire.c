#include "angband.h"

#include <assert.h>

static const char * _desc = 
    "One of the mightier undead creatures, the Vampire is an awe-inspiring sight. Yet this "
    "dread creature has a serious weakness: the bright rays of sun are its bane, and it "
    "will need to flee the surface to the deep recesses of earth until the sun finally "
    "sets. Darkness, on the other hand, (eventually) only makes the Vampire stronger. "
    "As undead, the Vampire has a firm hold on its life force, and resists nether attacks. "
    "The Vampire also resists cold and poison based attacks. It is, however, susceptible to its "
    "perpetual hunger for fresh blood, which can only be satiated by sucking the blood "
    "from a nearby monster.\n \n"
    "The Vampire is a monster race and cannot choose a normal class. Instead, the vampire gains "
    "access to various dark powers as they evolve. Of course, they gain a vampiric bite at a "
    "very early stage, as they must use this power to feed on the living. Killing humans with this "
    "power also is a means of perpetuating the vampire species, and many are the servants of "
    "true prince of darkness! Vampires are also rumored to have limited shapeshifting abilities "
    "and a powerful, hypnotic gaze.";

bool vampiric_drain_hack = FALSE;

/******************************************************************************
 *                  25                35              45
 * Vampire: Vampire -> Master Vampire -> Vampire Lord -> Elder Vampire
 ******************************************************************************/
static void _birth(void) 
{ 
    object_type forge;

    plr_mon_race_set("V.vampire");
    
    object_prep(&forge, lookup_kind(TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_SWORD, SV_DAGGER));
    forge.name2 = EGO_WEAPON_DEATH;
    plr_birth_obj(&forge);

    /* Encourage shapeshifting! */
    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_d = 4;
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);
}

static void _gain_level(int new_level) 
{
    if (plr_mon_race_is_("V.vampire") && new_level >= 25)
        plr_mon_race_evolve("V.master");
    if (plr_mon_race_is_("V.master") && new_level >= 35)
        plr_mon_race_evolve("V.lord");
    if (plr_mon_race_is_("V.lord") && new_level >= 45)
        plr_mon_race_evolve("V.elder");
}

/******************************************************************************
 * Powers
 ******************************************************************************/
static int _bite_amt(void)
{
    return 5 + plr_prorata_level_aux(300, 1, 2, 3);
}
static void _bite_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Vampiric Bite");
        break;
    case SPELL_DESC:
        var_set_string(res, "As a vampire, you must feed on fresh blood in order to sustain your unlife!");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _bite_amt()));
        break;
    case SPELL_CAST: {
        mon_ptr mon = plr_target_adjacent_mon();
        int amt;

        var_set_bool(res, FALSE);
        if (!mon) break;

        msg_print("You grin and bare your fangs...");
        amt = _bite_amt();

        vampiric_drain_hack = TRUE;  /* cf monster_death */
        if (plr_touch_mon(mon, GF_OLD_DRAIN, amt))
            vampire_feed(amt);
        else if (!mon_is_living(mon))
            msg_print("Yechh. That tastes foul.");
        vampiric_drain_hack = FALSE;

        var_set_bool(res, TRUE);
        break; }
    case SPELL_COST_EXTRA:
        var_set_int(res, MIN(_bite_amt() / 10, 29));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static int _gaze_power(void)
{
    int power = plr->lev;
    if (plr->lev > 40)
        power += plr->lev - 40;
    power += adj_con_fix[plr->stat_ind[A_CHR]] - 1;
    return power;
}
void _gaze_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Vampiric Gaze");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to dominate an intelligent foe causing stunning, confusion, fear or perhaps even enslavement.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_power(_gaze_power()));
        break;
    default:
        ball_spell(cmd, res, 0, GF_DOMINATION, _gaze_power());
    }
}
void _grasp_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Vampiric Grasp");
        break;
    case SPELL_DESC:
        var_set_string(res, "Pulls a target creature to you.");
        break;
    case SPELL_CAST: {
        bool         fear = FALSE;
        mon_ptr      mon = plr_target_mon();
        char         m_name[MAX_NLEN];

        var_set_bool(res, FALSE);
        if (!mon) break;
        var_set_bool(res, TRUE);

        monster_desc(m_name, mon, 0);
        if (_1d(100) <= mon_res_pct(mon, GF_TELEPORT))
        {
            mon_lore_resist(mon, GF_TELEPORT);
            msg_format("%s resists!", m_name);
        }
        else
        {
            msg_format("You grasp %s.", m_name);
            teleport_monster_to(mon, plr->pos, 100, TELEPORT_PASSIVE);
            mon_take_hit(mon, damroll(10, 10), &fear, extract_note_dies(real_r_ptr(mon)));
        }
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

void equip_shuffle(cptr tag)
{
    slot_t slot, equip_slot;
    for (slot = 1; slot <= pack_max(); slot++)
    {
        obj_ptr obj = pack_obj(slot);
        cptr    inscription;

        if (!obj) continue;
        if (!obj->inscription) continue;
        
        inscription = quark_str(obj->inscription);
        if (!strstr(inscription, tag)) continue;
        
        equip_slot = equip_first_empty_slot(obj);
        if (equip_slot)
        {
            equip_wield(obj, equip_slot);
            obj_release(obj, OBJ_RELEASE_QUIET);
        }
    }
}

static void _set_mimic_form(int which)
{
    plr->mimic_form = which;
    equip_on_change_race();

    if (plr->action == ACTION_QUICK_WALK || plr->action == ACTION_STALK) /* Wolf form ... */
        set_action(ACTION_NONE);

    plr->redraw |= PR_BASIC | PR_STATUS | PR_MAP | PR_EQUIPPY;
    plr->update |= PU_BONUS | PU_INNATE | PU_HP;
    handle_stuff();
}

static void _polymorph_undo_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Return to Vampire");
        break;
    case SPELL_DESC:
        var_set_string(res, "You stop assuming your current form and revert to your natural, vampiric self.");
        break;
    case SPELL_CAST:
        _set_mimic_form(MIMIC_NONE);
        equip_shuffle("@vampire1");
        equip_shuffle("@vampire2");
        equip_shuffle("@vampire3");
        equip_shuffle("@vampire4");
        equip_shuffle("@vampire");
        msg_print("You revert to your natural form.");
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _polymorph_bat_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Polymorph Bat");
        break;
    case SPELL_DESC:
        var_set_string(res, "You assume the form of a giant bat. This grants incredible speed, stealth and sensory awareness, but makes you extremely fragile. Also, bats have very restricted equipment options!");
        break;
    case SPELL_CAST:
        _set_mimic_form(MIMIC_BAT);
        equip_shuffle("@bat");
        msg_print("You transform into a vampire bat!");
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _polymorph_mist_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Polymorph Mist");
        break;
    case SPELL_DESC:
        var_set_string(res, "You lose your corporeal form to assume a cloud of evil sentient mist!");
        break;
    case SPELL_CAST:
        _set_mimic_form(MIMIC_MIST);
        equip_shuffle("@mist");
        msg_print("You transform into vampiric mist!");
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _polymorph_wolf_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Polymorph Wolf");
        break;
    case SPELL_DESC:
        var_set_string(res, "You assume the form of a wolf, hungry for prey.");
        break;
    case SPELL_CAST:
        _set_mimic_form(MIMIC_WOLF);
        equip_shuffle("@wolf");
        msg_print("You transform into a dire wolf!");
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static spell_info _spells[] = 
{
    {  2,  1, 30, _bite_spell },
    {  5,  3, 30, detect_life_spell },
    {  7,  4, 30, _polymorph_bat_spell },
    { 11,  7, 35, _polymorph_wolf_spell },
    { 15, 12, 40, _gaze_spell },
    { 20, 15, 40, create_darkness_spell },
    { 25,  7, 40, nether_bolt_spell },       /* Master Vampire */
    { 25, 10, 50, mind_blast_spell },
    { 25, 20, 50, _polymorph_mist_spell },
    { 35, 25, 50, nether_ball_spell },       /* Vampire Lord */
    { 35, 30, 60, _grasp_spell },
    { 40, 50, 70, repose_of_the_dead_spell },
    { 45, 50, 80, darkness_storm_II_spell }, /* Elder Vampire */
    { -1, -1, -1, NULL}
};

static int _get_spells(spell_info* spells, int max) 
{
    return get_spells_aux(spells, max, _spells);
}

static caster_info * _caster_info(void) 
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "dark power";
        me.which_stat = A_CHR;
        me.encumbrance.max_wgt = 450;
        me.encumbrance.weapon_pct = 50;
        me.encumbrance.enc_wgt = 800;
        me.options = CASTER_GAIN_SKILL;
        init = TRUE;
    }
    return &me;
}

/******************************************************************************
 * Bonuses (... and Penalties!)
 ******************************************************************************/
static int _light_penalty = 0; /* scaled by 100 */

static void _calc_bonuses(void) 
{
    plr->align -= 200;

    res_add(GF_DARK);
    res_add(GF_NETHER);
    res_add(GF_COLD);
    res_add(GF_POIS);
    res_add_vuln(GF_LIGHT);
    plr->hold_life++;
    plr->see_nocto = DUN_VIEW_MAX;

    if (equip_find_art("\".Night"))
    {
        plr->dec_mana++;
        plr->easy_spell++;
    }

    if (plr->lev >= 35)
    {
        res_add(GF_DARK);
        plr->levitation = TRUE;
        plr->pspeed += 1;
        plr->regen += 100;
        plr->self_lite--;
    }

    if (plr->lev >= 45)
    {
        res_add_immune(GF_DARK);
        plr->pspeed += 2;
        plr->self_lite--;
    }

    if (_light_penalty)
    {
        plr->to_a -= 35*_light_penalty/1000;
        plr->dis_to_a -= 35*_light_penalty/1000;

        plr->life -= 25*_light_penalty/1000;
    }
}

static void _calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    if (_light_penalty)
    {
        info->dis_to_h -= 25*_light_penalty/1000;
        info->to_h -= 25*_light_penalty/1000;
    }
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE]) 
{
    add_flag(flgs, OF_VULN_(GF_LIGHT));

    add_flag(flgs, OF_RES_(GF_NETHER));
    add_flag(flgs, OF_RES_(GF_COLD));
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_RES_(GF_DARK));
    add_flag(flgs, OF_HOLD_LIFE);
    if (plr->lev >= 35)
    {
        add_flag(flgs, OF_LEVITATION);
        add_flag(flgs, OF_SPEED);
        add_flag(flgs, OF_REGEN);
        add_flag(flgs, OF_DARKNESS);
    }
    if (plr->lev >= 45)
        add_flag(flgs, OF_IM_(GF_DARK));
}

static void _update_light(void)
{
    vampire_check_light_status();
}

/******************************************************************************
 * Public
 ******************************************************************************/
plr_race_ptr mon_vampire_get_race(void)
{
    static plr_race_ptr me = NULL;
    static cptr titles[4] =  {"Vampire", "Master Vampire", "Vampire Lord", "Elder Vampire"};    
    int         rank = 0;

    if (plr->lev >= 25) rank++;
    if (plr->lev >= 35) rank++;
    if (plr->lev >= 45) rank++;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  37,  36,   0,  32,  25,  60,  35};
    skills_t xs = { 35,  60,  50,   0,   0,   0, 105,  55};

        me = plr_race_alloc(RACE_MON_VAMPIRE);
        me->name = "Vampire";
        me->desc = _desc;

        me->skills = bs;
        me->extra_skills = xs;

        me->base_hp = 20;
        me->exp = 250;
        me->infra = 5;
        me->shop_adjust = 130;

        me->hooks.birth = _birth;
        me->hooks.gain_level = _gain_level;
        me->hooks.update_light = _update_light;
        me->hooks.get_spells = _get_spells;
        me->hooks.caster_info = _caster_info;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.timer_on = repose_timer_on;
        me->hooks.timer_off = repose_timer_off;

        me->flags = RACE_IS_NONLIVING | RACE_IS_UNDEAD | RACE_IS_MONSTER;
        me->pseudo_class_id = CLASS_ROGUE;

        me->boss_r_idx = mon_race_parse("V.Vlad")->id;
    }

    me->subname = titles[rank];
    me->stats[A_STR] =  2 + rank;
    me->stats[A_INT] =  1;
    me->stats[A_WIS] = -1 - rank;
    me->stats[A_DEX] =  0 + rank;
    me->stats[A_CON] = -2;
    me->stats[A_CHR] =  1 + 3*rank/2;
    me->life = 90 + 3*rank;

    me->skills.stl = 7 + 4*rank/3; /* 7, 8, 9, 11 */

    me->equip_template = plr_equip_template();

    if (birth_hack || spoiler_hack)
    {
        me->subname = NULL;
        me->subdesc = NULL;
    }
    return me;
}

void vampire_feed(int amt)
{
    int food;
    int div = 4;

    if (prace_is_(MIMIC_BAT))
        div = 16;

    if (plr->food < PY_FOOD_FULL)
        hp_player(amt);
    else
        msg_print("You were not hungry.");

    /* Experimental: Scale the feeding asymptotically. Historically, vampiric feeding
        was too slow in the early game (low damage) hence tedious. But by the end game,
        a mere two bites would fill the vampire, rendering the talent rather useless. */
    if (plr->food < PY_FOOD_VAMP_MAX)
        food = plr->food + (PY_FOOD_VAMP_MAX - plr->food) / div;
    /* Exceeding PY_FOOD_VAMP_MAX is unlikely, but possible (eg. eating rations of food?!) */
    else if (plr->food < PY_FOOD_MAX)
        food = plr->food + (PY_FOOD_MAX - plr->food) / div;
    else
        food = plr->food + amt;

    assert(food >= plr->food);
    set_food(food);
}

void vampire_check_light_status(void)
{
    static int _last_light_penalty = 100000; /* XXX no fear when restarting game */
    int lite = plr_light(plr->pos);

    if (lite > 0)
    {
        int amt = 100 * lite;
        int pct = res_pct(GF_LIGHT);

        amt -= pct * amt / 100;
        _light_penalty = MAX(0, amt);
    }
    else
        _light_penalty = 0;

    if (_light_penalty != _last_light_penalty)
    {
        if (_light_penalty > _last_light_penalty)
        {
            int n = _light_penalty * MAX(1, cave->difficulty/2) / 100;
            if (!fear_save_p(n))
            {
                msg_print("You fear the light!");
                fear_add_p(FEAR_SCARED);
            }
            else if (plr->wizard)
                msg_format("<color:B>You don't mind the light <color:U>(%d)</color>.</color>", n);
        }
        _last_light_penalty = _light_penalty;
        plr->update |= PU_BONUS;
        plr->redraw |= PR_STATUS;
    }
}

void vampire_take_light_damage(int amt)
{
    if (!fear_save_p(amt))
    {
        msg_print("You fear the light!");
        fear_add_p(FEAR_SCARED);
    }

    if (randint1(plr->chp) < amt && !res_save_default(GF_LIGHT))
    {
        int k = 0;
        cptr act = NULL;

        switch (randint1(12))
        {
        case 1: case 2: case 3: case 4: case 5:
            msg_print("You feel your unlife force diminish.");
            lose_exp(100 + (plr->exp / 100) * MON_DRAIN_LIFE);
            break;

        case 6: case 7: case 8: case 9:
            switch (randint1(6))
            {
                case 1: k = A_STR; act = "strong"; break;
                case 2: k = A_INT; act = "bright"; break;
                case 3: k = A_WIS; act = "wise"; break;
                case 4: k = A_DEX; act = "agile"; break;
                case 5: k = A_CON; act = "hale"; break;
                case 6: k = A_CHR; act = "confident"; break;
            }
            msg_format("You're not as %s as you used to be.", act);
            plr->stat_cur[k] = (plr->stat_cur[k] * 3) / 4;
            if (plr->stat_cur[k] < 3) plr->stat_cur[k] = 3;
            break;

        case 10:
            msg_print("You're not as powerful as you used to be.");
            for (k = 0; k < 6; k++)
            {
                plr->stat_cur[k] = (plr->stat_cur[k] * 7) / 8;
                if (plr->stat_cur[k] < 3) plr->stat_cur[k] = 3;
            }
            break;

        case 11: case 12:
            if (plr_tim_disenchant())
                msg_print("You feel diminished!");
            break;
        }

        plr->update |= PU_BONUS;
    }
}

void vampire_take_dark_damage(int amt)
{
    if (randint1(plr->chp) < amt)
    {
        /* TODO */
    }
}

/****************************************************************
 * Vampire Shapeshifting (Bat, Mist, Wolf, ...)
 *
 * The vampire can maintain their form for as long as they like
 * since equipment juggling is rather tedious. However, they can
 * only feed as a bat or as a vampire, so they will quickly grow
 * hungry for fresh blood!
 ****************************************************************/
static spell_info _mimic_spells[] = 
{
    { 1,  0,  0, _polymorph_undo_spell }, 
    {-1, -1, -1, NULL }
};

static int _mimic_get_spells(spell_info* spells, int max) 
{
    return get_spells_aux(spells, max, _mimic_spells);
}

/****************************************************************
 * Bat
 ****************************************************************/
static void _bat_calc_innate_bonuses(mon_blow_ptr blow)
{
    if (blow->method == RBM_BITE)
    {
        plr->innate_attack_info.blows_calc.wgt = 100;
        plr->innate_attack_info.blows_calc.mul = 20;
        plr->innate_attack_info.blows_calc.max = 400;
        plr_calc_blows_innate(blow);
    }
}
static void _bat_calc_innate_attacks(void) 
{
    int l = plr->lev;
    mon_blow_ptr blow = mon_blow_alloc(RBM_BITE);
    blow->power = l;
    blow->weight = 50;
    mon_blow_push_effect(blow, RBE_VAMP, dice_create(1 + l/12, 4 + l/15, 0));
    _bat_calc_innate_bonuses(blow);
    vec_add(plr->innate_blows, blow);
}
static void _bat_calc_bonuses(void)
{
    plr->levitation = TRUE;
    plr->see_inv++;
    plr->regen += 100;
    res_add(GF_DARK);
    res_add(GF_COLD);
    res_add(GF_POIS);
    plr->see_nocto = DUN_VIEW_MAX;
    plr->pspeed += 5 + plr->lev * 3 / 10;
    plr->hold_life++;
}
static void _bat_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_LEVITATION);
    add_flag(flgs, OF_SEE_INVIS);
    add_flag(flgs, OF_REGEN);
    add_flag(flgs, OF_SPEED);
    add_flag(flgs, OF_RES_(GF_DARK));
    add_flag(flgs, OF_RES_(GF_COLD));
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_HOLD_LIFE);
}
plr_race_ptr bat_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  45,  38,  10,  24,  16,  48,  30 };
    skills_t xs = { 60,  90,  55,   5,   0,   0,  65,  50 };

        me = plr_race_alloc(MIMIC_BAT);
        me->skills = bs;
        me->extra_skills = xs;

        me->name = "Vampire Bat";
        me->desc = "";

        me->stats[A_STR] = -3;
        me->stats[A_INT] =  0;
        me->stats[A_WIS] =  0;
        me->stats[A_DEX] =  4;
        me->stats[A_CON] = -3;
        me->stats[A_CHR] = -3;
        
        me->life = 75;
        me->base_hp = 10;
        me->exp = 75;
        me->infra = 10;
        me->shop_adjust = 120;

        me->hooks.get_spells = _mimic_get_spells;
        me->hooks.calc_innate_attacks = _bat_calc_innate_attacks;
        me->hooks.calc_innate_bonuses = _bat_calc_innate_bonuses;
        me->hooks.calc_bonuses = _bat_calc_bonuses;
        me->hooks.get_flags = _bat_get_flags;
        me->hooks.caster_info = _caster_info;

        me->flags = RACE_IS_NONLIVING | RACE_IS_UNDEAD | RACE_IS_MONSTER;

        me->equip_template = equip_template_parse("Vampire Bat");
    }

    return me;
}

/****************************************************************
 * Mist
 ****************************************************************/
static void _mist_calc_bonuses(void)
{
    plr->levitation = TRUE;
    plr->pass_wall = TRUE;
    plr->no_passwall_dam = TRUE;
    plr->see_inv++;
    plr->see_nocto = DUN_VIEW_MAX;
    plr->hold_life++;

    res_add(GF_ACID);
    res_add(GF_COLD);
    res_add(GF_POIS);
    res_add(GF_NETHER);

    plr->magic_resistance = 50;
}
static void _mist_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_LEVITATION);
    add_flag(flgs, OF_SEE_INVIS);
    add_flag(flgs, OF_HOLD_LIFE);

    add_flag(flgs, OF_RES_(GF_COLD));
    add_flag(flgs, OF_RES_(GF_POIS));
    add_flag(flgs, OF_RES_(GF_ACID));
    add_flag(flgs, OF_RES_(GF_NETHER));

    add_flag(flgs, OF_MAGIC_RESISTANCE);
}
plr_race_ptr mist_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  20,  40,  10,  10,   7,  0,  0};
    skills_t xs = { 30,  35,  50,   5,   0,   0,  0,  0};

        me = plr_race_alloc(MIMIC_MIST);
        me->skills = bs;
        me->extra_skills = xs;

        me->name = "Vampiric Mist";
        me->desc = "You are a cloud of evil, sentient mist. As such you are incorporeal and are "
            "unable to attack enemies directly. Conversely, you are resistant to material damage "
            "and may pass through walls. Probably, you should run away upon assuming this form.";

        me->stats[A_STR] = -3;
        me->stats[A_INT] = -3;
        me->stats[A_WIS] = -3;
        me->stats[A_DEX] = -3;
        me->stats[A_CON] = -3;
        me->stats[A_CHR] = -3;    

        me->life = 80;
        me->base_hp = 15;
        me->exp = 75;
        me->infra = 10;
        me->shop_adjust = 130;

        me->hooks.get_spells = _mimic_get_spells;
        me->hooks.calc_bonuses = _mist_calc_bonuses;
        me->hooks.get_flags = _mist_get_flags;
        me->hooks.caster_info = _caster_info;

        me->flags = RACE_IS_NONLIVING | RACE_IS_UNDEAD | RACE_IS_MONSTER;

        me->equip_template = equip_template_parse("Vampire");
    }

    return me;
}

/****************************************************************
 * Wolf
 ****************************************************************/
static power_info _wolf_powers[] = 
{
    { A_DEX, {  1,  1, 30, hound_sniff_spell } },
    { A_DEX, { 10,  0,  0, hound_stalk_spell}},
    { A_DEX, { 15,  0,  0, hound_run_spell}},
    { A_DEX, { 20, 10, 30, hound_leap_spell}},
    {    -1, { -1, -1, -1, NULL}}
};

static int _wolf_get_powers(spell_info* spells, int max) 
{
    return get_powers_aux(spells, max, _wolf_powers);
}
static void _wolf_calc_bonuses(void)
{
    plr->see_nocto = DUN_VIEW_MAX;
    plr->pspeed += 2 + plr->lev / 10;
}
static void _wolf_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SPEED);
}
plr_race_ptr wolf_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  20,  31,   4,  20,  15,  56,  30};
    skills_t xs = { 40,  40,  50,   5,   0,   0, 100,  35};

        me = plr_race_alloc(MIMIC_WOLF);
        me->skills = bs;
        me->extra_skills = xs;

        me->name = "Dire Wolf";
        me->desc = "";

        me->life = 100;
        me->base_hp = 22;
        me->exp = 120;
        me->infra = 5;
        me->shop_adjust = 115;

        me->hooks.get_spells = _mimic_get_spells;
        me->hooks.get_powers = _wolf_get_powers;
        me->hooks.calc_innate_attacks = hound_calc_innate_attacks;
        me->hooks.calc_innate_bonuses = hound_calc_innate_bonuses;
        me->hooks.calc_bonuses = _wolf_calc_bonuses;
        me->hooks.get_flags = _wolf_get_flags;
        me->hooks.caster_info = _caster_info;

        me->flags = RACE_IS_NONLIVING | RACE_IS_UNDEAD | RACE_IS_MONSTER;

        me->equip_template = equip_template_parse("Dire Wolf");
    }
    me->stats[A_STR] =  1 + plr->lev/12;
    me->stats[A_INT] = -3;
    me->stats[A_WIS] = -5;
    me->stats[A_DEX] =  2 + plr->lev/15;
    me->stats[A_CON] =  1 + plr->lev/15;
    me->stats[A_CHR] =  0 + plr->lev/25;

    return me;
}

