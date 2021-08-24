#include "angband.h"

#include <assert.h>

static bool _unclear_mind = TRUE;

/****************************************************************
 * Timers
 ****************************************************************/
enum { _ESP_MAGICAL = T_CUSTOM,
       _SPELL_REACTION,
       _RESIST_CURSES,
       _ARMOR_OF_FURY,
       _SPELL_TURNING };
/* _ESP_MAGICAL */
static bool _esp_magical_on(plr_tim_ptr timer)
{
    msg_print("You feel conscious of magical foes.");
    plr->update |= PU_BONUS | PU_MONSTERS;
    return TRUE;
}
static void _esp_magical_off(plr_tim_ptr timer)
{
    msg_print("You are no longer conscious of magical foes.");
    plr->update |= PU_BONUS | PU_MONSTERS;
}
static void _esp_magical_calc_bonuses(plr_tim_ptr timer)
{
    plr->esp_magical = TRUE;
}
static status_display_t _esp_magical_display(plr_tim_ptr timer)
{
    return status_display_create("Magic", "Mg", TERM_L_BLUE);
}
static plr_tim_info_ptr _esp_magical(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_ESP_MAGICAL, "Detect Magical Foes");
    info->desc = "You sense nearby magical foes.";
    info->on_f = _esp_magical_on;
    info->off_f = _esp_magical_off;
    info->calc_bonuses_f = _esp_magical_calc_bonuses;
    info->status_display_f = _esp_magical_display;
    return info;
}
/* _SPELL_REACTION */
static bool _spell_reaction_on(plr_tim_ptr timer)
{
    msg_print("You feel ready for magical attacks.");
    return TRUE;
}
static void _spell_reaction_off(plr_tim_ptr timer)
{
    msg_print("You are no longer ready for magical attacks.");
}
static status_display_t _spell_reaction_display(plr_tim_ptr timer)
{
    return status_display_create("Reaction", "Rct", TERM_L_BLUE);
}
static plr_tim_info_ptr _spell_reaction(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_SPELL_REACTION, "Spell Reaction");
    info->desc = "Magical attacks make you faster.";
    info->on_f = _spell_reaction_on;
    info->off_f = _spell_reaction_off;
    info->status_display_f = _spell_reaction_display;
    return info;
}
/* _RESIST_CURSES */
static bool _resist_curses_on(plr_tim_ptr timer)
{
    msg_print("You feel resistant to curses.");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _resist_curses_off(plr_tim_ptr timer)
{
    msg_print("You are no longer resistant to curses.");
    plr->update |= PU_BONUS;
}
static void _resist_curses_calc_bonuses(plr_tim_ptr timer)
{
    plr->skills.sav += 20;
    if (plr_tim_find(T_BERSERK))
        plr->skills.sav += 20;
}
static status_display_t _resist_curses_display(plr_tim_ptr timer)
{
    return status_display_create("Curses", "RC", TERM_YELLOW);
}
static plr_tim_info_ptr _resist_curses(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_RESIST_CURSES, "Resist Curses");
    info->desc = "You have enhanced magic resistance.";
    info->on_f = _resist_curses_on;
    info->off_f = _resist_curses_off;
    info->calc_bonuses_f = _resist_curses_calc_bonuses;
    info->status_display_f = _resist_curses_display;
    return info;
}
/* _ARMOR_OF_FURY */
static bool _armor_of_fury_on(plr_tim_ptr timer)
{
    msg_print("You feel cloaked in rage.");
    return TRUE;
}
static void _armor_of_fury_off(plr_tim_ptr timer)
{
    msg_print("You are no longer cloaked in rage.");
}
static status_display_t _armor_of_fury_display(plr_tim_ptr timer)
{
    return status_display_create("Fury", "Fy", TERM_RED);
}
static plr_tim_info_ptr _armor_of_fury(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_ARMOR_OF_FURY, "Armor of Fury");
    info->desc = "Spellcasting monsters are slowed and stunned by your fury.";
    info->on_f = _armor_of_fury_on;
    info->off_f = _armor_of_fury_off;
    info->status_display_f = _armor_of_fury_display;
    return info;
}
/* _SPELL_TURNING */
static bool _spell_turning_on(plr_tim_ptr timer)
{
    msg_print("You begin to turn magical attacks.");
    return TRUE;
}
static void _spell_turning_off(plr_tim_ptr timer)
{
    msg_print("You are no longer turn magical attacks.");
}
static status_display_t _spell_turning_display(plr_tim_ptr timer)
{
    return status_display_create("Turning", "Tn", TERM_GREEN);
}
static plr_tim_info_ptr _spell_turning(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_SPELL_TURNING, "Spell Turning");
    info->desc = "You turn magical attacks back on the casting monster.";
    info->on_f = _spell_turning_on;
    info->off_f = _spell_turning_off;
    info->status_display_f = _spell_turning_display;
    return info;
}
static void _register_timers(void)
{
    plr_tim_register(_esp_magical());
    plr_tim_register(_spell_reaction());
    plr_tim_register(_resist_curses());
    plr_tim_register(_armor_of_fury());
    plr_tim_register(_spell_turning());
}

/****************************************************************
 * Helpers
 ****************************************************************/
void rage_mage_rage_fueled(int dam)
{
    int x = dam;
    int y = plr->chp;
    int sp = x*(plr->mhp*3/2 - y)/plr->mhp;

    if (plr->pclass != CLASS_RAGE_MAGE) return;

    if (sp < 1)
        sp = 1;

    plr->csp += sp;
    if (plr->csp > plr->msp)
    {
        plr->csp = plr->msp;
        plr->csp_frac = 0;
    }
    plr->redraw |= PR_MANA;

    /*_unclear_mind = FALSE;*/
}
void rage_mage_blood_lust(int dam)
{
    int sp;

    if (plr->pclass != CLASS_RAGE_MAGE) return;

    if (plr_tim_find(T_BERSERK))
        sp = dam/8;
    else
        sp = dam/12;

    if (sp < 1)
        sp = 1;

    plr->csp += sp;
    if (plr->csp > plr->msp)
    {
        plr->csp = plr->msp;
        plr->csp_frac = 0;
    }
    plr->redraw |= PR_MANA;

    _unclear_mind = FALSE;
}
void rage_mage_armor_of_fury(mon_ptr mon, int dam)
{
    char name[MAX_NLEN_MON];

    if (plr->pclass != CLASS_RAGE_MAGE) return;
    if (!plr_tim_find(_ARMOR_OF_FURY)) return;

    assert(mon);
    monster_desc(name, mon, 0);
    msg_format("%^s is hit by your fury!", name);

    if ( mon_save_p(mon, A_STR)
      && (!plr_tim_find(T_BERSERK) || mon_save_p(mon, A_STR)) )
    {
        msg_format("%^s resists!", name);
    }
    else
    {
        int dur = 1;
        if (plr_tim_find(T_BERSERK)) dur = 3;
        mon_tim_add(mon, T_SLOW, dur);
        mon_stun(mon, mon_stun_amount(dur*dam));
    }
}
void rage_mage_spell_reaction(mon_ptr mon)
{
    if (plr->pclass != CLASS_RAGE_MAGE) return;
    if (!plr_tim_find(_SPELL_REACTION)) return;
    plr_tim_augment(T_FAST, 4);
}
bool rage_mage_spell_turning(mon_ptr mon)
{
    bool turn = FALSE;

    if (plr->pclass != CLASS_RAGE_MAGE) return FALSE;
    if (!plr_tim_find(_SPELL_TURNING)) return FALSE;

    if (plr_tim_find(T_BERSERK))
        turn = randint1(100) <= plr->lev;
    else
        turn = randint1(200) <= 20 + plr->lev;

    if (turn)
    {
        msg_print("You turn the magic on the caster!");
        disturb(1, 0);
        return TRUE;
    }

    return FALSE;
}

/****************************************************************
 * Spells
 ****************************************************************/
static void _anti_magic_ray_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Antimagic Ray");
        break;
    case SPELL_DESC:
        var_set_string(res, "Block spells from a chosen foe.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_cast_direct(GF_ANTIMAGIC, dice_create(0,0,0)));
        break;
    default:
        default_spell(cmd, res);
    }
}

static void _armor_of_fury_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Armor of Fury");
        break;
    case SPELL_DESC:
        var_set_string(res, "Whenever a monster attacks you with magic, they may become slowed and stunned.");
        break;
    case SPELL_CAST:
        plr_tim_add(_ARMOR_OF_FURY, 25 + _1d(25));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}

static void _barbarian_lore_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Barbarian Lore");
        break;
    default:
        identify_spell(cmd, res);
    }
}

static void _barbaric_resistance_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Barbaric Resistance");
        break;
    case SPELL_DESC:
        var_set_string(res, "Grants temporary protection from the elements.");
        break;
    case SPELL_CAST:
    {
        int base = plr_tim_find(T_BERSERK) ? 20 : 10;
        dice_t dice = dice_create(1, base, base);

        plr_tim_add(T_RES_ACID, dice_roll(dice));
        plr_tim_add(T_RES_ELEC, dice_roll(dice));
        plr_tim_add(T_RES_FIRE, dice_roll(dice));
        plr_tim_add(T_RES_COLD, dice_roll(dice));
        plr_tim_add(T_RES_POIS, dice_roll(dice));

        var_set_bool(res, TRUE);
    }
    default:
        default_spell(cmd, res);
    }
}

static void _crude_mapping_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Crude Mapping");
        break;
    case SPELL_DESC:
        var_set_string(res, "Maps the dungeon in your vicinity.");
        break;
    case SPELL_CAST:
        map_area(DETECT_RAD_DEFAULT); /* Was 14, but that was just plain annoying! */
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}

static bool _detect = FALSE;
static void _detect_obj(point_t pos, obj_ptr obj)
{
    int rng = DETECT_RAD_ALL;
    if (plr_distance(pos) > rng) return;
    if (obj_is_art(obj) || obj_is_ego(obj))
    {
        obj->marked |= OM_FOUND;
        plr->window |= PW_OBJECT_LIST;
        draw_pos(pos);
        _detect = TRUE;
    }
}
static void _detect_pile(point_t pos, obj_ptr pile)
{
    obj_ptr obj;
    for (obj = pile; obj; obj = obj->next)
        _detect_obj(pos, obj);
}
static void _detect_magic_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Magic");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects nearby magic users and items.");
        break;
    case SPELL_CAST:
        detect_monsters_magical(DETECT_RAD_DEFAULT);
        _detect = FALSE;
        dun_iter_floor_obj(cave, _detect_pile);
        if (_detect) msg_print("You sense the presence of magic objects!");
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}

static void _detect_magical_foes_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Magical Foes");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects nearby magic users.");
        break;
    case SPELL_CAST:
        detect_monsters_magical(DETECT_RAD_DEFAULT);
        if (plr_tim_find(T_BERSERK))
            plr_tim_add(_ESP_MAGICAL, 20 + _1d(20));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}

static void _evasive_leap_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Evasive Leap");
        break;
    case SPELL_ENERGY:
        if (plr_tim_find(T_BERSERK))
        {
            var_set_int(res, 30);
            break;
        }
    default:
        strafing_spell(cmd, res);
        break;
    }
}

static void _focus_rage_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Focus Rage");
        break;
    case SPELL_DESC:
        var_set_string(res, "Damage yourself and regain spell points.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, 10 + plr->lev/2));
        break;
    case SPELL_FAIL:
    {
        int hp = 10 + plr->lev/2;
        take_hit(DAMAGE_NOESCAPE, hp, "Rage");
        break;
    }
    case SPELL_CAST:
    {
        int hp = 10 + plr->lev/2;

        var_set_bool(res, FALSE);

        if (plr->chp < hp)
        {
            if (!get_check("Really? This will kill you!")) return;
        }

        take_hit(DAMAGE_NOESCAPE, hp, "Rage");
        sp_player(hp);

        _unclear_mind = FALSE; /* Hack to avoid automatic mana drain for this action */
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _force_brand_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Force Brand");
        break;
    case SPELL_DESC:
        var_set_string(res, "Temporarily brands your weapon with force.");
        break;
    case SPELL_CAST:
    {
        int base = 4;
        if (plr_tim_find(T_BERSERK))
            base = 10;
        plr_tim_add(T_BRAND_MANA, base + _1d(base));
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _greater_focus_rage_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Focus *Rage*");
        break;
    case SPELL_DESC:
        var_set_string(res, "Damage yourself and regain spell points.");
        break;
    case SPELL_INFO:
        if (plr_tim_find(T_BERSERK))
            var_set_string(res, info_damage(0, 0, 2 * plr->lev));
        else
            var_set_string(res, info_damage(0, 0, 10 + plr->lev));
        break;
    case SPELL_FAIL:
    {
        int hp = 10 + plr->lev;
        if (plr_tim_find(T_BERSERK))
            hp = 2 * plr->lev;
        take_hit(DAMAGE_NOESCAPE, hp, "Rage");
        break;
    }
    case SPELL_CAST:
    {
        int hp = 10 + plr->lev;

        var_set_bool(res, FALSE);

        if (plr_tim_find(T_BERSERK))
            hp = 2 * plr->lev;

        if (plr->chp < hp)
        {
            if (!get_check("Really? This will kill you!")) return;
        }

        take_hit(DAMAGE_NOESCAPE, hp, "Rage");
        sp_player(hp * 2);

        _unclear_mind = FALSE; /* Hack to avoid automatic mana drain for this action */
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static int _greater_shout_dam(void)
{
    int dam = 50 + plr_prorata_level(130);
    if (plr_tim_find(T_BERSERK))
        dam = dam * 4 / 3;
    return dam;
}
static void _greater_shout_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Greater Shout");
        break;
    case SPELL_DESC:
        var_set_string(res, "Projects a cone of sound at a chosen foe.");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, _greater_shout_dam()/10);
        break;
    default:
        breath_spell_innate(cmd, res, 3, GF_SOUND, _greater_shout_dam());
    }
}

static void _mana_clash_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mana Clash");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a ball at chosen target. Only spellcasters will be damaged.");
        break;
    default: /* dam = dam * spell_freq / 100 in gf.c */
        ball_spell(cmd, res, 2, GF_MANA_CLASH, 18*plr->lev);
    }
}

static int _rage_strike_dam(void)
{
    int sp = plr->csp;
    int z = sp*sp/100;
    return 1200*z/(1000+z);
}
static void _rage_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Ragestrike");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fire a ball of pure rage at chosen foe, striking with everything you've got!");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _rage_strike_dam()));
        break;
    case SPELL_FAIL:
        sp_player(-plr->csp);
        break;
    case SPELL_CAST:
    {
        dice_t dice = dice_create(0, 0, _rage_strike_dam());
        var_set_bool(res, FALSE);

        if (plr->chp < 100)
        {
            if (!get_check("Really? This will kill you!")) return;
        }

        if (plr_cast_ball(0, GF_MISSILE, dice))
        {
            take_hit(DAMAGE_NOESCAPE, 100, "Rage");
            if (!plr_tim_find(T_BERSERK))
                plr_tim_add(T_STUN, STUN_KNOCKED_OUT - 1); /* XXX bypass resistance */

            sp_player(-plr->csp); /* Don't use SPELL_COST_EXTRA since we pay mana up front these days! */
            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
    }
}

static void _rage_sustenance_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Rage Sustenance");
        break;
    default:
        satisfy_hunger_spell(cmd, res);
    }
}

static void _resist_curses_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Resist Curses");
        break;
    case SPELL_DESC:
        var_set_string(res, "Grants temporary magical resistance.");
        break;
    case SPELL_CAST:
        plr_tim_add(_RESIST_CURSES, 20 + _1d(20));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}

static void _resist_disenchantment_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Resist Disenchantment");
        break;
    case SPELL_DESC:
        var_set_string(res, "Grants temporary resistance to disenchantment.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_RES_DISEN, 10 + _1d(10));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}

static int _object_dam_type(object_type *o_ptr)
{
    switch (o_ptr->activation.type)
    {
    case EFFECT_BEAM_ACID:
    case EFFECT_BALL_ACID:
    case EFFECT_BOLT_ACID:
        return GF_ACID;

    case EFFECT_BEAM_ELEC:
    case EFFECT_BALL_ELEC:
    case EFFECT_BOLT_ELEC:
        return GF_ELEC;

    case EFFECT_BEAM_FIRE:
    case EFFECT_BREATHE_FIRE:
    case EFFECT_BOLT_PLASMA:
    case EFFECT_BALL_FIRE:
    case EFFECT_BOLT_FIRE:
        return GF_FIRE;

    case EFFECT_BEAM_COLD:
    case EFFECT_BREATHE_COLD:
    case EFFECT_BOLT_ICE:
    case EFFECT_BALL_COLD:
    case EFFECT_BOLT_COLD:
        return GF_COLD;

    case EFFECT_BALL_POIS:
        return GF_POIS;

    case EFFECT_BREATHE_ONE_MULTIHUED:
    {
        switch (randint1(5))
        {
        case 1: return GF_ACID;
        case 2: return GF_ELEC;
        case 3: return GF_FIRE;
        case 4: return GF_COLD;
        case 5: return GF_POIS;
        }
    }

    case EFFECT_CONFUSE_MONSTERS:
    case EFFECT_CONFUSING_LIGHT:
        return GF_CONFUSION;

    case EFFECT_STARBURST:
    case EFFECT_STARLIGHT:
    case EFFECT_BALL_LIGHT:
    case EFFECT_BEAM_LIGHT:
    case EFFECT_LIGHT_AREA:
    case EFFECT_BEAM_LIGHT_WEAK:
        return GF_LIGHT;

    case EFFECT_DARKNESS:
    case EFFECT_DARKNESS_STORM:
        return GF_DARK;

    case EFFECT_BALL_NETHER:
        return GF_NETHER;

    case EFFECT_BALL_NEXUS:
        return GF_NEXUS;

    case EFFECT_BALL_SOUND:
    case EFFECT_BEAM_SOUND:
        return GF_SOUND;

    case EFFECT_BALL_SHARDS:
        return GF_SHARDS;

    case EFFECT_BALL_CHAOS:
    case EFFECT_BEAM_CHAOS:
        return GF_CHAOS;

    case EFFECT_BALL_DISEN:
        return GF_DISENCHANT;

    case EFFECT_BEAM_GRAVITY:
        return GF_GRAVITY;

    case EFFECT_BEAM_DISINTEGRATE:
    case EFFECT_BALL_DISINTEGRATE:
        return GF_DISINTEGRATE;

    case EFFECT_ROCKET:
        return GF_ROCKET;

    case EFFECT_SPEED:
    case EFFECT_SLOWNESS:
    case EFFECT_HASTE_MONSTERS:
    case EFFECT_SLOW_MONSTERS:
        return GF_INERTIA;

    case EFFECT_HOLINESS:
        return GF_HOLY_FIRE;
    }

    return GF_MANA;
}

static void _shatter_device_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shatter Device");
        break;
    case SPELL_DESC:
        var_set_string(res, "Destroy a magical device in your inventory for various effects.");
        break;
    case SPELL_CAST:
    {
        obj_prompt_t prompt = {0};

        var_set_bool(res, FALSE);

        prompt.prompt = "Shatter which device?";
        prompt.error = "You have nothing to shatter.";
        prompt.filter = obj_is_device;
        prompt.where[0] = INV_PACK;
        prompt.where[1] = INV_FLOOR;

        obj_prompt(&prompt);
        if (!prompt.obj) return;

        var_set_bool(res, TRUE);

        if (prompt.obj->activation.type == EFFECT_NONE)
        {
            msg_print("Nothing happens.");
        }
        else if (prompt.obj->activation.type == EFFECT_DESTRUCTION)
        {
            if (destroy_area(plr->pos, 15 + plr->lev + randint0(11), 4 * plr->lev))
                msg_print("The dungeon collapses...");
            else
                msg_print("The dungeon trembles.");
        }
        else if ( prompt.obj->activation.type == EFFECT_HEAL_CURING
               || prompt.obj->activation.type == EFFECT_HEAL_CURING_HERO
               || prompt.obj->activation.type == EFFECT_RESTORING )
        {
            msg_print("You feel life flow through your body!");
            restore_level();
            plr_restore_life(1000);
            plr_tim_remove(T_POISON);
            plr_tim_remove(T_BLIND);
            plr_tim_remove(T_CONFUSED);
            plr_tim_remove(T_HALLUCINATE);
            plr_tim_remove(T_STUN);
            plr_tim_remove(T_CUT);
            do_res_stat(A_STR);
            do_res_stat(A_CON);
            do_res_stat(A_DEX);
            do_res_stat(A_WIS);
            do_res_stat(A_INT);
            do_res_stat(A_CHR);
            update_stuff(); /* hp may change if Con was drained ... */
            hp_player(5000);
        }
        else if ( prompt.obj->activation.type == EFFECT_TELEPORT_AWAY
               || prompt.obj->activation.type == EFFECT_BANISH_EVIL
               || prompt.obj->activation.type == EFFECT_BANISH_ALL )
        {
            plr_project_los(GF_TELEPORT, plr->lev * 4);
        }
        else
        {
            int gf = _object_dam_type(prompt.obj);
            int dam = 8*prompt.obj->activation.difficulty;
            plr_burst(5, gf, dam);
        }
        prompt.obj->number--;
        obj_release(prompt.obj, 0);
        break;
    }
    default:
        default_spell(cmd, res);
    }
}

static dice_t _shout_dice(void) { return dice_create(3 + (plr->lev - 1)/5, 4, 0); }
static void _shout_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shout");
        break;
    case SPELL_DESC:
        var_set_string(res, "Projects a cone of sound at a chosen foe.");
        break;
    default:
        breath_spell_aux(cmd, res, 2, GF_SOUND, _shout_dice());
    }
}

static void _smash_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Smash");
        break;
    case SPELL_DESC:
        var_set_string(res, "Destroys adjacent door, trap or wall.");
        break;
    case SPELL_CAST:
    {
        int dir;
        point_t pos;

        var_set_bool(res, FALSE);
        if (!get_rep_dir2(&dir)) return;
        if (dir == 5) return;

        pos = point_step(plr->pos, dir);
        if (!dun_pos_interior(cave, pos)) return;

        if (dun_tunnel(cave, pos, ACTION_FORCE | ACTION_QUIET) != ACTION_SUCCESS)
        {
            gf_affect_o(who_create_plr(), pos, GF_KILL_DOOR, 0, GF_AFFECT_SPELL); /* disarm chests */
            gf_affect_f(who_create_plr(), pos, GF_KILL_DOOR, 0, GF_AFFECT_SPELL);
        }
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
    }
}

static void _spell_reaction_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Spell Reaction");
        break;
    case SPELL_DESC:
        var_set_string(res, "Grants temporary speed whenever you are targetted by a magical attack.");
        break;
    case SPELL_CAST:
        plr_tim_add(_SPELL_REACTION, 30 + _1d(30));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}

static void _spell_turning_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Spell Turning");
        break;
    case SPELL_DESC:
        var_set_string(res, "Whenever you are the target of magic there is a chance of returning the spell to the caster.");
        break;
    case SPELL_CAST:
        plr_tim_add(_SPELL_TURNING, 20 + _1d(20));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}

static void _summon_commando_team_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Commando Team");
        break;
    case SPELL_DESC:
        var_set_string(res, "Summons Grand Master Mystics for assistance.");
        break;
    case SPELL_CAST:
    {
        int num = 1 + randint1(2);
        int mode = PM_FORCE_PET;
        int i;
        point_t pos;

        var_set_bool(res, FALSE);

        if (plr_tim_find(T_BERSERK))
            mode |= PM_HASTE;

        if (!target_set(TARGET_KILL)) return;
        pos = who_pos(plr->target);

        for (i = 0; i < num; i++)
            summon_named_creature(who_create_plr(), pos, mon_race_parse("p.grand master mystic"), mode);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
    }
}

static void _summon_horde_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Horde");
        break;
    case SPELL_DESC:
        var_set_string(res, "Summons Warriors of the Dawn for assistance.");
        break;
    case SPELL_CAST:
    {
        int num = 3 + randint1(3);
        int mode = PM_FORCE_PET;
        int i;

        if (plr_tim_find(T_BERSERK))
            mode |= PM_HASTE;

        for (i = 0; i < num; i++)
            summon_named_creature(who_create_plr(), plr->pos, mon_race_parse("p.dawn"), mode);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
    }
}

static void _veterans_blessing_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Veteran's Blessing");
        break;
    default:
        heroism_spell(cmd, res);
    }
}

static void _whirlwind_attack_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Whirlwind Attack");
        break;
    default:
        massacre_spell(cmd, res);
    }
}

/****************************************************************
 * The Rage Realm and Spellcasting
 ****************************************************************/
/* The Rage Mage uses spellbooks to learn spells
   like other magic classes. However, learning a
   spell destroys the book, and casting a spell
   does not require the book (cf The Samurai).
   Rage is a class specific realm.
*/
#define _SPELLS_PER_BOOK 8

typedef struct {
    cptr name;
    spell_info spells[_SPELLS_PER_BOOK];
} book_t;

static book_t _books[4] = {
    { "Anger Management",
        {{ 1,  2, 30, _shout_spell},
         { 2,  2, 25, _detect_magical_foes_spell},
         { 3,  3, 30, _smash_spell},
         { 5,  5, 25, _evasive_leap_spell},
         { 5,  5, 35, light_area_spell},
         { 7,  0, 50, _focus_rage_spell},
         { 8, 10, 50, _rage_sustenance_spell},
         {12,  6, 35, _veterans_blessing_spell}}
    },
    { "Northern Frights",
        {{15,  8, 45, _crude_mapping_spell},
         {18, 18, 50, _resist_disenchantment_spell},
         {20, 30, 55, awesome_blow_spell},
         {22, 15, 60, _spell_reaction_spell},
         {23,  5, 60, _greater_shout_spell},
         {25, 18, 60, _whirlwind_attack_spell},
         {27, 20, 55, _resist_curses_spell},
         {28, 23, 70, _detect_magic_spell}}
    },
    { "The Sound and the Fury",
        {{10, 12, 35, berserk_spell},
         {25, 16, 60, sterility_spell},
         {26, 20, 80, _barbaric_resistance_spell},
         {28, 22, 55, _summon_horde_spell},
         {32, 28, 75, _armor_of_fury_spell},
         {35, 55, 70, _force_brand_spell},
         {38, 30, 50, dispel_magic_spell},
         {40, 60, 85, _mana_clash_spell}}
    },
    { "Dire Ire",
        {{30, 25, 75, _barbarian_lore_spell},
         {32, 15, 65, earthquake_spell},
         {35,  0, 90, _greater_focus_rage_spell},
         {38, 60, 95, _spell_turning_spell},
         {40, 55, 80, _shatter_device_spell},
         {42, 40, 50, _summon_commando_team_spell},
         {43, 70, 80, _anti_magic_ray_spell},
         {47,  0, 80, _rage_strike_spell}}
    },
};

static int _spell_index(int book, int spell)
{
    return book * _SPELLS_PER_BOOK + spell;
}

static bool _is_spell_known(int book, int spell)
{
    int idx = _spell_index(book, spell);
    if (plr->spell_learned1 & (1L << idx)) return TRUE;
    return FALSE;
}

static void _learn_spell(int book, int spell)
{
    int idx = _spell_index(book, spell);
    int i;

    plr->spell_learned1 |= (1L << idx);

    /* Find the next open entry in "plr->spell_order[]" */
    for (i = 0; i < 64; i++)
    {
        /* Stop at the first empty space */
        if (plr->spell_order[i] == 99) break;
    }

    /* Add the spell to the known list */
    plr->spell_order[i++] = spell;
    plr->learned_spells++;
    plr->update |= PU_SPELLS;
    plr->redraw |= PR_EFFECTS;

    msg_format("You have learned the technique of %s.", get_spell_name(_books[book].spells[spell].fn));
}

static bool _gain_spell(int book)
{
    spell_info spells[_SPELLS_PER_BOOK];
    int        indices[_SPELLS_PER_BOOK];
    int        which;
    int        ct = 0, i;

    /* Build a list of learnable spells. Spells can only be
       learned once (no spell skills) and we only display spells
       if the user is of high enough level. This is rather
       different than how the system normally behaves, but why spoil
       the nature of future higher level spells to the player?
    */
    for (i = 0; i < _SPELLS_PER_BOOK; i++)
    {
        spell_info *src = &_books[book].spells[i];

        if (!_is_spell_known(book, i) && src->level <= plr->lev)
        {
            spell_info *dest = &spells[ct];

            dest->level = src->level;
            dest->cost = src->cost;
            dest->fail = calculate_fail_rate(
                src->level,
                src->fail,
                plr->stat_ind[A_STR]
            );
            dest->fn = src->fn;
            indices[ct] = i;

            ct++;
        }
    }

    if (ct == 0)
    {
        msg_print("You may not learn any spells in that book.");
        return FALSE;
    }

    which = choose_spell(spells, ct, "rage", 1000);
    if (which >= 0 && which < ct)
    {
        _learn_spell(book, indices[which]);
        return TRUE;
    }

    return FALSE;
}

static bool _is_rage_book(obj_ptr obj) { return obj->tval == TV_RAGE_BOOK; }

void rage_mage_gain_spell(void)
{
    obj_prompt_t prompt = {0};

    if (plr_tim_find(T_BLIND) || no_light())
    {
        msg_print("You cannot see!");
        return;
    }
    if (plr_tim_find(T_CONFUSED))
    {
        msg_print("You are too confused!");
        return;
    }
    if (!plr->new_spells)
    {
        msg_print("You cannot learn any new techniques!");
        return;
    }

    prompt.prompt = "Study which book?";
    prompt.error = "You have no books that you can read.";
    prompt.filter = _is_rage_book;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return;

    if (_gain_spell(prompt.obj->sval))
    {
        char o_name[MAX_NLEN];

        object_desc(o_name, prompt.obj, OD_COLOR_CODED | OD_SINGULAR);

        msg_format("%^s is destroyed.", o_name);
        prompt.obj->number--;
        obj_release(prompt.obj, 0);

        energy_use = 100;
    }
}

void rage_mage_browse_spell(void)
{
    /* TODO: Perhaps browse should display contents of rage
       spellbooks in inventory rather than already known spells? */
    do_cmd_spell_browse();
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "rage";
        me.encumbrance.max_wgt = 1000;
        me.encumbrance.weapon_pct = 20;
        me.encumbrance.enc_wgt = 1200;
        me.realm1_choices = CH_RAGE;
        init = TRUE;
    }
    return &me;
}

/****************************************************************
 * Hooks
 ****************************************************************/
static void _player_action(void)
{
    /* Unclear Mind */
    if (_unclear_mind)    /* Hack for Focus Rage spell to bypass sp loss for one action */
    {
        int loss;
        loss = plr->csp/8 + plr->lev/10 + 1;
        loss = loss * energy_use / 100; /* Prorata normal action energy */

        plr->csp -= loss;
        if (plr->csp < 0)
        {
            plr->csp = 0;
            plr->csp_frac = 0;
        }
        plr->redraw |= PR_MANA;
    }
    else
        _unclear_mind = TRUE; /* Resume normal sp loss */
}

static void _calc_bonuses(void)
{
    int squish = 5 + plr_prorata_level(55);
    plr->spell_cap += 3;

    /* Squishy */
    plr->to_a -= squish;
    plr->dis_to_a -= squish;

}

static int _get_spells_imp(spell_info* spells, int max, int book)
{
    int ct = 0, i;
    for (i = 0; i < _SPELLS_PER_BOOK; i++)
    {
        spell_info *src, *dest;

        if (ct >= max) break;
        src = &_books[book].spells[i];

        if (_is_spell_known(book, i))
        {
            dest = &spells[ct++];
            dest->level = src->level;
            dest->cost = src->cost;
            dest->fail = calculate_fail_rate(
                src->level,
                src->fail,
                plr->stat_ind[A_STR]
            );
            dest->fn = src->fn;
        }
    }
    return ct;
}

static void _book_menu_fn(int cmd, int which, vptr cookie, var_ptr res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, _books[which].name);
        break;
    default:
        default_menu(cmd, which, cookie, res);
    }
}

static int _get_spells(spell_info* spells, int max)
{
    int idx = -1;
    int ct = 0;
    menu_t menu = { "Use which group?", NULL, NULL,
                    _book_menu_fn, _books, 4 };

    idx = menu_choose(&menu);
    if (idx < 0) return 0;

    ct = _get_spells_imp(spells, max, idx);
    if (ct == 0)
        msg_print("You don't know any of those techniques yet!");
    return ct;
}

static void _character_dump(doc_ptr doc)
{
    spell_info spells[MAX_SPELLS];
    int        ct = 0, i;

    for (i = 0; i < 4; i++)
        ct += _get_spells_imp(spells + ct, MAX_SPELLS - ct, i);

    plr_display_spells(doc, spells, ct);
}


static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_BROAD_SWORD, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_spellbooks();
}

plr_class_ptr rage_mage_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  20,  40,  -1,  12,   2,  50,  30 };
    skills_t xs = { 35,  40,  75,   0,   0,   0,  75,  75 };

        me = plr_class_alloc(CLASS_RAGE_MAGE);
        me->name = "Rage-Mage";
        me->desc = "The Rage Mage is part of a secret sect descending from the Barbarians "
                    "in response to their natural foes, the mages. As time passed, other "
                    "races have also begun to learn their arts. The powers of the Rage Mage "
                    "are spells learned from books, but they don't work the way normal spells do. "
                    "First of all, the Rage Mage must perform a special Ritual of Anger to "
                    "learn a spell, and this ritual destroys the spell book in the process. As a "
                    "result, it may take a long time for the Rage Mage to learn all of their "
                    "high level powers. Once learned, the Rage Mage no longers requires the spell "
                    "book in order to perform the power.\n \n"
                    "Another unique aspect of the Rage Mage concerns their Mana pool. Unlike "
                    "normal spellcasters, the Rage Mage's mana does not regenerate on its own. "
                    "In fact, their mana actually decreases rapidly each turn, meaning that they "
                    "had better use their powers quickly while they still can. The Rage Mage gains "
                    "mana whenever he is the target of a magical spell. Indeed, magic makes the "
                    "Rage Mage very angry! The Rage Mage can also fuel their mana by hurting "
                    "those around them. This can be quite effective in crowded situations.";
        me->stats[A_STR] =  3;
        me->stats[A_INT] = -2;
        me->stats[A_WIS] = -2;
        me->stats[A_DEX] = -2;
        me->stats[A_CON] =  2;
        me->stats[A_CHR] =  1;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 106;
        me->base_hp = 6;
        me->exp = 150;
        me->pets = 40;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_STRONG;

        me->hooks.birth = _birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_spells = _get_spells;
        me->hooks.caster_info = _caster_info;
        me->hooks.player_action = _player_action;
        me->hooks.character_dump = _character_dump;
        me->hooks.register_timers = _register_timers;
    }

    return me;
}
