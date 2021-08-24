#include "angband.h"

#include <assert.h>

/************************************************************************
 * Utilities
 ************************************************************************/
static point_t _illusion_pos;
static bool _auditory_tim_p(mon_tim_ptr t)
{
    if (t->id == T_PARALYZED) return TRUE; /* XXX debatable: e.g. muscular paralysis still leaves the victim conscious and aware! */
    if (t->id == T_INVULN) return TRUE; /* XXX debatable */
    if (t->id == T_STUN && randint0(100) < t->count) return TRUE; /* too dazed to notice */
    return FALSE;
}
static bool _visual_tim_p(mon_tim_ptr t)
{
    if (t->id == MT_SLEEP) return TRUE;
    if (t->id == T_BLIND) return TRUE;
    return _auditory_tim_p(t);
}
static bool _mon_p(mon_ptr mon)
{
    if (mon->mflag2 & MFLAG2_ILLUSION) return FALSE; /* pets are affected unless themselves illusory */

    if (mon_immune_illusion(mon)) return FALSE;
    if (mon_has_empty_mind(mon)) return FALSE;
    if (mon_has_weird_mind(mon) && one_in_(2)) return FALSE;

    return TRUE;
}
static bool _auditory_p(mon_ptr mon)
{
    if (mon_tim_find_p(mon, _auditory_tim_p)) return FALSE;
    if (!_mon_p(mon)) return FALSE;
    if (!mon_project(mon, _illusion_pos)) return FALSE; /* walls dampen sound */
    return TRUE;
}
static bool _visual_p(mon_ptr mon)
{
    if (mon_tim_find_p(mon, _visual_tim_p)) return FALSE;
    if (!_mon_p(mon)) return FALSE;
    if (!mon_project(mon, _illusion_pos)) return FALSE;
    return TRUE;
}
/* flashes are visual illusions affecting a single monster
 * projection to illusion source is handled by plr_target_mon() */
static bool _flash_p(mon_ptr mon)
{
    if (mon_tim_find_p(mon, _visual_tim_p)) return FALSE;
    if (!_mon_p(mon)) return FALSE;
    return TRUE;
}
static void _flash(mon_ptr mon, int gf, int power)
{
    if (!_flash_p(mon))
    {
        char name[MAX_NLEN_MON];
        monster_desc(name, mon, 0);
        msg_format("%^s did not notice your illusion.", name);
        return;
    }
    gf_affect_m(who_create_plr(), mon, gf, power, GF_AFFECT_SPELL); 
}
/* projected illusions affect monsters with "los" to the source of the illusion */
static vec_ptr _monsters(mon_p p, point_t pos)
{
    _illusion_pos = pos;
    return dun_filter_mon(cave, p);
}
static bool _project_aux(int gf, int dam, mon_p p, point_t pos)
{
    vec_ptr v = _monsters(p, pos);
    bool    result = FALSE;
    int     i;

    for (i = 0; i < vec_length(v); i++)
    {
        mon_ptr mon = vec_get(v, i);
        if (!mon_is_valid(mon)) continue; /* paranoia: an illusion never really harmed anybody! */
        if (gf_affect_m(who_create_plr(), mon, gf, dam, GF_AFFECT_SPELL)) result = TRUE;
    }

    vec_free(v);
    return result;
}
static bool _project_visual(int gf, int dam)
{
    return _project_aux(gf, dam, _visual_p, plr->pos);
}
static void _project_flash_bang(int power)
{
    vec_ptr v = _monsters(_auditory_p, plr->pos);
    int     i;

    for (i = 0; i < vec_length(v); i++)
    {
        mon_ptr mon = vec_get(v, i);
        if (!mon_is_valid(mon)) continue; /* paranoia: an illusion never really harmed anybody! */
        mon_tim_delete(mon, MT_SLEEP); /* can't sleep thru this one! */
        gf_affect_m(who_create_plr(), mon, GF_STUN, power, GF_AFFECT_SPELL);
        gf_affect_m(who_create_plr(), mon, GF_BLIND, power, GF_AFFECT_SPELL);
    }

    vec_free(v);
}
/* while not truly an auditory illusion, discord will wake up sleeping monsters,
 * and "aggravate" blind ones. no need for GF_DISCORD just for us. */
static bool _project_discord(void)
{
    vec_ptr v = _monsters(_auditory_p, plr->pos);
    bool    result = FALSE;
    int     i;

    for (i = 0; i < vec_length(v); i++)
    {
        mon_ptr mon = vec_get(v, i);
        mon_race_ptr race = mon->race;

        mon_tim_delete(mon, MT_SLEEP);

        if (mon->mflag2 & MFLAG2_QUESTOR) continue;
        if (mon_race_is_unique(race) && randint0(100) < race->alloc.lvl) continue;
        mon_tim_add(mon, MT_DISCORD, 2);
        result = TRUE;
    }
    vec_free(v);
    return result;
}
static bool _project_command(void)
{
    vec_ptr v = _monsters(_visual_p, plr->pos);
    bool    result = FALSE;
    int     i;

    for (i = 0; i < vec_length(v); i++)
    {
        mon_ptr mon = vec_get(v, i);
        mon_race_ptr race = mon->race;

        if (mon_is_pet(mon)) continue; /* don't make permanent pets temporary */
        if (mon->mflag2 & MFLAG2_QUESTOR) continue;
        if (mon_race_is_unique(race) && randint0(70) < race->alloc.lvl) continue;
        if (!result)
            msg_print("You project an aura of command!");
        if (mon_show_msg(mon)) /* before set_pet: "The Foo submits ..." vs "Your Foo submits ..." */
        {
            char name[MAX_NLEN_MON];
            monster_desc(name, mon, 0);
            msg_format("%^s submits to your commanding presence!", name);
        }
        set_temp_pet(mon);
        result = TRUE;
    }
    vec_free(v);
    return result;
}
/* positional diversion ... monsters wander over to investigate, sniff about
 * a few turns, and then give up to re-pursue the plr */
static void _divert_monsters(point_t pos)
{ 
    vec_ptr v = _monsters(_visual_p, pos);
    int     i;

    for (i = 0; i < vec_length(v); i++)
    {
        mon_ptr mon = vec_get(v, i);
        if (mon_is_pet(mon)) continue; /* pets normally affected by illusions are not diverted by them */
        mon->target_id = 0; 
        mon->target_pos = pos;
    }
    vec_free(v);
}
/* monster (illusion) diversion ... monsters lock on to the illusion for mon vs mon fighting */
static void _decoy_monsters(mon_ptr decoy)
{ 
    vec_ptr v = _monsters(_visual_p, decoy->pos);
    int     i;

    for (i = 0; i < vec_length(v); i++)
    {
        mon_ptr mon = vec_get(v, i);
        if (mon_is_pet(mon)) continue; /* pets normally affected by illusions are not decoyed by them */
        mon->target_id = decoy->id; 
        mon->target_pos.x = 0;
        mon->target_pos.y = 0;
    }
    vec_free(v);
}
/* Monster Illusions are currently just normal monsters with MFLAG2_ILLUSION.
 * This approach requires some special checks in the system (so that ai like
 * fear or confusion has no effect). An alternative would be a "tanuki" kind
 * of implementation, but that makes it harder to set the ap_r_idx correctly.
 * With the MFLAG2_ILLUSION approach we can leverage all the SUMMON_FOO constants.
 * Also, we can support special plr illusions using MON_PLAYER (=0), whereas an
 * ap_r_idx of 0 implies r_idx should be used for appearance. MON_PLAYER illusions
 * can give "a better Bunshin", if you know what I mean. */
static int _summon_lvl(void)
{
    int  lvl = spell_power(plr->lev);
    lvl += _1d(lvl);
    lvl += 25;
    return lvl;
}
static u32b _summon_mode(void) { return PM_ILLUSION | PM_FORCE_PET; }
static void _summon_illusion(int what, int count)
{
    int  i;
    for (i = 0; i < count; i++)
        summon_specific(who_create_plr(), plr->pos, _summon_lvl(), what, _summon_mode());
}
static mon_ptr _summon_decoy(point_t pos)
{
    return summon_named_creature(who_create_plr(), pos, mon_race_parse("@.player"), _summon_mode());
}

/************************************************************************
 * Custom Timers
 ************************************************************************/
/* Cloaks: Only one cloak may be active at a time. Cloaks are weavings
 * of light and shadow. They may protect the plr by obscuring him from 
 * sight (invisibility, +AC or +Stealth). They may also provide protection
 * from Light, Darkness, Confusion and Blindness. */
static int _cloaks[] = {T_CLOAK_PROTECTION, T_CLOAK_SHADOWS, T_CLOAK_INNOCENCE, T_CLOAK_INVIS, T_NONE};
static void _cloak_on(int which)
{
    int i;
    for (i = 0; ; i++)
    {
        int cloak = _cloaks[i];
        if (cloak == T_NONE) break;
        if (cloak == which) continue;
        plr_tim_remove(cloak);
    }
}

/* Cloak of Protection: This illusion makes you harder to hit
 * as well as protecting you from the illusions of others. */
static bool _cloak_protection_on(plr_tim_ptr timer)
{
    _cloak_on(timer->id);
    msg_print("You conjure a cloak of protection!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _cloak_protection_off(plr_tim_ptr timer)
{
    msg_print("Your cloak of protection vanishes.");
    plr->update |= PU_BONUS;
}
static void _cloak_protection_bonus(plr_tim_ptr timer)
{
    plr_bonus_ac(10 + 40*plr->lev/50);
    res_add(GF_CONF);
    res_add(GF_BLIND);
}
static void _cloak_protection_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_RES_(GF_CONF));
    add_flag(flgs, OF_RES_(GF_BLIND));
}
static status_display_t _cloak_protection_display(plr_tim_ptr timer)
{
    return status_display_create("Protect", "(P", TERM_L_UMBER);
}
static plr_tim_info_ptr _cloak_protection(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_CLOAK_PROTECTION, "Cloak of Protection");
    info->desc = "You are enveloped by protective illusions.";
    info->on_f = _cloak_protection_on;
    info->off_f = _cloak_protection_off;
    info->calc_bonuses_f = _cloak_protection_bonus;
    info->flags_f = _cloak_protection_flags;
    info->status_display_f = _cloak_protection_display;
    info->dispel_prob = 50;
    return info;
}
/* Cloak of Shadows */
static bool _cloak_shadows_on(plr_tim_ptr timer)
{
    _cloak_on(timer->id);
    msg_print("You conjure a cloak of shadows!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _cloak_shadows_off(plr_tim_ptr timer)
{
    msg_print("Your cloak of shadows vanishes.");
    plr->update |= PU_BONUS;
}
static void _cloak_shadows_bonus(plr_tim_ptr timer)
{
    plr->skills.stl += 3 + plr->lev/5;
    res_add(GF_LIGHT);
    res_add(GF_DARK);
}
static void _cloak_shadows_flags(plr_tim_ptr timer, u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_STEALTH);
    add_flag(flgs, OF_RES_(GF_LIGHT));
    add_flag(flgs, OF_RES_(GF_DARK));
}
static status_display_t _cloak_shadows_display(plr_tim_ptr timer)
{
    return status_display_create("Shadows", "(S", TERM_L_DARK);
}
static plr_tim_info_ptr _cloak_shadows(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_CLOAK_SHADOWS, "Cloak of Shadows");
    info->desc = "You are enveloped by illusions of shadow.";
    info->on_f = _cloak_shadows_on;
    info->off_f = _cloak_shadows_off;
    info->calc_bonuses_f = _cloak_shadows_bonus;
    info->flags_f = _cloak_shadows_flags;
    info->status_display_f = _cloak_shadows_display;
    info->dispel_prob = 10;
    return info;
}
/* Cloak of Innocence: see mon_is_hostile and mon_take_hit. */
static bool _cloak_innocence_on(plr_tim_ptr timer)
{
    _cloak_on(timer->id);
    msg_print("You conjure a cloak of innocence!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _cloak_innocence_off(plr_tim_ptr timer)
{
    msg_print("Your cloak of innocence vanishes.");
    plr->update |= PU_BONUS;
}
static void _cloak_innocence_bonus(plr_tim_ptr timer)
{
    plr->innocence = TRUE;
}
static status_display_t _cloak_innocence_display(plr_tim_ptr timer)
{
    return status_display_create("Innocence", "(I", TERM_L_GREEN);
}
static plr_tim_info_ptr _cloak_innocence(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_CLOAK_INNOCENCE, "Cloak of Innocence");
    info->desc = "You wouldn't hurt a fly ... would you?";
    info->on_f = _cloak_innocence_on;
    info->off_f = _cloak_innocence_off;
    info->calc_bonuses_f = _cloak_innocence_bonus;
    info->status_display_f = _cloak_innocence_display;
    return info;
}

/* Cloak of Invisibility: This is a powerful cloak. Its implementation
 * requires help elsewhere in the code base, much like Ninja's stealth. 
 * One difference is that any plr spell projection reveals the plr's 
 * presence: The cloak shimmers and the plr is discovered ... at least
 * for a turn. */
static bool _cloak_invis_on(plr_tim_ptr timer)
{
    _cloak_on(timer->id);
    msg_print("You conjure a cloak of invisibility!");
    plr->special_defense |= DEFENSE_INVISIBLE;
    plr->redraw |= PR_EFFECTS;
    return TRUE;
}
static void _cloak_invis_off(plr_tim_ptr timer)
{
    msg_print("Your cloak of invisibility vanishes.");
    plr->special_defense &= ~(DEFENSE_INVISIBLE);
    plr->redraw |= PR_EFFECTS;
}
static status_display_t _cloak_invis_display(plr_tim_ptr timer)
{
    return status_display_create("Invis", "(I", TERM_L_BLUE);
}
static plr_tim_info_ptr _cloak_invis(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_CLOAK_INVIS, "Invisible");
    info->desc = "You are invisible.";
    info->on_f = _cloak_invis_on;
    info->off_f = _cloak_invis_off;
    info->status_display_f = _cloak_invis_display;
    info->dispel_prob = 100;
    return info;
}

/* Masks: Only one mask may be active at a time. These are self illusions
 * of "presence" that affect LoS monsters every tick. Sleeping monsters
 * are not affected. Note that a Cloak of Invisibility will block projections! */
static int _masks[] = {T_MASK_FEAR, T_MASK_CONF, T_MASK_CHARM, T_MASK_DISCORD, T_NONE};
static void _mask_on(int which)
{
    int i;
    for (i = 0; ; i++)
    {
        int mask = _masks[i];
        if (mask == T_NONE) break;
        if (mask == which) continue;
        plr_tim_remove(mask);
    }
}
static void _mask_project(int gf, int power)
{
    if (plr->special_defense & DEFENSE_INVISIBLE) return; /* Cloak of Invisibility hides Masks */
    _project_visual(gf, power);
}

/* Terror Mask */
static bool _mask_fear_on(plr_tim_ptr timer)
{
    _mask_on(timer->id);
    msg_print("You conjure a mask of terror!");
    plr->update |= PU_BONUS;
    return TRUE;
}
static void _mask_fear_tick(plr_tim_ptr timer)
{
    timer->count--;
    _mask_project(GF_FEAR, 10 + plr->lev);
}
static void _mask_fear_off(plr_tim_ptr timer)
{
    msg_print("Your mask of terror vanishes.");
    plr->update |= PU_BONUS;
}
void _mask_fear_stats(plr_tim_ptr timer, s16b stats[MAX_STATS])
{
    stats[A_CHR] += 5;
}
static status_display_t _mask_fear_display(plr_tim_ptr timer)
{
    return status_display_create("Terror", "]F", TERM_RED);
}
static plr_tim_info_ptr _mask_fear(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_MASK_FEAR, "Terror Mask");
    info->desc = "You project an aura of fear.";
    info->on_f = _mask_fear_on;
    info->tick_f = _mask_fear_tick;
    info->off_f = _mask_fear_off;
    info->stats_f = _mask_fear_stats;
    info->status_display_f = _mask_fear_display;
    return info;
}
/* Confusion Mask */
static bool _mask_conf_on(plr_tim_ptr timer)
{
    _mask_on(timer->id);
    msg_print("You conjure a mask of confusion!");
    return TRUE;
}
static void _mask_conf_tick(plr_tim_ptr timer)
{
    timer->count--;
    _mask_project(GF_OLD_CONF, 10 + plr->lev);
}
static void _mask_conf_off(plr_tim_ptr timer)
{
    msg_print("Your mask of confusion vanishes.");
}
static status_display_t _mask_conf_display(plr_tim_ptr timer)
{
    return status_display_create("Conf", "]C", TERM_L_UMBER);
}
static plr_tim_info_ptr _mask_conf(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_MASK_CONF, "Mask of Confusion");
    info->desc = "You project an aura of confusion.";
    info->on_f = _mask_conf_on;
    info->tick_f = _mask_conf_tick;
    info->off_f = _mask_conf_off;
    info->status_display_f = _mask_conf_display;
    return info;
}

/* Mask of Command */
static bool _mask_command_on(plr_tim_ptr timer)
{
    _mask_on(timer->id);
    msg_print("You conjure a mask of command!");
    return TRUE;
}
static void _mask_command_tick(plr_tim_ptr timer)
{
    timer->count--;
    if (plr->special_defense & DEFENSE_INVISIBLE) return; /* Cloak of Invisibility hides Masks */
    _project_command();
}
static void _mask_command_off(plr_tim_ptr timer)
{
    int i;
    vec_ptr v = mon_pack_filter(plr_pack(), mon_is_temp_pet); /* copy since set_hostile does mon_pack_remove */
    msg_print("Your mask of command vanishes.");
    for (i = 0; i < vec_length(v); i++)
    {
        mon_ptr mon = vec_get(v, i);
        if (mon_show_msg(mon))
        {
            char name[MAX_NLEN_MON];
            monster_desc(name, mon, 0);
            msg_format("%^s gets angry!", name);
        }
        set_hostile(mon); /* note: friendly monsters don't appreciate being used this way! */
    }
    vec_free(v);
}
static status_display_t _mask_command_display(plr_tim_ptr timer)
{
    return status_display_create("Command", "]C", TERM_L_GREEN);
}
static plr_tim_info_ptr _mask_command(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_MASK_CHARM, "Mask of Command");
    info->desc = "You project an aura of command.";
    info->on_f = _mask_command_on;
    info->tick_f = _mask_command_tick;
    info->off_f = _mask_command_off;
    info->status_display_f = _mask_command_display;
    return info;
}

/* Mask of Discord */
static bool _mask_discord_on(plr_tim_ptr timer)
{
    _mask_on(timer->id);
    msg_print("You conjure a mask of discord!");
    return TRUE;
}
static void _mask_discord_tick(plr_tim_ptr timer)
{
    timer->count--;
    if (plr->special_defense & DEFENSE_INVISIBLE) return; /* Cloak of Invisibility hides Masks */
    if (_project_discord())
        msg_print("You sow the seeds of discord!");
}
static void _mask_discord_off(plr_tim_ptr timer)
{
    msg_print("Your mask of discord vanishes.");
}
static status_display_t _mask_discord_display(plr_tim_ptr timer)
{
    return status_display_create("Discord", "]D", TERM_VIOLET);
}
static plr_tim_info_ptr _mask_discord(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(T_MASK_DISCORD, "Mask of Discord");
    info->desc = "You project an aura of discord.";
    info->on_f = _mask_discord_on;
    info->tick_f = _mask_discord_tick;
    info->off_f = _mask_discord_off;
    info->status_display_f = _mask_discord_display;
    return info;
}

/* register the timers: these are global timers available in every game */
void illusion_register_timers(void)
{
    plr_tim_register(_cloak_innocence());
    plr_tim_register(_cloak_invis());
    plr_tim_register(_cloak_protection());
    plr_tim_register(_cloak_shadows());
    plr_tim_register(_mask_fear());
    plr_tim_register(_mask_conf());
    plr_tim_register(_mask_discord());
    plr_tim_register(_mask_command());
}

/************************************************************************
 * Spells
 ************************************************************************/
void confusing_lights(int power) /* EFFECT_CONFUSING_LIGHT */
{
    /* XXX Cloak of Invisibility does not block */
    _project_visual(GF_SLOW, power * 2);
    _project_visual(GF_STUN, 5 + power/2);
    _project_visual(GF_OLD_CONF, power * 2);
    _project_visual(GF_FEAR, power * 2);
    _project_visual(GF_STASIS, power * 3 / 4);
}
static void _jump(int gf, int dam)
{
    int rad = MIN(MAX_PRECOMPUTE_DISTANCE, 2 + plr->lev/5);
    plr_burst(rad, gf, dam);
    teleport_player(20, TELEPORT_OUT_OF_SIGHT);
}
static bool _wall(point_t pos, int dur)
{
    dun_cell_ptr cell = dun_cell_at(cave, pos);
    if (!floor_is_clean(cell)) return FALSE;
    if (dun_mon_at(cave, pos)) return FALSE;
    dun_tim_add_at(cave, pos, DT_ILLUSION, dur);
    return TRUE;
} 
cptr do_illusion_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME);
    bool desc = (mode == SPELL_DESC);
    bool info = (mode == SPELL_INFO);
    bool cast = (mode == SPELL_CAST);

    switch (spell)
    {
    /* Elusive Beginnings */
    case 0:
        if (name) return "Detect Illusion";
        if (desc) return "Detects all nearby illusions including secret traps and doors. Also reveals invisible monsters and mimics.";
       {int rad = spell_power(DETECT_RAD_DEFAULT);
        if (info) return info_radius(rad);
        if (cast) {
            detect_secret_doors(rad);
            detect_secret_traps(rad);
            detect_monsters_invis(rad);
            detect_monsters_mimic(rad);
        }}
        break;
    case 1:
        if (name) return "Illumination";
        if (desc) return "Conjure light to illuminate your surroundings.";
       {int dd = 2, ds = 5, rad = plr->lev/10 + 1;
        if (info) return info_damage(dd, ds, 0);
        if (cast) lite_area(damroll(dd, ds), rad);}
        break;
    case 2:
        if (name) return "Blink Self";
        if (desc) return "This short range teleport might help you escape a tricky situation.";
       {int range = 10;
        if (info) return info_range(range);
        if (cast)
        {
            if (mut_present(MUT_ASTRAL_GUIDE))
                energy_use /= 3;
            teleport_player(range, 0);
        }}
        break;
    case 3:
        if (name) return "Flash of Confusion";
        if (desc) return "Weave a brief flash of dazzling lights in the face of a single nearby monster.";
       {int power = spell_power(5 + plr->lev*3/2);
        if (info) return info_power(power);
        if (cast) {
            mon_ptr mon = plr_target_mon();
            if (!mon) return NULL;
            _flash(mon, GF_OLD_CONF, power);
        }}
        break;
    case 4:
        if (name) return "Flash of Light";
        if (desc) return "Weave a brief flash of blinding lights in the face of a single nearby monster.";
       {int power = spell_power(5 + plr->lev*3/2);
        if (info) return info_power(power);
        if (cast) {
            mon_ptr mon = plr_target_mon();
            if (!mon) return NULL;
            _flash(mon, GF_BLIND, power);
        }}
        break;
    case 5:
        if (name) return "Flash of Mesmerism";
        if (desc) return "Weave a brief flash of hypnotic lights in the face of a single nearby monster.";
       {int power = spell_power(5 + plr->lev*3/2);
        if (info) return info_power(power);
        if (cast) {
            mon_ptr mon = plr_target_mon();
            if (!mon) return NULL;
            _flash(mon, GF_SLEEP, power);
        }}
        break;
    case 6:
        if (name) return "Resist Confusion";
        if (desc) return "For a short while you will gain enhanced resistance to confusion.";
       {int base = 20;
        int ds = plr->lev;
        if (info) return info_duration(base, ds); 
        if (cast) plr_tim_add(T_RES_CONF, spell_power(base + _1d(ds)));}
        break;
    case 7:
        if (name) return "Spying";
        if (desc) return "For a short while you will gain telepathic awareness of nearby monsters.";
       {int base = 20;
        int ds = 25 + plr->lev/2;
        if (info) return info_duration(base, ds); 
        if (cast) plr_tim_add(T_TELEPATHY, spell_power(base + _1d(ds)));}
        break;

    /* The Art of Misdirection */
    case 8:
        if (name) return "Diversion";
        if (desc) return "Disappear in a flash of light. Visible monsters are confused by your sudden disappearance, and proceed to your former location to investigate.";
        if (cast) {
            lite_area(_1d(4), 2);
            _divert_monsters(plr->pos);
            teleport_player(20, TELEPORT_OUT_OF_SIGHT);
        }
        break;
    case 9:
        if (name) return "Klaxon";
        if (desc) return "This stationary, auditory illusion will make a horribly aggravating noise. Better move quickly away!";
        if (cast) dun_tim_add_at(plr_dun(), plr->pos, DT_KLAXON, 100 + _1d(100));
        break;
    case 10:
        if (name) return "Summon Illusion";
        if (desc) return "Conjure a single, illusory monster. While not actually dangerous, it can divert and distract enemy monsters.";
        if (cast) _summon_illusion(SUMMON_MONSTER, 1);
        break;
    case 11:
        if (name) return "Terror Mask";
        if (desc) return "This illusion of self will alter your appearance into something truly frightening. Don't look in the mirror!";
       {int base = 10;
        int ds = 10;
        if (info) return info_duration(base, ds); 
        if (cast) plr_tim_add(T_MASK_FEAR, spell_power(base + _1d(ds)));}
        break;
    case 12:
        if (name) return "Blink Other";
        if (desc) return "Teleport a single monster a short distance away.";
        if (cast) {
            mon_ptr mon = plr_target_mon();
            if (!mon) return NULL;
            gf_affect_m(who_create_plr(), mon, GF_TELEPORT, 10, GF_AFFECT_SPELL);
        }
        break;
    case 13:
        if (name) return "Smoke Jump";
        if (desc) return "Disappear in a confusing blast of smoke.";
       {int power = 25 + plr_prorata_level(100);
        if (info) return info_power(power);
        if (cast) _jump(GF_OLD_CONF, power);}
        break;
    case 14:
        if (name) return "Flash Bang";
        if (desc) return "Disappear in a loud, blinding flash of light.";
       {int power = 25 + plr_prorata_level(100);
        if (info) return info_power(power);
        if (cast) {
            _project_flash_bang(power);
            teleport_player(20, TELEPORT_OUT_OF_SIGHT);
        }}
        break;
    case 15:
        if (name) return "Dragonic Prism";
        if (desc) return "Conjure one or more high level, illusory dragons.";
        if (cast) _summon_illusion(SUMMON_HI_DRAGON, _1d(3));
        break;

    /* Illusion Mastery */
    case 16:
        if (name) return "Cloak of Protection";
        if (desc) return "This illusion of self envelops you in weavings of light both protective and deceptive.";
       {int base = 20;
        int ds = plr->lev;
        if (info) return info_duration(base, ds); 
        if (cast) plr_tim_add(T_CLOAK_PROTECTION, spell_power(base + _1d(ds)));}
        break;
    case 17:
        if (name) return "Baffling Presence";
        if (desc) return "This illusion of self will alter your appearance into something utterly incongruous and bizarre. Monsters are baffled by your presence.";
       {int base = 10;
        int ds = 10;
        if (info) return info_duration(base, ds); 
        if (cast) plr_tim_add(T_MASK_CONF, spell_power(base + _1d(ds)));}
        break;
    case 18:
        if (name) return "Decoy";
        if (desc) return "Teleport away, leaving an illusory decoy in your place to lure visible monsters.";
        if (cast) {
            point_t pos = plr->pos;
            mon_ptr decoy;
            teleport_player(20, TELEPORT_OUT_OF_SIGHT);
            if (point_equals(plr->pos, pos)) /* extreme paranoia */
            {
                msg_print("Failed!");
                return "";
            }
            decoy = _summon_decoy(pos);
            if (decoy) /* paranoia */
                _decoy_monsters(decoy);
        }
        break;
    case 19:
        if (name) return "Cloak of Shadows";
        if (desc) return "This illusion of self hides your movement in shadows granting enhanced stealth as well as resistance to light and darkness.";
       {int base = 20;
        int ds = plr->lev;
        if (info) return info_duration(base, ds); 
        if (cast) plr_tim_add(T_CLOAK_SHADOWS, spell_power(base + _1d(ds)));}
        break;
    case 20:
        if (name) return "Wall of Illusion";
        if (desc) return "Conjure an illusory wall.";
       {int base = 20;
        int ds = plr->lev;
        if (info) return info_duration(base, ds); 
        if (cast) {
            if (!_wall(plr->pos, spell_power(base + _1d(ds))))
                msg_print("Failed");
        }}
        break;
    case 21:
        if (name) return "Cloak of Innocence";
        if (desc) return "This illusion of self masks your true intentions. Enemy monsters no longer consider you hostile ... at least until you show them otherwise!";
       {int base = 20;
        int ds = plr->lev;
        if (info) return info_duration(base, ds); 
        if (cast) plr_tim_add(T_CLOAK_INNOCENCE, spell_power(base + _1d(ds)));}
        break;
    case 22:
        if (name) return "Bind Monster";
        if (desc) return "Bind a single monster in illusory ropes, precluding movement. The ropes are just illusions but it won't know that!";
        if (cast) {
            mon_ptr mon = plr_target_mon();
            if (!mon) return FALSE;
            mon_tim_add(mon, MT_BOUND, spell_power(5 + _1d(5)));
            /* XXX A bound monster can still see and be affected by illusions; can still cast
             * spells; can still attack adjacent monsters. However, it cannot move! cf mon_ai,
             * especially _try_move_aux. cf _skip_blow in mon_attack.c: the bindings block some
             * forms of melee attacks. */
        }
        break;
    case 23:
        if (name) return "Cloak of Invisibility";
        if (desc) return "For a short time, you will be hidden from ordinary sight. Attacking or spellcasting will temporarily reveal your presence, however.";
       {int base = 20;
        int ds = plr->lev;
        if (info) return info_duration(base, ds); 
        if (cast) plr_tim_add(T_CLOAK_INVIS, spell_power(base + _1d(ds)));}
        break;

    /* Phantasmagoric Tome */
    case 24:
        if (name) return "Hasty Illusions"; /* XXX running out of ideas ... */
        if (desc) return "With increased speed comes increased powers of deception.";
        if (cast) plr_tim_add(T_FAST, spell_power(25 + _1d(25)));
        break;
    case 25:
        if (name) return "Prison of Light";
        if (desc) return "Weave an illusory wall of stone around a single, visible monster.";
        if (cast) {
            int i;
            mon_ptr mon = plr_target_mon();
            if (!mon) return NULL;
            for (i = 0; i < 8; i++)
            {
                point_t p = point_step(mon->pos, ddd[i]);
                _wall(p, 20 + _1d(20));
            }
        }
        break;
    case 26:
        if (name) return "Step into Shadows";
        if (desc) return "Teleport to given location.";
       {int range = spell_power(plr->lev/2 + 10);
        if (info) return info_range(range);
        if (cast) {
            msg_print("You open a dimensional gate. Choose a destination.");
            if (!dimension_door(range)) return NULL;
        }}
        break;
    case 27:
        if (name) return "Confusing Lights";
        if (desc) return "This visual illusion emits a vast array of dazzling lights. Monsters may be slowed, stunned, confused or frightened by this awesome display of mastery over light.";
       {int power = spell_power(plr_prorata_level(100));
        if (info) return info_power(power);
        if (cast) {
            msg_print("You glare nearby monsters with a dazzling array of confusing lights!");
            confusing_lights(power);
        }}
        break;
    case 28:
        if (name) return "Mask of Command";
        if (desc) return "This illusion of self will project an aura of command causing monsters to follow your lead!";
       {int base = 5;
        int ds = 5;
        if (info) return info_duration(base, ds); 
        if (cast) plr_tim_add(T_MASK_CHARM, spell_power(base + _1d(ds)));}
        break;
    case 29:
        if (name) return "Mask of Discord";
        if (desc) return "This illusion of self will project an aura of discord causing monsters to turn on each other!";
       {int base = 5;
        int ds = 5;
        if (info) return info_duration(base, ds); 
        if (cast) plr_tim_add(T_MASK_DISCORD, spell_power(base + _1d(ds)));}
        break;
    case 30:
        if (name) return "Hall of Mirrors";
        if (desc) return "Create numerous illusions of yourself.";
        if (cast) {
            int ct = 2 + randint0(4), i;
            for (i = 0; i < ct; i++)
                _summon_decoy(plr->pos);
        }
        break;
    case 31:
        if (name) return "Phantasmagoria";
        if (desc) return "Conjure a vast array of illusionary monsters to overwhelm and distract your enemies.";
        if (cast) {
            int i;
            for (i = 0; i < 18; i++)
            {
                int attempt = 30;
                point_t mp;
                int what;

                while (attempt--)
                {
                    mp = scatter(plr->pos, MAX_RANGE);
                    if (dun_allow_mon_at(cave, mp)) break;
                }
                if (attempt < 0) continue;
                switch (_1d(7))
                {
                case 1: what = SUMMON_HI_UNDEAD; break;
                case 2: what = SUMMON_HI_DRAGON; break;
                case 3: what = SUMMON_HI_DEMON; break;
                case 4: what = SUMMON_ANGEL; break;
                case 5: what = SUMMON_CYBER; break;
                default: what = SUMMON_MONSTER; break;
                }
                summon_specific(who_create_plr(), mp, _summon_lvl(), what, PM_FORCE_PET | PM_ILLUSION | PM_HASTE | PM_ALLOW_GROUP);
            }
        }
        break;
    }
    return "";
}
