#include "angband.h"

/****************************************************************
 * Helpers
 ****************************************************************/
static int _get_toggle(void)
{
    return plr->magic_num1[0];
}

static int _set_toggle(s32b toggle)
{
    int result = plr->magic_num1[0];

    if (toggle == result) return result;

    plr->magic_num1[0] = toggle;

    plr->redraw |= PR_STATUS;
    plr->update |= PU_BONUS;
    handle_stuff();

    return result;
}

int mystic_get_toggle(void)
{
    int result = TOGGLE_NONE;
    if (plr->pclass == CLASS_MYSTIC && !heavy_armor())
        result = _get_toggle();
    return result;
}

/****************************************************************
 * Mystic Combat
 ****************************************************************/
enum {
    _MYSTIC_ACID = PLR_HIT_CUSTOM,
    _MYSTIC_ELEC,
    _MYSTIC_FIRE,
    _MYSTIC_COLD,
    _MYSTIC_POIS,
    _MYSTIC_KNOCKOUT,
};
static bool _begin_weapon(plr_attack_ptr context)
{
    if (context->info.type == PAT_MONK)
    {
        switch (context->mode)
        {
        case _MYSTIC_KNOCKOUT:
            context->info.base_blow = 100;
            context->info.xtra_blow = 0;
            context->info.info = "Attempt to knockout an adjacent opponent.";
            break;
        case PLR_HIT_CRIT:
            context->info.crit.qual_add += 650;
            break;
        }
    }
    return TRUE;
}
static slay_t _calc_brand(plr_attack_ptr context, slay_ptr best_brand)
{
    slay_t brand = {0};
    bool display = BOOL(context->flags & PAC_DISPLAY);
    int res_pct = 0;
    if (context->info.type != PAT_MONK) return brand;
    switch (context->mode)
    {
    case _MYSTIC_ACID:
        if (!display) res_pct = mon_res_pct(context->mon, GF_ACID);
        if (res_pct) mon_lore_resist(context->mon, GF_ACID);
        if (res_pct <= 0)
        {
            brand.id = OF_BRAND_ACID;
            brand.name = "Acid";
            if (have_flag(context->obj_flags, OF_BRAND_ACID)) brand.mul = 350;
            else brand.mul = 250;
            brand.add = 5;
            if (res_pct < 0) slay_scale(&brand, 100 - res_pct);
        }
        break;
    case _MYSTIC_ELEC:
        if (!display) res_pct = mon_res_pct(context->mon, GF_ELEC);
        if (res_pct) mon_lore_resist(context->mon, GF_ELEC);
        if (res_pct <= 0)
        {
            brand.id = OF_BRAND_ELEC;
            brand.name = "Elec";
            if (have_flag(context->obj_flags, OF_BRAND_ELEC)) brand.mul = 600;
            else brand.mul = 500;
            brand.add = 7;
            if (res_pct < 0) slay_scale(&brand, 100 - res_pct);
        }
        break;
    case _MYSTIC_FIRE:
        if (!display) res_pct = mon_res_pct(context->mon, GF_FIRE);
        if (res_pct) mon_lore_resist(context->mon, GF_FIRE);
        if (res_pct <= 0)
        {
            brand.id = OF_BRAND_FIRE;
            brand.name = "Fire";
            if (have_flag(context->obj_flags, OF_BRAND_FIRE)) brand.mul = 350;
            else brand.mul = 250;
            brand.add = 3;
            if (res_pct < 0) slay_scale(&brand, 100 - res_pct);
        }
        break;
    case _MYSTIC_COLD:
        if (!display) res_pct = mon_res_pct(context->mon, GF_COLD);
        if (res_pct) mon_lore_resist(context->mon, GF_COLD);
        if (res_pct <= 0)
        {
            brand.id = OF_BRAND_COLD;
            brand.name = "Cold";
            if (have_flag(context->obj_flags, OF_BRAND_COLD)) brand.mul = 350;
            else brand.mul = 250;
            brand.add = 3;
            if (res_pct < 0) slay_scale(&brand, 100 - res_pct);
        }
        break;
    case _MYSTIC_POIS:
        if (!display) res_pct = mon_res_pct(context->mon, GF_POIS);
        if (res_pct) mon_lore_resist(context->mon, GF_POIS);
        if (res_pct <= 0)
        {
            brand.id = OF_BRAND_POIS;
            brand.name = "Poison";
            if (have_flag(context->obj_flags, OF_BRAND_POIS)) brand.mul = 350;
            else brand.mul = 250;
            brand.add = 5;
            if (res_pct < 0) slay_scale(&brand, 100 - res_pct);
        }
        break;
    }
    return brand;
}
static void _after_hit(plr_attack_ptr context)
{
    if (context->info.type != PAT_MONK) return;
    if (context->mode == _MYSTIC_KNOCKOUT && !mon_tim_find(context->mon, T_PARALYZED))
    {
        if (mon_save_p(context->mon, A_DEX))
        {
            msg_format("%^s resists.", context->mon_name);
        }
        else
        {
            msg_format("%^s is <color:b>knocked out</color>.", context->mon_name);
            mon_tim_add(context->mon, T_PARALYZED, randint1(3));
        }
    }
}
static void _end_weapon(plr_attack_ptr context)
{
}
static void _strike_spell(int mode, int cmd, var_ptr res)
{
    if (cmd == SPELL_CAST || cmd == SPELL_ON_BROWSE)
    {
        plr_attack_t ctx = {0};
        ctx.mode = mode;
        ctx.flags = PAC_NO_INNATE;
        ctx.hooks.begin_weapon_f = _begin_weapon;
        ctx.hooks.after_hit_f = _after_hit;
        ctx.hooks.calc_brand_f = _calc_brand;
        ctx.hooks.end_weapon_f = _end_weapon;
        if (cmd == SPELL_CAST)
            var_set_bool(res, plr_attack_special_aux(&ctx, 1));
        else if (mode != PLR_HIT_KILL && mode != PLR_HIT_STUN)
        {
            plr_attack_display_aux(&ctx);
            var_set_bool(res, TRUE);
        }
    }
    else default_spell(cmd, res);
}

/****************************************************************
 * Mystic Spells
 ****************************************************************/
static void _toggle_spell(int which, int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (_get_toggle() == which)
            _set_toggle(TOGGLE_NONE);
        else
            _set_toggle(which);
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        if (_get_toggle() != which)
            var_set_int(res, 0);    /* no charge for dismissing a technique */
        else
            var_set_int(res, 100);
        break;
    default:
        default_spell(cmd, res);
    }
}
static void _acid_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Corrosive Blow");
        break;
    default:
        _strike_spell(_MYSTIC_ACID, cmd, res);
    }
}

static void _cold_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Icy Fists");
        break;
    default:
        _strike_spell(_MYSTIC_COLD, cmd, res);
    }
}

static void _crushing_blow_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Crushing Blow");
        break;
    default:
        _strike_spell(PLR_HIT_CRIT, cmd, res);
    }
}

static void _defense_toggle_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Defensive Stance");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique, you gain increased armor class at the expense of your fighting prowess.");
        break;
    default:
        _toggle_spell(MYSTIC_TOGGLE_DEFENSE, cmd, res);
    }
}

static void _elec_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Lightning Eagle");
        break;
    default:
        _strike_spell(_MYSTIC_ELEC, cmd, res);
    }
}

static void _fast_toggle_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Quick Approach");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique, you will move with great haste.");
        break;
    default:
        _toggle_spell(MYSTIC_TOGGLE_FAST, cmd, res);
    }
}

static void _fire_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Flaming Strike");
        break;
    default:
        _strike_spell(_MYSTIC_FIRE, cmd, res);
    }
}

static void _killing_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Touch of Death");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to kill an adjacent opponent with a single blow.");
        break;
    default:
        _strike_spell(PLR_HIT_KILL, cmd, res);
    }
}

static void _knockout_blow_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Knockout Blow");
        break;
    default:
        _strike_spell(_MYSTIC_KNOCKOUT, cmd, res);
    }
}

static void _mystic_insights_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mystic Insights");
        break;
    default:
        probing_spell(cmd, res);
    }
}

static void _offense_toggle_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Death Stance");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique, you concentrate all your mental efforts on offensive deadliness. As such, you become more exposed to enemy attacks.");
        break;
    default:
        _toggle_spell(MYSTIC_TOGGLE_OFFENSE, cmd, res);
    }
}

static void _poison_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Serpent's Tongue");
        break;
    default:
        _strike_spell(_MYSTIC_POIS, cmd, res);
    }
}

static void _retaliate_toggle_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Aura of Retaliation");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique, you will retaliate when struck.");
        break;
    default:
        _toggle_spell(MYSTIC_TOGGLE_RETALIATE, cmd, res);
    }
}

static void _stealth_toggle_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Stealthy Approach");
        break;
    case SPELL_DESC:
        var_set_string(res, "When using this technique, you will gain enhanced stealth.");
        break;
    default:
        _toggle_spell(MYSTIC_TOGGLE_STEALTH, cmd, res);
    }
}

static void _stunning_blow_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Stunning Blow");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent opponent with stunning blows.");
        break;
    default:
        _strike_spell(PLR_HIT_STUN, cmd, res);
    }
}

static void _summon_hounds_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Hounds");
        break;
    case SPELL_DESC:
        var_set_string(res, "Summon hounds for assistance.");
        break;
    case SPELL_CAST:
    {
        int num = 1; /* randint0(plr->lev/10); */
        int ct = 0, i;
        int l = plr->lev + randint1(plr->lev);

        for (i = 0; i < num; i++)
        {
            ct += summon_specific(who_create_plr(), plr->pos, l, SUMMON_HOUND, PM_FORCE_PET | PM_ALLOW_GROUP);
        }
        if (!ct)
            msg_print("No hounds arrive.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
    }
}

static void _summon_spiders_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Spiders");
        break;
    case SPELL_DESC:
        var_set_string(res, "Summon spiders for assistance.");
        break;
    case SPELL_CAST:
    {
        int num = 1; /* randint0(plr->lev/10); */
        int ct = 0, i;
        int l = plr->lev + randint1(plr->lev);

        for (i = 0; i < num; i++)
        {
            ct += summon_specific(who_create_plr(), plr->pos, l, SUMMON_SPIDER, PM_FORCE_PET | PM_ALLOW_GROUP);
        }
        if (!ct)
            msg_print("No spiders arrive.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
    }
}

/****************************************************************
 * Spell Table and Exports
 ****************************************************************/
static spell_info _spells[] =
{
    /*lvl cst fail spell */
    {  1,  0,  0, samurai_concentration_spell},
    {  3,  8,  0, _fire_strike_spell},
    {  5,  8, 30, _summon_spiders_spell},
    {  7,  8,  0, _cold_strike_spell},
    {  9, 10, 30, detect_menace_spell},
    { 11, 10,  0, _poison_strike_spell},
    { 13, 15, 40, sense_surroundings_spell},
    { 15,  0,  0, _stealth_toggle_spell},
    { 17,  0,  0, _fast_toggle_spell},
    { 19,  0,  0, _defense_toggle_spell},
    { 21, 15, 50, _mystic_insights_spell},
    { 25, 17,  0, _acid_strike_spell},
    { 27, 20,  0, _stunning_blow_spell},
    { 29,  0,  0, _retaliate_toggle_spell},
    { 30, 30, 60, haste_self_spell},
    { 32, 30, 60, resistance_spell},
    { 33, 30,  0, _elec_strike_spell},
    { 35, 20, 60, rush_attack_spell},
    { 36, 40, 60, _summon_hounds_spell},
    { 37,  0,  0, _offense_toggle_spell},
    { 39, 40,  0, _knockout_blow_spell},
    { 42, 50,  0, _killing_strike_spell},
    { 45, 70,  0, _crushing_blow_spell},
    { -1, -1, -1, NULL}
};

static int _get_spells(spell_info* spells, int max)
{
    return get_spells_aux(spells, max, _spells);
}
static void _calc_bonuses(void)
{
    plr->monk_lvl = plr->lev;
    if (!heavy_armor())
    {
        plr->pspeed += plr->lev/10;
        if  (plr->lev >= 25)
            plr->free_act++;

        switch (_get_toggle())
        {
        case MYSTIC_TOGGLE_STEALTH:
            plr->skills.stl += 2 + 3 * plr->lev/50;
            break;
        case MYSTIC_TOGGLE_FAST:
            plr->quick_walk = TRUE;
            break;
        case MYSTIC_TOGGLE_DEFENSE:
        {
            int bonus = 10 + 40*plr->lev/50;
            plr->to_a += bonus;
            plr->dis_to_a += bonus;
            break;
        }
        case MYSTIC_TOGGLE_OFFENSE:
        {
            int penalty = 10 + 40*plr->lev/50;
            plr->to_a -= penalty;
            plr->dis_to_a -= penalty;
            break;
        }
        }
    }
    monk_ac_bonus();
}
static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (!heavy_armor())
    {
        if (_get_toggle() == MYSTIC_TOGGLE_RETALIATE)
            add_flag(flgs, OF_AURA_REVENGE);
        if (plr->lev >= 10)
            add_flag(flgs, OF_SPEED);
        if (plr->lev >= 25)
            add_flag(flgs, OF_FREE_ACT);
    }
}
static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "mystic technique";
        me.which_stat = A_CHR;
        me.encumbrance.max_wgt = 350;
        me.encumbrance.weapon_pct = 100;
        me.encumbrance.enc_wgt = 800;
        me.options = CASTER_SUPERCHARGE_MANA | CASTER_GAIN_SKILL;
        init = TRUE;
    }
    return &me;
}
static void _character_dump(doc_ptr doc)
{
    spell_info spells[MAX_SPELLS];
    int        ct = _get_spells(spells, MAX_SPELLS);

    plr_display_spells(doc, spells, ct);
}
static status_display_t _status_display(void)
{
    status_display_t d = {0};
    switch (_get_toggle())
    {
    case MYSTIC_TOGGLE_STEALTH:
        d.name = "Stealth"; d.abbrev = "Sl"; d.color = TERM_L_DARK;
        break;
    case MYSTIC_TOGGLE_FAST:
        d.name = "Fast"; d.abbrev = "Fs"; d.color = TERM_YELLOW;
        break;
    case MYSTIC_TOGGLE_RETALIATE:
        d.name = "Retaliate"; d.abbrev = "Rt"; d.color = TERM_L_BLUE;
        break;
    case MYSTIC_TOGGLE_OFFENSE:
        d.name = "Death"; d.abbrev = "Of"; d.color = TERM_L_RED;
        break;
    case MYSTIC_TOGGLE_DEFENSE:
        d.name = "Defense"; d.abbrev = "Df"; d.color = TERM_L_UMBER;
        break;
    }
    return d;
}
plr_class_ptr mystic_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 45,  34,  36,   5,  32,  24,  64,  40};
    skills_t xs = { 75,  55,  50,   0,   0,   0,  90,  75};

        me = plr_class_alloc(CLASS_MYSTIC);
        me->name = "Mystic";
        me->desc = "Mystics are masters of bare handed fighting, like Monks. However, they "
                  "do not learn normal spells. Instead, they gain mystical powers with experience, "
                  "and these powers directly influence their martial arts. In this respect, "
                  "Mystics are similar to the Samurai. Indeed, they even concentrate to boost "
                  "their mana like the Samurai. Mystics eschew weapons of any kind and require "
                  "the lightest of armors in order to practice their martial arts. The number "
                  "of attacks are influenced by dexterity and experience level while the mystic's "
                  "mana and fail rates are influenced by charisma. Mystics are in tune with the "
                  "natural forces around them and may even call on aid when necessary. It has "
                  "been whispered that mystics have even discovered how to kill an opponent with "
                  "a single touch, though they do not share this knowledge with novices.";

        me->stats[A_STR] =  2;
        me->stats[A_INT] = -1;
        me->stats[A_WIS] = -2;
        me->stats[A_DEX] =  3;
        me->stats[A_CON] =  1;
        me->stats[A_CHR] =  2;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 100;
        me->base_hp = 4;
        me->exp = 130;
        me->pets = 35;
        me->flags = CLASS_SENSE1_MED | CLASS_SENSE1_WEAK |
                    CLASS_SENSE2_SLOW | CLASS_SENSE2_STRONG | CLASS_MARTIAL_ARTS;

        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_spells = _get_spells;
        me->hooks.character_dump = _character_dump;
        me->hooks.status_display = _status_display;
    }

    return me;
}
