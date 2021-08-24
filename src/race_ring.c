#include "angband.h"

static int _count(int list[])
{
    int i;
    for (i = 0; ; i++)
    {
        if (list[i] == -1) return i;
    }
    /* return 0;  error: missing sentinel ... unreachable */
}

static int _random(int list[])
{
    return list[randint0(_count(list))];
}

/**********************************************************************
 * Essences
 **********************************************************************/
#define _MAX_ESSENCE 255
#define _ESSENCE_AC  254

static int _essences[_MAX_ESSENCE] = {0};
static int _effects[EFFECT_MAX] = {0};

static int _bounds_check(int n)
{
    if (n < 0) n = 0;
    if (n > 10000) n = 10000;
    return n;
}

static void _load(savefile_ptr file)
{
    int ct, i;

    /* Essences */
    for (i = 0; i < _MAX_ESSENCE; i++)
        _essences[i] = 0;

    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        int j = savefile_read_s16b(file);
        int n = _bounds_check(savefile_read_s16b(file));


        if (0 <= j && j < _MAX_ESSENCE)
            _essences[j] += n;
    }

    /* Effects (i.e. Activations) */
    for (i = 0; i < EFFECT_MAX; i++)
        _effects[i] = 0;

    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        int j = savefile_read_s16b(file);
        int n = _bounds_check(savefile_read_s16b(file));

        if (0 <= j && j < EFFECT_MAX)
            _effects[j] += n;
    }
}

static void _save(savefile_ptr file)
{
    int ct = 0, i;

    /* Essences */
    for (i = 0; i < _MAX_ESSENCE; i++)
    {
        if (_essences[i])
            ct++;
    }

    savefile_write_s16b(file, ct);

    for (i = 0; i < _MAX_ESSENCE; i++)
    {
        if (_essences[i])
        {
            savefile_write_s16b(file, i);
            savefile_write_s16b(file, _essences[i]);
        }
    }

    /* Effects */
    ct = 0;
    for (i = 0; i < EFFECT_MAX; i++)
    {
        if (_effects[i])
            ct++;
    }

    savefile_write_s16b(file, ct);

    for (i = 0; i < EFFECT_MAX; i++)
    {
        if (_effects[i])
        {
            savefile_write_s16b(file, i);
            savefile_write_s16b(file, _effects[i]);
        }
    }
}

static bool _of_filter(of_info_ptr info)
{
    switch (info->id)
    {
    case OF_BRAND_MANA:
    case OF_TUNNEL:
    case OF_BLOWS:
    case OF_BRAND_CHAOS:
    case OF_BRAND_VAMP:
    case OF_SLAY_ANIMAL:
    case OF_SLAY_EVIL:
    case OF_SLAY_UNDEAD:
    case OF_SLAY_DEMON:
    case OF_SLAY_ORC:
    case OF_SLAY_TROLL:
    case OF_SLAY_GIANT:
    case OF_SLAY_DRAGON:
    case OF_KILL_DRAGON:
    case OF_VORPAL:
    case OF_IMPACT:
    case OF_BRAND_POIS:
    case OF_BRAND_ACID:
    case OF_BRAND_ELEC:
    case OF_BRAND_FIRE:
    case OF_BRAND_COLD:
    case OF_RIDING:
    case OF_THROWING:
    case OF_SLAY_HUMAN:
   /*case TR_NO_TELE:*/
    case OF_NO_MAGIC:
    case OF_TY_CURSE:
    case OF_HIDE_TYPE:
    case OF_SHOW_MODS:
    case OF_WEAPONMASTERY:
    case OF_XTRA_MIGHT:
    case OF_XTRA_SHOTS:
    case OF_IGNORE_ACID:
    case OF_IGNORE_ELEC:
    case OF_IGNORE_FIRE:
    case OF_IGNORE_COLD:
    case OF_ACTIVATE:
    case OF_DRAIN_EXP:
    case OF_TELEPORT:
    case OF_AGGRAVATE:
    case OF_BLESSED:
    case OF_KILL_ANIMAL:
    case OF_KILL_EVIL:
    case OF_KILL_UNDEAD:
    case OF_KILL_DEMON:
    case OF_KILL_ORC:
    case OF_KILL_TROLL:
    case OF_KILL_GIANT:
    case OF_KILL_HUMAN:
    case OF_FULL_NAME:
    case OF_FIXED_FLAVOR:
    case OF_DARKNESS:
    case OF_SLAY_GOOD:
    case OF_DEC_STR:
    case OF_DEC_INT:
    case OF_DEC_WIS:
    case OF_DEC_DEX:
    case OF_DEC_CON:
    case OF_DEC_CHR:
    case OF_VULN_(GF_ACID):
    case OF_VULN_(GF_ELEC):
    case OF_VULN_(GF_FIRE):
    case OF_VULN_(GF_COLD):
    case OF_VULN_(GF_POIS):
    case OF_VULN_(GF_FEAR):
    case OF_VULN_(GF_LIGHT):
    case OF_VULN_(GF_DARK):
    case OF_VULN_(GF_BLIND):
    case OF_VULN_(GF_CONF):
    case OF_VULN_(GF_SOUND):
    case OF_VULN_(GF_SHARDS):
    case OF_VULN_(GF_NETHER):
    case OF_VULN_(GF_NEXUS):
    case OF_VULN_(GF_CHAOS):
    case OF_VULN_(GF_DISEN):
    case OF_DEC_STEALTH:
    case OF_DEC_SPEED:
    case OF_DEC_LIFE:
    case OF_AURA_REVENGE:
    case OF_VORPAL2:
    case OF_DEC_MAGIC_MASTERY:
    case OF_DEC_SPELL_CAP:
    case OF_DEC_SPELL_POWER:
    case OF_SLAY_LIVING:
    case OF_STUN:
        return FALSE;
    }
    return TRUE;
}

static bool _add_essence(int which, int amount)
{
    int n = _essences[which];

    if (amount > 0)
    {
        n += amount;
        n = _bounds_check(n);

        if (n != _essences[which])
        {
            _essences[which] = n;
            return TRUE;
        }
    }

    return FALSE;
}

static bool _absorb(object_type *o_ptr)
{
    bool    result = FALSE;
    int     i;
    int     div = 1;
    vec_ptr v = NULL;
    u32b    flags[OF_ARRAY_SIZE];

    obj_flags(o_ptr, flags);

    if (o_ptr->curse_flags & OFC_AGGRAVATE)
        div++;
    if (o_ptr->curse_flags & (OFC_TY_CURSE | OFC_HEAVY_CURSE))
        div++;

    /* Object Flags */
    v = of_lookup_filter(_of_filter);
    for (i = 0; i < vec_length(v); i++)
    {
        of_info_ptr info = vec_get(v, i);
        if (have_flag(flags, info->id))
        {
            if (info->flags & OFT_PVAL)
            {
                if (_add_essence(info->id, o_ptr->pval/div))
                    result = TRUE;
            }
            else
            {
                _essences[info->id]++;
                if (info->id == OF_AURA_FIRE && !have_flag(flags, OF_LIGHT)) _essences[OF_LIGHT]++;
                result = TRUE;
            }
        }
    }
    vec_free(v);
    v = NULL;

    /* Special Bonuses for AC and Activations */
    if (_add_essence(_ESSENCE_AC, o_ptr->to_a/div))
        result = TRUE;

    if (obj_has_effect(o_ptr))
    {
        effect_t e = obj_get_effect(o_ptr);
        if (!_effects[e.type])
        {
            msg_format("You have gained the power of '%s'.", do_effect(&e, SPELL_NAME, 0));
        }
        _effects[e.type]++;
        result = TRUE;
    }

    if (result)
    {
        plr->update |= PU_BONUS;
        msg_print("You grow stronger!");
    }
    return result;
}

static bool _absorb_object(object_type *o_ptr)
{
    if (obj_is_jewelry(o_ptr))
    {
        char o_name[MAX_NLEN];
        object_desc(o_name, o_ptr, OD_NAME_ONLY);
        msg_format("You attempt to drain power from %s.", o_name);
        _absorb(o_ptr);
        return TRUE;
    }
    return FALSE;
}

static int _calc_amount(int amount, int power, int rep)
{
    int result = 0;
    int ct = 0;

    while (amount > 0)
    {
        if (amount >= power)
        {
            result++;
            ct++;
        }
        amount -= power;

        if (ct % rep == 0)
        {
            power *= 2;
            if (power > 1024)
                power = 1024;
        }
    }

    return result;
}

static int _calc_needed(int amount, int power, int rep)
{
    int result = 0;
    int ct = 0;
    /* It would be cleaner to loop until _calc_amount increased, but the
       power required increases exponentially! */
    for (;;)
    {
        if (amount >= power)
            ct++;
        result += power;
        amount -= power;
        if (amount < 0)
            break;
        if (ct % rep == 0)
        {
            power *= 2;
            if (power > 1024)
                power = 1024;
        }
    }

    return result;
}

static int _calc_stat_bonus(int flag)
{
    return _calc_amount(_essences[flag], 2, 1);
}

static void _add_stat_flag(int flag, u32b flgs[OF_ARRAY_SIZE])
{
    if (_calc_stat_bonus(flag))
        add_flag(flgs, flag);
}

static int _res_power(int which)
{
    switch (which)
    {
    case GF_ELEC:
        return 3;

    case GF_ACID:
    case GF_FIRE:
    case GF_COLD:
    case GF_CONF:
    case GF_FEAR:
    case GF_TIME:
    case GF_TELEPORT:
        return 2;

    case GF_SOUND:
    case GF_SHARDS:
    case GF_CHAOS:
        return 4;

    case GF_DISEN:
    case GF_POIS:
    case GF_LIGHT:
    case GF_DARK:
    case GF_NETHER:
    case GF_NEXUS:
        return 3;
    }

    return 2;
}
static int _calculate_cost(int which, int base)
{
    int result = base;
    int dec = 8 * _calc_amount(_essences[OF_DEC_MANA], 1, 1);
    dec += 5 * _calc_amount(_effects[which] - 1, 1, 1);
    
    if (dec > 60)
        dec = 60;

    if (dec)
    {
        result -= dec * base / 100;
        if (result < 1)
            result = 1;
    }
    return result;
}

/**********************************************************************
 * Birth and Evolution
 **********************************************************************/
static void _birth(void) 
{ 
    object_type forge;
    int i;

    for (i = 0; i < _MAX_ESSENCE; i++)
        _essences[i] = 0;

    for (i = 0; i < EFFECT_MAX; i++)
        _effects[i] = 0;

    plr_mon_race_set("=.mimic");

    object_prep(&forge, lookup_kind(TV_RING, 0));
    add_flag(forge.flags, OF_NO_REMOVE);
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.to_a = 10;
    effect_add(&forge, EFFECT_BOLT_MISSILE);
    forge.art_name = quark_add(art_get_name(&forge, 0));
    art_remember_name(quark_str(forge.art_name));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_AMULET, 0));
    forge.pval = 1;
    add_flag(forge.flags, OF_INT);
    add_flag(forge.flags, OF_CHR);
    effect_add(&forge, EFFECT_PHASE_DOOR);
    forge.art_name = quark_add(art_get_name(&forge, 0));
    art_remember_name(quark_str(forge.art_name));
    plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_SCROLL, SV_SCROLL_TELEPORT));
    forge.number = 10;
    plr_birth_obj(&forge);

    plr_birth_obj_aux(TV_STAFF, EFFECT_NOTHING, 1);
}

static bool _drain_essences(int div)
{
    bool result = FALSE;
    int i;

    for (i = 0; i < _MAX_ESSENCE; i++)
    {
        int n = _essences[i];
        
        if (!n) continue;
        if (i == OF_SPEED) continue;
        
        _essences[i] -= _essences[i] / div;
        if (_essences[i] < n)
            result = TRUE;
    }
    if (result)
        plr->update |= PU_BONUS;
    return result;
}

static void _gain_one_effect(int list[])
{
    effect_t e = {0};
    e.type = _random(list);
    if (!_effects[e.type])
        msg_format("You have gained the power of '%s'.", do_effect(&e, SPELL_NAME, 0));
    else
        msg_format("Your power of '%s' has grown stronger.", do_effect(&e, SPELL_NAME, 0));
    _effects[e.type]++;
}

static void _gain_level(int new_level) 
{
    switch (new_level)
    {
    case 10:
    {
        int choices[] = {EFFECT_LIGHT_AREA, EFFECT_DETECT_TRAPS, EFFECT_DETECT_MONSTERS, 
                         EFFECT_DETECT_OBJECTS, EFFECT_SATISFY_HUNGER, -1};
        _gain_one_effect(choices);
        break;
    }
    case 15:
    {
        int choices[] = {EFFECT_BOLT_COLD, EFFECT_BOLT_FIRE, EFFECT_BOLT_ACID, 
                         EFFECT_BOLT_ELEC, EFFECT_BOLT_POIS, -1};
        _gain_one_effect(choices);
        break;
    }
    case 25:
    {
        int choices[] = {EFFECT_BALL_COLD, EFFECT_BALL_FIRE, EFFECT_BALL_ACID, 
                         EFFECT_BALL_ELEC, EFFECT_BALL_POIS, -1};
        _gain_one_effect(choices);
        break;
    }
    case 35:
    {
        int choices[] = {EFFECT_BREATHE_COLD, EFFECT_BREATHE_FIRE, EFFECT_BREATHE_ACID, 
                         EFFECT_BREATHE_ELEC, EFFECT_BREATHE_POIS, -1};
        _gain_one_effect(choices);
        break;
    }
    case 45:
    {
        int choices[] = {EFFECT_BOLT_WATER, EFFECT_BOLT_MANA, EFFECT_BALL_LIGHT, 
                         EFFECT_BALL_DARK, EFFECT_BALL_CHAOS, EFFECT_BALL_WATER,
                         EFFECT_BALL_MANA, EFFECT_BREATHE_SOUND, EFFECT_BREATHE_SHARDS,
                         EFFECT_BREATHE_CHAOS, -1};
        _gain_one_effect(choices);
        break;
    }
    }
}

/**********************************************************************
 * Powers
 **********************************************************************/
static void _absorb_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Absorb Jewelry");
        break;
    case SPELL_DESC:
        var_set_string(res, "Destroys a single piece of jewelry, absorbing the essence of its power.");
        break;
    case SPELL_CAST:
    {
        obj_prompt_t prompt = {0};
        char o_name[MAX_NLEN];

        var_set_bool(res, FALSE);
        prompt.prompt = "Absorb which item?";
        prompt.error = "You have nothing to absorb.";
        prompt.filter = obj_is_jewelry;
        prompt.where[0] = INV_PACK;
        prompt.where[1] = INV_FLOOR;

        obj_prompt(&prompt);
        if (!prompt.obj) return;

        object_desc(o_name, prompt.obj, OD_NAME_ONLY);
        msg_format("You absorb the power of %s!", o_name);
        _absorb(prompt.obj);

        prompt.obj->number = 0;
        obj_release(prompt.obj, 0);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static bool _detect = FALSE;
static void _detect_obj(point_t pos, obj_ptr obj)
{
    int rng = DETECT_RAD_ALL;
    if (!obj_is_jewelry(obj)) return;
    if (plr_distance(pos) > rng) return;
    obj->marked |= OM_FOUND;
    plr->window |= PW_OBJECT_LIST;
    draw_pos(pos);
    _detect = TRUE;
}
static void _detect_pile(point_t pos, obj_ptr pile)
{
    obj_ptr obj;
    for (obj = pile; obj; obj = obj->next)
        _detect_obj(pos, obj);
}
static void _detect_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Jewelry");
        break;
    case SPELL_DESC:
        var_set_string(res, "Locate nearby jewelry, animated or not.");
        break;
    case SPELL_CAST:
    {
        _detect = FALSE;
        dun_iter_floor_obj(cave, _detect_pile);
        if (detect_monsters_string(DETECT_RAD_DEFAULT, "=\"")) _detect = TRUE;
        if (_detect) msg_print("You sense your kind.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _judge_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Identify Jewelry");
        break;
    case SPELL_DESC:
        var_set_string(res, "Identifies a piece of jewelry.");
        break;
    case SPELL_CAST:
        if (plr->lev >= 35)
            var_set_bool(res, identify_fully(obj_is_jewelry));
        else
            var_set_bool(res, ident_spell(obj_is_jewelry));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _glitter_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Glitter");
        break;
    case SPELL_DESC:
        var_set_string(res, "This spell makes yourself irresistible to potential ring bearers.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr->riding)
        {
            msg_print("You already have a ring bearer.");
            return;
        }
        set_action(ACTION_GLITTER);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static int _charm_power(void)
{
    return spell_power(plr->lev * 3 / 2 + plr->stat_ind[A_CHR] + 3);
}
static void _charm_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Charm Ring Bearer");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to dominate a single ring bearer.");
        break;
    default:
        direct_spell(cmd, res, GF_CHARM_RING_BEARER, _charm_power());
    }
}

static power_info _powers[] = 
{
    { A_INT, {  1,  0,  0, _absorb_spell } },
    { A_CHR, {  1,  0,  0, _glitter_spell } },
    { A_INT, {  5,  1, 30, _detect_spell } },
    { A_INT, { 10, 10, 50, _judge_spell } },
    { A_CHR, { 15, 10, 50, _charm_spell } },
    {    -1, { -1, -1, -1, NULL}}
};

static int _get_powers(spell_info* spells, int max) 
{
    return get_powers_aux(spells, max, _powers);
}

/**********************************************************************
 * Spells
 **********************************************************************/
static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "spell";
        me.which_stat = A_INT;
        me.encumbrance.max_wgt = 430;
        me.encumbrance.weapon_pct = 100;
        me.encumbrance.enc_wgt = 600;
        me.options = CASTER_ALLOW_DEC_MANA;
        init = TRUE;
    }
    return &me;
}

typedef struct {
    int     effect;
    int     level;
    int     cost;
    int     fail;
/*  TODO
    s16b    extra; */
} _spell_t, *_spell_ptr;

#define _MAX_PER_GROUP 30
typedef struct {
    cptr     name;
    char     key;
    int      color;
    _spell_t spells[_MAX_PER_GROUP];
} _group_t, *_group_ptr;

static _group_t _groups[] = {
    { "Offense: Bolt", '1', TERM_RED,
      { { EFFECT_BOLT_MISSILE,         1,   1, 30 }, 
        { EFFECT_BOLT_ELEC,           10,   3, 35 },
        { EFFECT_BOLT_POIS,           12,   5, 45 },
        { EFFECT_BOLT_COLD,           13,   6, 45 },
        { EFFECT_BOLT_ACID,           14,   8, 55 },
        { EFFECT_BOLT_FIRE,           15,   9, 55 },
        { EFFECT_BOLT_LIGHT,           17,  11, 55 },
        { EFFECT_BOLT_DARK,           19,  12, 60 },
        { EFFECT_BOLT_NETHER,         20,  12, 60 },
        { EFFECT_BOLT_NEXUS,          22,  15, 65 },
        { EFFECT_BOLT_CONF,           24,  17, 65 },
        { EFFECT_BOLT_SOUND,          25,  17, 65 },
        { EFFECT_BOLT_SHARDS,         26,  18, 65 },
        { EFFECT_BOLT_DISEN,          27,  18, 65 },
        { EFFECT_BOLT_TIME,           28,  19, 65 },
        { EFFECT_BOLT_ICE,            29,  20, 65 },
        { EFFECT_BOLT_CHAOS,          30,  20, 65 },
        { EFFECT_BOLT_WATER,          31,  20, 65 },
        { EFFECT_BOLT_PLASMA,         32,  21, 65 },
        { EFFECT_BOLT_MANA,           33,  25, 70 },
        { EFFECT_NONE } } },

    { "Offense: Ball", '2', TERM_RED,
      { { EFFECT_BALL_POIS,           10,   5, 35 },
        { EFFECT_BALL_ELEC,           19,  10, 45 },
        { EFFECT_BALL_COLD,           23,  12, 45 },
        { EFFECT_BALL_ACID,           25,  14, 55 },
        { EFFECT_BALL_FIRE,           27,  16, 55 },
        { EFFECT_BALL_NETHER,         30,  20, 60 },
        { EFFECT_BALL_NEXUS,          30,  22, 65 },
        { EFFECT_BALL_CONF,           30,  25, 65 },
        { EFFECT_BALL_SOUND,          31,  25, 65 },
        { EFFECT_BALL_SHARDS,         32,  27, 65 },
        { EFFECT_BALL_DISEN,          34,  27, 65 },
        { EFFECT_BALL_TIME,           34,  30, 65 },
        { EFFECT_BALL_LIGHT,           35,  35, 65 },
        { EFFECT_BALL_DARK,           36,  35, 65 },
        { EFFECT_BALL_CHAOS,          37,  35, 65 },
        { EFFECT_BALL_WATER,          38,  37, 70 },
        { EFFECT_BALL_MANA,           40,  45, 75 },
        { EFFECT_NONE } } },

    { "Offense: Breath", '3', TERM_RED,
      { { EFFECT_BREATHE_ELEC,        25,  30, 35 },
        { EFFECT_BREATHE_COLD,        26,  30, 45 },
        { EFFECT_BREATHE_ACID,        27,  35, 55 },
        { EFFECT_BREATHE_FIRE,        28,  35, 55 },
        { EFFECT_BREATHE_POIS,        30,  35, 45 },
        { EFFECT_BREATHE_LIGHT,        32,  35, 55 },
        { EFFECT_BREATHE_DARK,        33,  35, 60 },
        { EFFECT_BREATHE_NETHER,      35,  35, 60 },
        { EFFECT_BREATHE_NEXUS,       36,  40, 65 },
        { EFFECT_BREATHE_CONF,        37,  40, 65 },
        { EFFECT_BREATHE_DISEN,       38,  45, 65 },
        { EFFECT_BREATHE_TIME,        39,  45, 65 },
        { EFFECT_BREATHE_CHAOS,       40,  50, 70 },
        { EFFECT_BREATHE_SOUND,       41,  55, 70 },
        { EFFECT_BREATHE_SHARDS,      42,  55, 70 },
        { EFFECT_BREATHE_HOLY_FIRE,   43,  60, 70 },
        { EFFECT_NONE } } },

    { "Offense: Other", '4', TERM_RED,
      { { EFFECT_PESTICIDE,            5,   2, 35 },
        { EFFECT_DISPEL_MONSTERS,     10,   5, 40 }, /* Faramir for only 4 damage */
        { EFFECT_BEAM_ELEC,           15,   7, 40 },
        { EFFECT_BEAM_COLD,           17,   8, 45 },
        { EFFECT_BEAM_FIRE,           22,   9, 45 },
        { EFFECT_BEAM_ACID,           25,  10, 45 },
        { EFFECT_BEAM_SOUND,          27,  15, 50 },
        { EFFECT_DRAIN_LIFE,          30,  20, 50 },
        { EFFECT_ARROW,               30,  20, 50 },
        { EFFECT_DISPEL_UNDEAD,       32,  30, 50 }, 
        { EFFECT_DISPEL_DEMON,        32,  30, 50 },
        { EFFECT_DISPEL_GOOD,         33,  30, 55 }, 
        { EFFECT_DISPEL_EVIL,         35,  35, 60 }, 
        { EFFECT_DISPEL_LIFE,         35,  35, 60 }, 
        { EFFECT_CONFUSING_LIGHT,      37,  40, 60 },
        { EFFECT_HOLINESS,            40,  40, 60 },
        { EFFECT_DISPEL_EVIL_HERO,    40,  40, 60 },
        { EFFECT_ROCKET,              42,  50, 65 },
        { EFFECT_WRATH_OF_GOD,        43,  50, 70 },
        { EFFECT_STAR_BALL,           44,  55, 75 },
        { EFFECT_STARBURST,           45,  60, 75 },
        { EFFECT_MANA_STORM,          45,  60, 75 },
        { EFFECT_NONE } } },

    { "Buff", 'B', TERM_YELLOW,
      { { EFFECT_BLESS,                5,   3, 35 },
        { EFFECT_HEROISM,             12,   5, 45 },
        { EFFECT_TELEPATHY,           15,   7, 50 },
        { EFFECT_RESIST_ACID,         15,  10, 50 },
        { EFFECT_RESIST_ELEC,         15,  10, 50 },
        { EFFECT_RESIST_FIRE,         15,  10, 50 },
        { EFFECT_RESIST_COLD,         15,  10, 50 },
        { EFFECT_RESIST_POIS,         23,  15, 50 },
        { EFFECT_BERSERK,             25,  10, 55 }, 
        { EFFECT_STONE_SKIN,          25,  15, 60 },
        { EFFECT_SPEED,               27,  20, 65 }, 
        { EFFECT_RESISTANCE,          30,  25, 65 },
        { EFFECT_PROT_EVIL,           32,  25, 65 },
        { EFFECT_SPEED_HERO,          35,  30, 65 },
        { EFFECT_HOLY_GRAIL,          37,  30, 65 },
        { EFFECT_SPEED_HERO_BLESS,    40,  35, 70 },
        { EFFECT_WRAITHFORM,          47,  90, 90 },
        { EFFECT_INVULNERABILITY,     49, 100, 90 },
        { EFFECT_LIGHT_SPEED,         50, 100, 90 },
        { EFFECT_NONE } } },

    { "Healing/Recovery", 'H', TERM_YELLOW,
      { { EFFECT_CURE_POIS,           10,   7, 40 },
        { EFFECT_CURE_FEAR,           15,  10, 40 },
        { EFFECT_REMOVE_CURSE,        20,  15, 45 },
        { EFFECT_CLARITY,             25,   0, 80 }, /* ??! */
        { EFFECT_CURE_FEAR_POIS,      25,  20, 50 },
        { EFFECT_HEAL,                26,  20, 60 }, /* 50hp */
        { EFFECT_CURING,              27,  25, 55 },
        { EFFECT_HEAL_CURING,         32,  45, 60 }, /* 300hp */
        { EFFECT_RESTORE_EXP,         35,  40, 60 },
        { EFFECT_REMOVE_ALL_CURSE,    37,  50, 60 },
        { EFFECT_SACRED_KNIGHTS,      39,  50, 65 },
        { EFFECT_RESTORE_STATS,       40,  60, 70 },
        { EFFECT_RESTORING,           40,  60, 70 },
        { EFFECT_HEAL_CURING_HERO,    45,  65, 80 },
        { EFFECT_NONE } } },

    { "Detection/Knowledge", 'D', TERM_L_BLUE,
      { { EFFECT_LIGHT_AREA,            1,   2, 25 },
        { EFFECT_DETECT_TRAPS,         3,   4, 30 },
        { EFFECT_DETECT_GOLD,          4,   4, 30 },
        { EFFECT_DETECT_EVIL,          5,   5, 35 },
        { EFFECT_DETECT_MONSTERS,      7,   5, 35 },
        { EFFECT_DETECT_OBJECTS,       8,   7, 40 },
        { EFFECT_IDENTIFY,            15,  12, 55 },
        { EFFECT_LIGHT_MAP_AREA,       20,  10, 50 },
        { EFFECT_ENLIGHTENMENT,       20,  10, 50 },
        { EFFECT_DETECT_ALL,          25,  15, 60 },
        { EFFECT_PROBING,             27,  20, 60 },
        { EFFECT_SELF_KNOWLEDGE,      30,  23, 65 },
        { EFFECT_IDENTIFY_FULL,       33,  25, 65 },
        { EFFECT_LIST_UNIQUES,        40,  50, 75 },
        { EFFECT_LIST_ARTIFACTS,      42,  50, 75 }, 
        { EFFECT_CLAIRVOYANCE,        45,  50, 70 },
        { EFFECT_NONE } } },

    { "Teleportations", 'T', TERM_L_BLUE,
      { { EFFECT_PHASE_DOOR,           3,   3, 35 },
        { EFFECT_TELEPORT,            12,   7, 45 },
        { EFFECT_STRAFING,            15,   8, 55 },
        { EFFECT_TELEPORT_AWAY,       20,  15, 55 },
        { EFFECT_RECALL,              25,  20, 55 },
        { EFFECT_TELEKINESIS,         27,  22, 60 },
        { EFFECT_ESCAPE,              30,  25, 60 },
        { EFFECT_BANISH_EVIL,         37,  35, 65 },
        { EFFECT_BANISH_ALL,          40,  40, 70 },
        { EFFECT_DIMENSION_DOOR,      43,  50, 70 },
        { EFFECT_NONE } } },

    { "Utility", 'U', TERM_L_BLUE, 
      { { EFFECT_SATISFY_HUNGER,       5,   5, 35 },
        { EFFECT_STONE_TO_MUD,        15,  10, 50 },
        { EFFECT_DESTROY_TRAP,        20,  12, 50 },
        { EFFECT_DESTROY_TRAPS,       25,  15, 55 },
        { EFFECT_EARTHQUAKE,          25,  20, 60 },
        { EFFECT_RUNE_EXPLOSIVE,      29,  25, 65 },
        { EFFECT_ENCHANTMENT,         30,  80, 85 },
        { EFFECT_GENOCIDE_ONE,        33,  30, 65 },
        { EFFECT_RECHARGE_FROM_DEVICE,35,  30, 65 },
        { EFFECT_RUNE_PROTECTION,     37,  50, 70 },
        { EFFECT_RECHARGE_FROM_PLAYER,38,  30, 70 },
        { EFFECT_DESTRUCTION,         40,  35, 70 },
        { EFFECT_ALCHEMY,             43,  90, 70 },
        { EFFECT_GENOCIDE,            45,  55, 75 },
        { EFFECT_MASS_GENOCIDE,       47,  75, 85 },
        { EFFECT_NONE } } },

    { "Summoning", 'S', TERM_UMBER,
      { { EFFECT_SUMMON_ANTS,         20,  15, 45 },        
        { EFFECT_SUMMON_ELEMENTAL,    23,  20, 45 },
        { EFFECT_SUMMON_PHANTASMAL,   25,  25, 45 },
        { EFFECT_CHARM_ANIMAL,        27,  25, 45 },
        { EFFECT_SUMMON_MONSTERS,     30,  30, 50 },
        { EFFECT_CHARM_DEMON,         31,  30, 50 },
        { EFFECT_CHARM_UNDEAD,        32,  30, 50 },
        { EFFECT_SUMMON_HOUNDS,       33,  35, 50 },
        { EFFECT_SUMMON_HYDRAS,       35,  40, 50 },
        { EFFECT_SUMMON_DRAGON,       37,  45, 55 },
        { EFFECT_SUMMON_UNDEAD,       39,  50, 55 },
        { EFFECT_SUMMON_DEMON,        40,  55, 55 },
        { EFFECT_SUMMON_ANGEL,        45,  80, 65 },
        { EFFECT_SUMMON_CYBERDEMON,   50, 100, 75 },
        { EFFECT_NONE } } },

    { "Other", 'O', TERM_UMBER,
      { { EFFECT_AGGRAVATE,           10,   3, 35 },
        { EFFECT_POLY_SELF,           20,  10, 40 },
        { EFFECT_SCARE_MONSTERS,      22,  15, 45 },
        { EFFECT_CHARM_MONSTER,       23,  17, 45 },
        { EFFECT_SLEEP_MONSTERS,      25,  20, 45 },
        { EFFECT_SLOW_MONSTERS,       25,  20, 45 },
        { EFFECT_CONFUSE_MONSTERS,    27,  25, 50 },
        { EFFECT_ANIMATE_DEAD,        30,  30, 50 },
        { EFFECT_STASIS_MONSTERS,     40,  50, 70 },
        { EFFECT_NONE } } },

    { NULL }
};

static int _boost(int effect)
{
    int result = 0;
    int ct = _effects[effect] - 1;

    while (ct > 0)
    {
        result += 5;
        ct /= 2;
    }
    result += spell_power(100) - 100;
    return result;
}

int _prorata_level_aux(int amt, int lvl, int w1, int w2, int w3)
{
    int result = 0;
    int wt = w1 + w2 + w3;

    /* assume out of 50 max lvl */
    if (lvl == 50)
        return amt; /* make sure the player gets the entire amt */

    result += amt * lvl * w1 / (50*wt);
    result += amt * lvl * lvl * w2 / (50*50*wt);
    result += (amt * lvl * lvl / 50) * lvl * w3 / (50*50*wt); /* 2^31/50^3 is about 17000 */

    return result;
}


static int _power(_spell_ptr spell)
{
    /* cf plr_prorata_level.ods ... plr_prorata_level_aux() assumes the player's level.
     * Historically, absorbed ring powers have always used the spell's level, but this
     * is unsatisfactory for high end spells (and depends on implementation in devices.c;
     * for example, Rockets are fine with Power 42 but Starburst is lame with power 45). */
    return spell->level + _prorata_level_aux(30, spell->level, 0, 0, 1);
}

static effect_t _effect(_spell_ptr spell)
{
    effect_t effect = {0};

    effect.type = spell->effect;
    effect.power = _power(spell);
    effect.difficulty = spell->level;

    return effect;
}

static int _groups_count(void)
{
    int result = 0;
    int i;
    for (i = 0; ; i++)
    {
        if (!_groups[i].name) break;
        result++;
    }
    return result;
}
static int _spells_count(_spell_ptr spells)
{
    int result = 0;
    int i;
    for (i = 0; ; i++)
    {
        if (spells[i].effect == EFFECT_NONE) break;
        result++;
    }
    return result;
}
static int _spells_count_allowed(_spell_ptr spells)
{
    int result = 0;
    int i;
    for (i = 0; ; i++)
    {
        if (spells[i].effect == EFFECT_NONE) break;
        if (_effects[spells[i].effect])
            result++;
    }
    return result;
}

/* The following will help me locate effects missing
 * from the spell tables above. This happens when I
 * add new allowable activations to jewelry. */
static _spell_ptr _find_spell(int effect)
{
    int i, j;
    for (i = 0; ; i++)
    {
        _group_ptr g = &_groups[i];
        if (!g->name) break;
        for (j = 0; ; j++)
        {
            _spell_ptr s = &g->spells[j];
            if (s->effect == EFFECT_NONE) break;
            if (s->effect == effect) return s;
        }
    }
    return NULL;
}
static vec_ptr _missing_effects(void)
{
    vec_ptr v = vec_alloc(NULL);
    int     i;

    for (i = 0; i < EFFECT_MAX; i++)
    {
        if (_effects[i] > 0 && !_find_spell(i))
            vec_add_int(v, i);
    }
    return v;
}

/* Menu Code 1: Choose which group of magic to use */
static cptr _group_choice = NULL;

static void _group_menu_fn(int cmd, int which, vptr cookie, var_ptr res)
{
    switch (cmd)
    {
    case MENU_KEY:
        var_set_int(res, _groups[which].key);
        break;
    case MENU_TEXT:
        var_set_string(res, format("%s", _groups[which].name));
        break;
    case MENU_COLOR:
        if (!_spells_count_allowed(_groups[which].spells))
            var_set_int(res, TERM_L_DARK);
        else
            var_set_int(res, _groups[which].color);
        break;
    default:
        default_menu(cmd, which, cookie, res);
    }
}

static _group_ptr _prompt_group(void)
{
    int        idx = -1;
    menu_t     menu = { "Use which type of spell?", NULL, NULL,
                        _group_menu_fn, NULL, _groups_count()};

    idx = menu_choose(&menu);
    if (idx < 0) return NULL;
    _group_choice = _groups[idx].name;
    return &_groups[idx];
}

/* Menu Code 2: Choose which spell to use */
static void _spell_menu_fn(int cmd, int which, vptr cookie, var_ptr res)
{
    _spell_ptr ss = (_spell_ptr)cookie;
    _spell_ptr s = ss + which;

    switch (cmd)
    {
    case MENU_TEXT:
    {
        char     buf[1024];
        char     name[255];
        char     info[255];
        effect_t effect = _effect(s);

        sprintf(name, "%s", do_effect(&effect, SPELL_NAME, 0));
        sprintf(info, "%s", do_effect(&effect, SPELL_INFO, _boost(s->effect)));

        sprintf(buf, "%-30.30s %3d %3d %3d%% ", name, s->level, s->cost, s->fail);
        strcat(buf, info);
        var_set_string(res, buf);
        break;            
    }
    case MENU_HELP:
    {
        effect_t effect = _effect(s);
        var_set_string(res, do_effect(&effect, SPELL_DESC, 0));
        break;
    }
    case MENU_COLOR:
        if (s->level > plr->lev)
            var_set_int(res, TERM_L_DARK);
        else if (s->cost > plr->csp)
            var_set_int(res, TERM_L_DARK);
        else
            var_set_int(res, TERM_WHITE);
        break;
    default:
        default_menu(cmd, which, cookie, res);
    }
}
static _spell_t _prompt_spell(_spell_ptr spells)
{
    _spell_t result = {0};
    _spell_t choices[_MAX_PER_GROUP];
    int      ct_total = _spells_count(spells);
    int      ct_avail = 0;
    int      i;

    for (i = 0; i < ct_total; i++)
    {
        _spell_ptr spell = &spells[i];
        
        if (_effects[spell->effect])
        {
            _spell_ptr choice = &choices[ct_avail];
            
            choice->effect = spell->effect;
            choice->level = spell->level;
            choice->cost = _calculate_cost(spell->effect, spell->cost);
            choice->fail = calculate_fail_rate(spell->level, spell->fail, plr->stat_ind[A_INT]);

            ct_avail++;
        }
    }

    if (!ct_avail)
    {
        msg_print("You haven't absorbed any of these effects yet.");
    }
    else
    {
        int    idx = -1;
        char   heading[255], prompt1[255], prompt2[255];
        menu_t menu = { prompt1, prompt2, heading,
                        _spell_menu_fn, choices, ct_avail};

        sprintf(prompt1, "Use which type of %s?", _group_choice);
        sprintf(prompt2, "Browse which type of %s?", _group_choice);
        sprintf(heading, "%-30.30s Lvl Cst Fail Info", "");
        idx = menu_choose(&menu);
        if (idx >= 0)
            result = choices[idx];
    }
    return result;
}

/* Menu Code: Putting it all together */
static _spell_t _prompt(void)
{
    _group_ptr group;
    _spell_t   result = {0};

    for (;;)
    {
        group = _prompt_group();
        if (!group)
            break;
        result = _prompt_spell(group->spells);
        if (result.effect != EFFECT_NONE)
            break;
    }
    return result;
}

void ring_cast(void)
{
    _spell_t spell = {0};

    if (plr_tim_find(T_CONFUSED))
    {
        msg_print("You are too confused!");
        return;
    }

    spell = _prompt();
    if (spell.effect == EFFECT_NONE)
        return;

    if (spell.level > plr->lev)
    {
        msg_print("You can't use that spell yet!");
        return;
    }
    if (spell.cost > plr->csp)
    {
        msg_print("You do not have enough mana to use this power.");
        return;
    }

    plr->csp -= spell.cost;

    if (randint0(100) < spell.fail)
    {
        if (flush_failure) flush();
        msg_format("You failed to get the spell off!");
        sound(SOUND_FAIL);
    }
    else
    {
        effect_t effect = _effect(&spell);
        device_known = TRUE; /* Hack */
        if (!do_effect(&effect, SPELL_CAST, _boost(spell.effect)))
        {
            plr->csp += spell.cost;
        }
    }
    energy_use = 100;
    plr->redraw |= PR_MANA;
    plr->redraw |= PR_HP;
    plr->window |= PW_SPELL;
}

static void _browse(void)
{
    _group_ptr group;
    _spell_t   spell = {0};
    effect_t   effect = {0};
    int i, ct, line;
    char desc[1024];
    char tmp[62*10];

    for (;;)
    {
        group = _prompt_group();
        if (!group) break;
        ct = _spells_count_allowed(group->spells);
        screen_save();
        for (;;)
        {
            spell = _prompt_spell(group->spells);
            if (spell.effect == EFFECT_NONE) break;
            for (i = 0; i < 7; i++)
                Term_erase(13, ct + i + 2, 255);

            effect = _effect(&spell);
            sprintf(desc, "%s", do_effect(&effect, SPELL_DESC, 0));
            roff_to_buf(desc, 62, tmp, sizeof(tmp));

            for(i = 0, line = ct + 3; tmp[i]; i += 1+strlen(&tmp[i]))
            {
                prt(&tmp[i], line, 15);
                line++;
            }
        }
        screen_load();            
    }
}

void ring_browse(void)
{
    _browse();
}

/**********************************************************************
 * Bonuses and Display Flags
 **********************************************************************/
static void _calc_bonuses(void) 
{
    int i;
    int l = plr->lev;
    int to_a = l;

    plr->skill_dig += 30;

    to_a += _calc_amount(_essences[_ESSENCE_AC], 1, 15);
    plr->to_a += to_a;
    plr->dis_to_a += to_a;

    res_add_vuln(GF_ELEC);
    plr->no_cut = TRUE;

    /* Speed rings come very late, and very unreliably ... */
    plr->pspeed += plr->lev / 10;

    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
    {
        int j = OF_RES_(i);
        int n = _calc_amount(_essences[j], _res_power(i), 1);

        for (; n; --n)
            res_add(i);
    }
    if (_essences[OF_IM_(GF_ACID)] >= 2)
        res_add_immune(GF_ACID);
    if (_essences[OF_IM_(GF_ELEC)] >= 2)
        res_add_immune(GF_ELEC);
    if (_essences[OF_IM_(GF_FIRE)] >= 2)
        res_add_immune(GF_FIRE);
    if (_essences[OF_IM_(GF_COLD)] >= 2)
        res_add_immune(GF_COLD);

    plr->life += 3*_calc_amount(_essences[OF_LIFE], 7, 1);

    plr->skills.stl += _calc_amount(_essences[OF_STEALTH], 2, 1);
    plr->pspeed += _calc_amount(_essences[OF_SPEED], 1, 5);
    plr->skills.dev += 8*_calc_amount(_essences[OF_MAGIC_MASTERY], 2, 1);
    plr->device_power += _calc_amount(_essences[OF_DEVICE_POWER], 2, 1);
    plr->spell_power += _calc_amount(_essences[OF_SPELL_POWER], 2, 1);
    plr->spell_cap += _calc_amount(_essences[OF_SPELL_CAP], 2, 1);
    plr->skills.srh += 5*_calc_amount(_essences[OF_SEARCH], 2, 1);
    plr->skills.fos += 5*_calc_amount(_essences[OF_SEARCH], 2, 1);
    plr->see_infra += _calc_amount(_essences[OF_INFRA], 2, 1);

    if (_essences[OF_DEC_MANA] >= 7)
        plr->dec_mana++;
    if (_essences[OF_EASY_SPELL] >= 7)
        plr->easy_spell++;

    if (_essences[OF_SUST_STR] >= 5)
        plr->sustain_str = TRUE;
    if (_essences[OF_SUST_INT] >= 5)
        plr->sustain_int = TRUE;
    if (_essences[OF_SUST_WIS] >= 5)
        plr->sustain_wis = TRUE;
    if (_essences[OF_SUST_DEX] >= 5)
        plr->sustain_dex = TRUE;
    if (_essences[OF_SUST_CON] >= 5)
        plr->sustain_con = TRUE;
    if (_essences[OF_SUST_CHR] >= 5)
        plr->sustain_chr = TRUE;

    if (_essences[OF_TELEPATHY] >= 2)
        plr->telepathy = TRUE;
    if (_essences[OF_ESP_ANIMAL] >= 2)
        plr->esp_animal = TRUE;
    if (_essences[OF_ESP_UNDEAD] >= 2)
        plr->esp_undead = TRUE;
    if (_essences[OF_ESP_DEMON] >= 2)
        plr->esp_demon = TRUE;
    if (_essences[OF_ESP_ORC] >= 2)
        plr->esp_orc = TRUE;
    if (_essences[OF_ESP_TROLL] >= 2)
        plr->esp_troll = TRUE;
    if (_essences[OF_ESP_GIANT] >= 2)
        plr->esp_giant = TRUE;
    if (_essences[OF_ESP_DRAGON] >= 2)
        plr->esp_dragon = TRUE;
    if (_essences[OF_ESP_HUMAN] >= 2)
        plr->esp_human = TRUE;
    if (_essences[OF_ESP_EVIL] >= 2)
        plr->esp_evil = TRUE;
    if (_essences[OF_ESP_GOOD] >= 2)
        plr->esp_good = TRUE;
    if (_essences[OF_ESP_NONLIVING] >= 2)
        plr->esp_nonliving = TRUE;
    if (_essences[OF_ESP_UNIQUE] >= 2)
        plr->esp_unique = TRUE;

    if (_essences[OF_LEVITATION] >= 2)
        plr->levitation = TRUE;

    plr->free_act += _calc_amount(_essences[OF_FREE_ACT], 2, 1);
    plr->see_inv += _calc_amount(_essences[OF_SEE_INVIS], 3, 1);
    plr->hold_life += _calc_amount(_essences[OF_HOLD_LIFE], 3, 1);
    plr->regen += 5 * _calc_amount(_essences[OF_REGEN], 2, 10);

    if (_essences[OF_SLOW_DIGEST] >= 2)
        plr->slow_digest = TRUE;
    if (_essences[OF_REFLECT] >= 7)
        plr->reflect = TRUE;

    if (_essences[OF_AURA_FIRE] >= 7)
        plr->sh_fire = TRUE;
    if (_essences[OF_AURA_ELEC] >= 7)
        plr->sh_elec = TRUE;
    if (_essences[OF_AURA_COLD] >= 7)
        plr->sh_cold = TRUE;
}

static void _calc_stats(s16b stats[MAX_STATS])
{
    int i;
    for (i = 0; i < 6; i++)
        stats[i] += _calc_stat_bonus(OF_STR + i);
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE]) 
{
    int i;

    add_flag(flgs, OF_LIGHT);
    add_flag(flgs, OF_VULN_(GF_ELEC));

    for (i = 0; i < 6; i++) /* Assume in order */
    {
        if (i != 0) /* Bug: Giving TR_STR marks the player as cursed ... */
            _add_stat_flag(OF_STR + i, flgs);
        if (_essences[OF_SUST_STR + i] >= 5)
            add_flag(flgs, OF_SUST_STR + i);
    }

    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
    {
        int j = OF_RES_(i);
        int n = _calc_amount(_essences[j], _res_power(i), 1);

        if (n)
            add_flag(flgs, j);
    }

    if (_essences[OF_DEC_MANA] >= 7)
        add_flag(flgs, OF_DEC_MANA);
    if (_essences[OF_EASY_SPELL] >= 7)
        add_flag(flgs, OF_EASY_SPELL);

    if (plr->lev >= 10 || _calc_amount(_essences[OF_SPEED], 1, 5))
        add_flag(flgs, OF_SPEED);
    if (_calc_amount(_essences[OF_STEALTH], 2, 1))
        add_flag(flgs, OF_STEALTH);
    if (_calc_amount(_essences[OF_MAGIC_MASTERY], 2, 1))
        add_flag(flgs, OF_MAGIC_MASTERY);
    if (_calc_amount(_essences[OF_SEARCH], 2, 1))
        add_flag(flgs, OF_SEARCH);
    if (_calc_amount(_essences[OF_INFRA], 2, 1))
        add_flag(flgs, OF_INFRA);
    if (_calc_amount(_essences[OF_SPELL_POWER], 2, 1))
        add_flag(flgs, OF_SPELL_POWER);
    if (_calc_amount(_essences[OF_SPELL_CAP], 2, 1))
        add_flag(flgs, OF_SPELL_CAP);

    if (_essences[OF_TELEPATHY] >= 2)
        add_flag(flgs, OF_TELEPATHY);

    if (_essences[OF_ESP_EVIL] >= 2)
        add_flag(flgs, OF_ESP_EVIL);
    if (_essences[OF_ESP_GOOD] >= 2)
        add_flag(flgs, OF_ESP_GOOD);
    if (_essences[OF_ESP_NONLIVING] >= 2)
        add_flag(flgs, OF_ESP_NONLIVING);
    if (_essences[OF_ESP_UNIQUE] >= 2)
        add_flag(flgs, OF_ESP_UNIQUE);
    if (_essences[OF_ESP_DRAGON] >= 2)
        add_flag(flgs, OF_ESP_DRAGON);
    if (_essences[OF_ESP_DEMON] >= 2)
        add_flag(flgs, OF_ESP_DEMON);
    if (_essences[OF_ESP_UNDEAD] >= 2)
        add_flag(flgs, OF_ESP_UNDEAD);
    if (_essences[OF_ESP_ANIMAL] >= 2)
        add_flag(flgs, OF_ESP_ANIMAL);
    if (_essences[OF_ESP_HUMAN] >= 2)
        add_flag(flgs, OF_ESP_HUMAN);
    if (_essences[OF_ESP_ORC] >= 2)
        add_flag(flgs, OF_ESP_ORC);
    if (_essences[OF_ESP_TROLL] >= 2)
        add_flag(flgs, OF_ESP_TROLL);
    if (_essences[OF_ESP_GIANT] >= 2)
        add_flag(flgs, OF_ESP_GIANT);

    if (_essences[OF_LEVITATION] >= 2)
        add_flag(flgs, OF_LEVITATION);

    if (_calc_amount(_essences[OF_FREE_ACT], 2, 1))
        add_flag(flgs, OF_FREE_ACT);
    if (_calc_amount(_essences[OF_SEE_INVIS], 3, 1))
        add_flag(flgs, OF_SEE_INVIS);
    if (_calc_amount(_essences[OF_HOLD_LIFE], 3, 1))
        add_flag(flgs, OF_HOLD_LIFE);
    if (_calc_amount(_essences[OF_REGEN], 2, 10))
        add_flag(flgs, OF_REGEN);

    if (_essences[OF_SLOW_DIGEST] >= 2)
        add_flag(flgs, OF_SLOW_DIGEST);
    if (_essences[OF_REFLECT] >= 7)
        add_flag(flgs, OF_REFLECT);

    if (_essences[OF_AURA_FIRE] >= 7)
        add_flag(flgs, OF_AURA_FIRE);
    if (_essences[OF_AURA_ELEC] >= 7)
        add_flag(flgs, OF_AURA_ELEC);
    if (_essences[OF_AURA_COLD] >= 7)
        add_flag(flgs, OF_AURA_COLD);

    if (_essences[OF_IM_(GF_ACID)] >= 2)
        add_flag(flgs, OF_IM_(GF_ACID));
    if (_essences[OF_IM_(GF_ELEC)] >= 2)
        add_flag(flgs, OF_IM_(GF_ELEC));
    if (_essences[OF_IM_(GF_FIRE)] >= 2)
        add_flag(flgs, OF_IM_(GF_FIRE));
    if (_essences[OF_IM_(GF_COLD)] >= 2)
        add_flag(flgs, OF_IM_(GF_COLD));
}

/**********************************************************************
 * Character Dump
 **********************************************************************/
static void _dump_ability_flag(doc_ptr doc, int which, int threshold, cptr name)
{
    int n = _essences[which];
    if (n > 0)
    {
        doc_printf(doc, "   %-22.22s %5d %5d %5.5s\n",
            name,
            n, 
            threshold,
            n >= threshold ? "Y" : ""
        );
    }
}
static void _dump_bonus_flag(doc_ptr doc, int which, int power, int rep, cptr name)
{
    int n = _essences[which];
    if (n > 0)
    {
        doc_printf(doc, "   %-22.22s %5d %5d %+5d\n",
            name,
            n, 
            _calc_needed(n, power, rep),
            _calc_amount(n, power, rep)
        );
    }
}

static void _dump_missing_effects(doc_ptr doc)
{
    vec_ptr v = _missing_effects();

    if (vec_length(v))
    {
        int i;
        doc_insert(doc, "\n   <color:r>Missing Effects</color>\n");
        for (i = 0; i < vec_length(v); i++)
        {
            effect_t effect = {0};
            effect.type = vec_get_int(v, i);
            doc_printf(doc, "   %s\n", do_effect(&effect, SPELL_NAME, 0));
        }
    }
    vec_free(v);
}
static void _dump_effects(doc_ptr doc)
{
    int i, j;
    for (i = 0; ; i++)
    {
        _group_ptr g = &_groups[i];
        int ct = 0;
        if (!g->name) break;
        for (j = 0; ;j++)
        {
            _spell_ptr s = g->spells + j;
            if (s->effect == EFFECT_NONE) break;
            if (_effects[s->effect] || plr->wizard)
            {
                char     buf[1024];
                char     name[255];
                char     info[255];
                effect_t effect = _effect(s);

                sprintf(name, "%s", do_effect(&effect, SPELL_NAME, 0));
                sprintf(info, "%s", do_effect(&effect, SPELL_INFO, _boost(s->effect)));

                sprintf(buf, "<color:%c>%-30.30s %3d %3d %3d %3d%% %s</color>",
                              _effects[s->effect] ? 'w' : 'D',
                              name, _effects[s->effect], s->level, _calculate_cost(s->effect, s->cost), 
                              calculate_fail_rate(s->level, s->fail, plr->stat_ind[A_INT]), info);

                if (!ct)
                {
                    doc_printf(doc, "\n   <color:G>%-30.30s  Ct Lvl Cst Fail Info</color>\n", g->name);
                }
                doc_printf(doc, "   %s\n", buf);
                ct++;
            }
        }
    }
    if (plr->wizard)
        _dump_missing_effects(doc);
}

static void _character_dump(doc_ptr doc)
{
    int i;
    doc_printf(doc, "<topic:Essences>=================================== <color:keypress>E</color>ssences ==================================\n\n");
    doc_printf(doc, "   <color:G>%-22.22s Total  Need Bonus</color>\n", "Stats");
    for (i = 0; i < 6; i++) /* Assume in order */
        _dump_bonus_flag(doc, OF_STR + i, 2, 1, stat_name_true[A_STR + i]);

    doc_printf(doc, "\n   <color:G>%-22.22s Total  Need Bonus</color>\n", "Skills");
    _dump_bonus_flag(doc, _ESSENCE_AC, 1, 15, "To AC");
    _dump_bonus_flag(doc, OF_STEALTH, 2, 1, "Stealth");
    _dump_bonus_flag(doc, OF_SPEED, 1, 5, "Speed");
    _dump_bonus_flag(doc, OF_LIFE, 7, 1, "Life");
    _dump_bonus_flag(doc, OF_SEARCH, 2, 1, "Searching");
    _dump_bonus_flag(doc, OF_INFRA, 2, 1, "Infravision");
    _dump_bonus_flag(doc, OF_TUNNEL, 2, 1, "Digging");
    _dump_bonus_flag(doc, OF_LIGHT, 1, 1, "Light");
    _dump_bonus_flag(doc, OF_MAGIC_MASTERY, 2, 1, "Magic Mastery");
    _dump_bonus_flag(doc, OF_DEVICE_POWER, 2, 1, "Device Power");
    _dump_bonus_flag(doc, OF_SPELL_POWER, 2, 1, "Spell Power");
    _dump_bonus_flag(doc, OF_SPELL_CAP, 2, 1, "Spell Capacity");
 
    doc_printf(doc, "\n   <color:G>%-22.22s Total  Need Bonus</color>\n", "Resistances");
    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
        _dump_bonus_flag(doc, OF_RES_(i), _res_power(i), 1, format("%^s", res_name(i)));

    _dump_ability_flag(doc, OF_IM_(GF_ACID), 2, "Immune Acid");
    _dump_ability_flag(doc, OF_IM_(GF_ELEC), 2, "Immune Elec");
    _dump_ability_flag(doc, OF_IM_(GF_FIRE), 2, "Immune Fire");
    _dump_ability_flag(doc, OF_IM_(GF_COLD), 2, "Immune Cold");

    doc_printf(doc, "\n   <color:G>%-22.22s Total  Need Bonus</color>\n", "Abilities");
    _dump_bonus_flag(doc, OF_FREE_ACT, 2, 1, "Free Action");
    _dump_bonus_flag(doc, OF_SEE_INVIS, 3, 1, "See Invisible");
    _dump_bonus_flag(doc, OF_HOLD_LIFE, 3, 1, "Hold Life");
    _dump_ability_flag(doc, OF_LEVITATION, 2, "Levitation");
    _dump_ability_flag(doc, OF_SLOW_DIGEST, 2, "Slow Digestion");
    {
        int n = _essences[OF_REGEN];
        if (n > 0)
        {
            doc_printf(doc, "   %-22.22s %5d %5d %+5d%%\n",
                "Regeneration",
                n, 
                _calc_needed(n, 2, 10),
                5*_calc_amount(n, 2, 10)
            );
        }
    }
    _dump_bonus_flag(doc, OF_DEC_MANA, 1, 1, "Economical Mana");
    _dump_ability_flag(doc, OF_EASY_SPELL, 7, "Wizardry");
    _dump_ability_flag(doc, OF_REFLECT, 7, "Reflection");
    _dump_ability_flag(doc, OF_AURA_FIRE, 7, "Aura Fire");
    _dump_ability_flag(doc, OF_AURA_ELEC, 7, "Aura Elec");
    _dump_ability_flag(doc, OF_AURA_COLD, 7, "Aura Cold");
    for (i = 0; i < 6; i++) /* Assume in order */
        _dump_ability_flag(doc, OF_SUST_STR + i, 5, format("Sustain %s", stat_name_true[A_STR + i]));

    doc_printf(doc, "\n   <color:G>%-22.22s Total  Need Bonus</color>\n", "ESP");
    _dump_ability_flag(doc, OF_TELEPATHY, 2, "Telepathy");
    _dump_ability_flag(doc, OF_ESP_ANIMAL, 2, "ESP Animals");
    _dump_ability_flag(doc, OF_ESP_UNDEAD, 2, "ESP Undead");
    _dump_ability_flag(doc, OF_ESP_DEMON, 2, "ESP Demon");
    _dump_ability_flag(doc, OF_ESP_ORC, 2, "ESP Orc");
    _dump_ability_flag(doc, OF_ESP_TROLL, 2, "ESP Troll");
    _dump_ability_flag(doc, OF_ESP_GIANT, 2, "ESP Giant");
    _dump_ability_flag(doc, OF_ESP_DRAGON, 2, "ESP Dragon");
    _dump_ability_flag(doc, OF_ESP_HUMAN, 2, "ESP Human");
    _dump_ability_flag(doc, OF_ESP_EVIL, 2, "ESP Evil");
    _dump_ability_flag(doc, OF_ESP_GOOD, 2, "ESP Good");
    _dump_ability_flag(doc, OF_ESP_NONLIVING, 2, "ESP Nonliving");
    _dump_ability_flag(doc, OF_ESP_UNIQUE, 2, "ESP Unique");

    doc_printf(doc, "\n<topic:Spells>==================================== <color:keypress>S</color>pells ===================================\n");
    _dump_effects(doc);

    doc_newline(doc);
}

/**********************************************************************
 * Public
 **********************************************************************/
plr_race_ptr mon_ring_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  40,  38,   5,  25,  20,  34,  20};
    skills_t xs = { 35,  75,  55,   0,   0,   0,  30,  35};

        me = plr_race_alloc(RACE_MON_RING);
        me->skills = bs;
        me->extra_skills = xs;

        me->name = "Ring";
        me->desc = "Rings are sentient creatures animated by magical means.\n \n"
                    "Rings cannot wear normal equipment. Rather, they simply are a ring, "
                    "magically enchanted. They gain resistances, attributes and spells "
                    "by absorbing the magic from jewelry they find (rings or amulets).\n \n"
                    "Rings also have a small arsenal of innate spells. Since the ring "
                    "is unable to move on its own it will need to lure a nearby monster "
                    "into wearing it. Magic rings tend to corrupt their owners, and over time, "
                    "the ring-bearer will find that his will is not his own!\n \n"
                    "Rings have no physical attacks. Instead, they may blast enemies with mighty "
                    "spells (once learned, of course). Or, they may simply lie back and let their "
                    "current bearer do the fighting for them.";

        me->stats[A_STR] = -3;
        me->stats[A_INT] =  4;
        me->stats[A_WIS] =  1;
        me->stats[A_DEX] =  0;
        me->stats[A_CON] = -1;
        me->stats[A_CHR] =  3;

        me->life = 85;
        me->infra = 5;
        me->exp = 150;
        me->base_hp = 10;
        me->shop_adjust = 110; /* Really should depend on current bearer */

        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_stats = _calc_stats;

        me->hooks.get_powers = _get_powers;
        me->hooks.caster_info = _caster_info;

        me->hooks.character_dump = _character_dump;
        me->hooks.get_flags = _get_flags;
        me->hooks.gain_level = _gain_level;
        me->hooks.birth = _birth;

        me->hooks.load_player = _load;
        me->hooks.save_player = _save;
        me->hooks.destroy_object = _absorb_object;

        me->flags = RACE_IS_MONSTER | RACE_IS_NONLIVING;
        me->pseudo_class_id = CLASS_MAGE;
    }

    me->subname = NULL;
    if (plr->riding && !plr->is_dead && character_loaded)
    {
        mon_race_ptr race = plr_riding_race();
        if (race)
            me->subname = race->name;
    }

    me->equip_template = plr_equip_template();
    return me;
}

int ring_calc_torch(void)
{
    return 1 + _calc_amount(_essences[OF_LIGHT], 1, 1);
}

bool ring_disenchant(void)
{
    bool result = FALSE;
    if (!res_save(GF_DISEN, 44) && _drain_essences(20))
    {
        msg_print("You feel power draining from your body!");
        result = TRUE;
    }
    return result;
}

static int _plev(void)
{
    if (plr->lev <= 40)
        return plr->lev;

    return 40 + (plr->lev - 40)*2;
}

static int _r_level(monster_race *r_ptr)
{
    int ml = r_ptr->alloc.lvl;
    
    if (mon_race_is_unique(r_ptr))
        ml += ml/5;

    return ml;
}

static bool _mon_save_p(monster_type *m_ptr)
{
    int           pl = _plev();
    monster_race *r_ptr = m_ptr->race;
    int           ml = _r_level(r_ptr);
    bool          result = FALSE;

    if (m_ptr->mflag2 & MFLAG2_QUESTOR)
        return TRUE;
    
    /* Player may not exert their force of will out of sight! */
    if (plr_project_mon(m_ptr))
        pl += adj_stat_save_fear[plr->stat_ind[A_CHR]];

    if (pl <= 1) 
        return TRUE;

    if (randint1(pl) <= randint1(ml)) 
        result = TRUE;

    return result;
}

bool ring_dominate_m(mon_ptr mon)
{
    if ( plr->prace == RACE_MON_RING 
      && !plr->riding
      && !is_aware(mon) 
      && mon_is_type(mon->race, SUMMON_RING_BEARER) )
    {
        char m_name[MAX_NLEN];
        monster_desc(m_name, mon, 0);
        if (_mon_save_p(mon))
        {
            msg_format("%^s sees you for what you truly are!", m_name);
            mon->mflag2 |= MFLAG2_AWARE;
        }
        else
        {
            /* Pick the pretty up! */
            set_pet(mon);

            switch (randint1(5))
            {
            case 1: msg_format("%^s says, 'My Precious!'", m_name); break;
            case 2: msg_format("%^s says, 'Look! A Pretty!'", m_name); break;
            case 3: msg_format("%^s says, 'Hark! What have we here?'", m_name); break;
            case 4: msg_format("%^s says, 'Finally! A lucky drop at last!'", m_name); break;
            case 5: msg_format("%^s says, 'This better be a Ring of Speed!'", m_name); break;
            }
            msg_format("%^s picks you up.", m_name);
            plr->riding = mon->id;
            if (plr->riding == plr->health_who) health_track(NULL);
            set_action(ACTION_NONE);

            plr->update |= PU_UN_VIEW | PU_UN_LIGHT;
            plr->update |= PU_BONUS;
            plr->redraw |= PR_MAP | PR_EXTRA;
            plr->redraw |= PR_HEALTH_BARS;
            move_player_effect(mon->pos, MPE_HANDLE_STUFF | MPE_ENERGY_USE | MPE_DONT_PICKUP | MPE_DONT_SWAP_MON);
        
            return TRUE;
        }
    }
    return FALSE;
}

void ring_process_m(mon_ptr mon)
{
    if (plr->prace == RACE_MON_RING && plr->riding == mon->id)
    {
        int odds = 10000;

        if (mon_race_is_unique(mon->race))
            odds = 1000;

        if (one_in_(odds) && _mon_save_p(mon))
        {
            int  sn = 0;
            point_t sp = plr->pos;
            int  i;

            for (i = 0; i < 8; i++)
            {
                point_t p = point_step(plr->pos, ddd[i]);
                dun_cell_ptr cell;

                if (!dun_pos_interior(cave, p)) continue;
                if (dun_mon_at(cave, p)) continue;

                cell = dun_cell_at(cave, p);
                if (cell_is_wall(cell) || door_is_closed(cell))
                {
                    if (!player_can_ride_aux(p, FALSE)) continue;
                }
                if (cell->type == FEAT_PATTERN) continue;

                /* This location is safe! */
                sn++;
                if (randint0(sn) > 0) continue;
                sp = p;
            }
            if (sn)
            {
                char m_name[MAX_NLEN];
                monster_desc(m_name, mon, 0);
                cmsg_format(TERM_VIOLET, "%^s removes you in disgust.", m_name);
                dun_draw_pos(cave, plr->pos);
                dun_draw_pos(cave, sp);
                viewport_verify();
                set_hostile(mon);
                plr->riding = 0;
                calc_bonuses();

                plr->update |= PU_BONUS | PU_VIEW | PU_LIGHT | PU_FLOW | PU_MON_LIGHT | PU_MONSTERS;
                plr->window |= PW_OVERHEAD | PW_DUNGEON;
                plr->redraw |= PR_EXTRA;
                plr->redraw |= PR_HEALTH_BARS;

                move_player_effect(sp, MPE_DONT_PICKUP | MPE_DONT_SWAP_MON);
                handle_stuff();

                /* This can be quite jarring when it happens out of the blue! */ 
                msg_print("Press Space to continue.");
                flush();
                for (;;)
                {
                    char ch = inkey();
                    if (ch == ' ') break;
                }
                msg_line_clear();
           }
        }
    }
}

void ring_summon_ring_bearer(void)
{
    if (plr->prace == RACE_MON_RING && plr->action == ACTION_GLITTER && !plr->riding)
    {
        int i;
        const int max_attempts = 10000;

        for (i = 0; i < max_attempts; i++)
        {
            point_t pos = point_random_jump(plr->pos, 10);
            if (!dun_pos_interior(cave, pos)) continue;
            if (!dun_allow_mon_at(cave, pos)) continue;
            summon_specific(who_create_plr(), pos, MAX(5, cave->difficulty), SUMMON_RING_BEARER, PM_ALLOW_UNIQUE);
            break;
        }
    }    
}
