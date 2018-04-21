#include "angband.h"

static cptr _mon_name(int r_idx)
{
    if (r_idx)
        return r_name + r_info[r_idx].name;
    return ""; /* Birth Menu */
}

/**********************************************************************
 * Essences
 **********************************************************************/
#define _MAX_ESSENCE 255

static int _essences[_MAX_ESSENCE] = {0};
static int _effects[EFFECT_MAX] = {0};

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
        int n = savefile_read_s16b(file);

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
        int n = savefile_read_s16b(file);

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

static bool _skip_flag(int which)
{
    switch (which)
    {
    case TR_FORCE_WEAPON:
    case TR_TUNNEL:
    case TR_BLOWS:
    case TR_CHAOTIC:
    case TR_VAMPIRIC:
    case TR_SLAY_ANIMAL:
    case TR_SLAY_EVIL:
    case TR_SLAY_UNDEAD:
    case TR_SLAY_DEMON:
    case TR_SLAY_ORC:
    case TR_SLAY_TROLL:
    case TR_SLAY_GIANT:
    case TR_SLAY_DRAGON:
    case TR_KILL_DRAGON:
    case TR_VORPAL:
    case TR_IMPACT:
    case TR_BRAND_POIS:
    case TR_BRAND_ACID:
    case TR_BRAND_ELEC:
    case TR_BRAND_FIRE:
    case TR_BRAND_COLD:
    case TR_RIDING:
    case TR_THROW:
    case TR_SLAY_HUMAN:
    case TR_NO_TELE:
    case TR_NO_MAGIC:
    case TR_TY_CURSE:
    case TR_HIDE_TYPE:
    case TR_SHOW_MODS:
    case TR_WEAPONMASTERY:
    case TR_XTRA_MIGHT:
    case TR_XTRA_SHOTS:
    case TR_IGNORE_ACID:
    case TR_IGNORE_ELEC:
    case TR_IGNORE_FIRE:
    case TR_IGNORE_COLD:
    case TR_ACTIVATE:
    case TR_DRAIN_EXP:
    case TR_TELEPORT:
    case TR_AGGRAVATE:
    case TR_BLESSED:
    case TR_KILL_ANIMAL:
    case TR_KILL_EVIL:
    case TR_KILL_UNDEAD:
    case TR_KILL_DEMON:
    case TR_KILL_ORC:
    case TR_KILL_TROLL:
    case TR_KILL_GIANT:
    case TR_KILL_HUMAN:
    case TR_FULL_NAME:
    case TR_FIXED_FLAVOR:
    case TR_WILD:
    case TR_ORDER:
    case TR_DARKNESS:
    case TR_SLAY_GOOD:
    case TR_DEC_STR:
    case TR_DEC_INT:
    case TR_DEC_WIS:
    case TR_DEC_DEX:
    case TR_DEC_CON:
    case TR_DEC_CHR:
    case TR_VULN_ACID:
    case TR_VULN_ELEC:
    case TR_VULN_FIRE:
    case TR_VULN_COLD:
    case TR_VULN_POIS:
    case TR_VULN_FEAR:
    case TR_VULN_LITE:
    case TR_VULN_DARK:
    case TR_VULN_BLIND:
    case TR_VULN_CONF:
    case TR_VULN_SOUND:
    case TR_VULN_SHARDS:
    case TR_VULN_NETHER:
    case TR_VULN_NEXUS:
    case TR_VULN_CHAOS:
    case TR_VULN_DISEN:
    case TR_DEC_STEALTH:
    case TR_DEC_SPEED:
    case TR_DEC_LIFE:
    case TR_SH_REVENGE:
    case TR_VORPAL2:
    case TR_DEC_MAGIC_MASTERY:
    case TR_DEC_SPELL_CAP:
    case TR_DEC_SPELL_POWER:
    case TR_SLAY_LIVING:
    case TR_STUN:
        return TRUE;
    }
    return FALSE;
}

static bool _add_essence(int which, int amount)
{
    int n = _essences[which];

    if (amount > 0)
        n += amount;

    if (n > 10000)
        n = 10000;

    if (n != _essences[which])
    {
        _essences[which] = n;
        return TRUE;
    }

    return FALSE;
}

static bool _absorb(object_type *o_ptr)
{
    bool result = FALSE;
    int i;
    int div = 1;
    u32b flags[TR_FLAG_SIZE];
    object_flags(o_ptr, flags);

    if (o_ptr->curse_flags & TRC_AGGRAVATE)
        div++;
    if (o_ptr->curse_flags & (TRC_TY_CURSE | TRC_HEAVY_CURSE))
        div++;

    for (i = 0; i < TR_FLAG_MAX; i++)
    {
        if (_skip_flag(i)) continue;
        if (have_flag(flags, i))
        {
            if (is_pval_flag(i))
            {
                if (_add_essence(i, o_ptr->pval/div))
                    result = TRUE;
            }
            else
            {
                _essences[i]++;
                result = TRUE;
            }
        }
    }

    if (_add_essence(TR_ES_AC, o_ptr->to_a/div))
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
        p_ptr->update |= PU_BONUS;
        msg_print("You grow stronger!");
    }
    return result;
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

static void _add_stat_flag(int flag, u32b flgs[TR_FLAG_SIZE])
{
    if (_calc_stat_bonus(flag))
        add_flag(flgs, flag);
}

static int _res_power(int which)
{
    switch (which)
    {
    case RES_ACID:
    case RES_FIRE:
    case RES_COLD:
    case RES_ELEC:
    case RES_CONF:
    case RES_FEAR:
    case RES_TIME:
    case RES_TELEPORT:
        return 2;

    case RES_SOUND:
    case RES_SHARDS:
    case RES_CHAOS:
        return 4;

    case RES_DISEN:
    case RES_POIS:
    case RES_LITE:
    case RES_DARK:
    case RES_NETHER:
    case RES_NEXUS:
        return 3;
    }

    return 2;
}

/**********************************************************************
 * Birth and Evolution
 **********************************************************************/
static void _birth(void) 
{ 
    object_type forge;
    char        buf[MAX_NLEN];
    int i;

    for (i = 0; i < _MAX_ESSENCE; i++)
        _essences[i] = 0;

    for (i = 0; i < EFFECT_MAX; i++)
        _effects[i] = 0;

    p_ptr->current_r_idx = MON_RING_MIMIC;
    equip_on_change_race();

    object_prep(&forge, lookup_kind(TV_RING, 0));
    add_flag(forge.art_flags, TR_NO_REMOVE);
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_RING, 0));
    get_random_name(buf, &forge, 1);
    forge.art_name = quark_add(buf);
    forge.to_a = 10;
    effect_add(&forge, EFFECT_BOLT_MISSILE);
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_AMULET, 0));
    get_random_name(buf, &forge, 1);
    forge.art_name = quark_add(buf);
    forge.pval = 1;
    add_flag(forge.art_flags, TR_INT);
    add_flag(forge.art_flags, TR_CHR);
    effect_add(&forge, EFFECT_PHASE_DOOR);
    add_outfit(&forge);

    object_prep(&forge, lookup_kind(TV_SCROLL, SV_SCROLL_TELEPORT));
    forge.number = 10;
    add_outfit(&forge);

    show_file(TRUE, "rings.txt", NULL, 0, 0);
}

static bool _drain_essences(int div)
{
    bool result = FALSE;
    int i;

    for (i = 0; i < _MAX_ESSENCE; i++)
    {
        int n = _essences[i];
        
        if (!n) continue;
        if (i == TR_SPEED) continue;
        
        _essences[i] -= _essences[i] / div;
        if (_essences[i] < n)
            result = TRUE;
    }
    if (result)
        p_ptr->update |= PU_BONUS;
    return result;
}

static void _gain_level(int new_level) 
{
}

/**********************************************************************
 * Powers
 **********************************************************************/
static void _absorb_spell(int cmd, variant *res)
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
        object_type * o_ptr;
        int item;
        char o_name[MAX_NLEN];

        var_set_bool(res, FALSE);
        item_tester_hook = object_is_jewelry;

        if (!get_item(&item, "Absorb which item? ", "You have nothing to absorb.", USE_INVEN | USE_FLOOR)) break;

        if (item >= 0)
            o_ptr = &inventory[item];
        else
            o_ptr = &o_list[0 - item];

        object_desc(o_name, o_ptr, OD_NAME_ONLY);
        msg_format("You absorb the power of %s!", o_name);
        _absorb(o_ptr);

        if (item >= 0)
        {
            inven_item_increase(item, -1);
            inven_item_describe(item);
            inven_item_optimize(item);
        }
        else
        {
            floor_item_increase(0 - item, -1);
            floor_item_describe(0 - item);
            floor_item_optimize(0 - item);
        }

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _detect_spell(int cmd, variant *res)
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
        int rng = DETECT_RAD_DEFAULT;
        int i, y, x;
        bool detect = FALSE;

        if (d_info[dungeon_type].flags1 & DF1_DARKNESS) rng /= 3;

        for (i = 1; i < o_max; i++)
        {
            object_type *o_ptr = &o_list[i];

            if (!o_ptr->k_idx) continue;
            if (o_ptr->held_m_idx) continue;
            y = o_ptr->iy;
            x = o_ptr->ix;
            if (distance(py, px, y, x) > rng) continue;
            if (!object_is_jewelry(o_ptr)) continue;
            o_ptr->marked |= OM_FOUND;
            lite_spot(y, x);
            detect = TRUE;
        }
        if (detect_monsters_string(DETECT_RAD_DEFAULT, "=\""))
            detect = TRUE;

        if (detect)
            msg_print("You sense your kind.");

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _judge_spell(int cmd, variant *res)
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
        if (p_ptr->lev >= 35)
            var_set_bool(res, identify_fully(object_is_jewelry));
        else
            var_set_bool(res, ident_spell(object_is_jewelry));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _glitter_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Glitter");
        break;
    case SPELL_DESC:
        var_set_string(res, "This spell makes yourself irresistable to potential ring bearers.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (p_ptr->riding)
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

static void _charm_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Charm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to dominate a single ring bearer.");
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        if (get_aim_dir(&dir))
        {
            int power = spell_power(p_ptr->lev * 2);          
            project_hook(GF_CHARM_RING_BEARER, dir, power, PROJECT_STOP | PROJECT_KILL);
            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
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
        me.weight = 430;
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
        { EFFECT_BOLT_LITE,           17,  11, 55 },
        { EFFECT_BOLT_DARK,           19,  12, 60 },
        { EFFECT_BOLT_NETHER,         20,  12, 60 },
        { EFFECT_BOLT_NEXUS,          22,  15, 65 },
        { EFFECT_BOLT_CONF,           24,  17, 65 },
        { EFFECT_BOLT_SOUND,          25,  17, 65 },
        { EFFECT_BOLT_SHARDS,         26,  18, 65 },
        { EFFECT_BOLT_DISEN,          27,  18, 65 },
        { EFFECT_BOLT_TIME,           28,  19, 65 },
        { EFFECT_BOLT_CHAOS,          30,  20, 65 },
        { EFFECT_BOLT_WATER,          31,  20, 65 },
        { EFFECT_BOLT_MANA,           32,  22, 70 },
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
        { EFFECT_BALL_LITE,           35,  18, 55 },
        { EFFECT_BALL_DARK,           36,  18, 60 },
        { EFFECT_BALL_CHAOS,          37,  35, 65 },
        { EFFECT_BALL_WATER,          38,  37, 70 },
        { EFFECT_BALL_MANA,           40,  42, 75 },
        { EFFECT_NONE } } },

    { "Offense: Breath", '3', TERM_RED,
      { { EFFECT_BREATHE_ELEC,        25,  30, 35 },
        { EFFECT_BREATHE_COLD,        26,  30, 45 },
        { EFFECT_BREATHE_ACID,        27,  35, 55 },
        { EFFECT_BREATHE_FIRE,        28,  35, 55 },
        { EFFECT_BREATHE_POIS,        30,  35, 45 },
        { EFFECT_BREATHE_LITE,        32,  35, 55 },
        { EFFECT_BREATHE_DARK,        33,  35, 60 },
        { EFFECT_BREATHE_NETHER,      35,  35, 60 },
        { EFFECT_BREATHE_NEXUS,       36,  40, 65 },
        { EFFECT_BREATHE_CONF,        37,  40, 65 },
        { EFFECT_BREATHE_DISEN,       38,  45, 65 },
        { EFFECT_BREATHE_TIME,        39,  45, 65 },
        { EFFECT_BREATHE_CHAOS,       40,  50, 70 },
        { EFFECT_BREATHE_SOUND,       41,  55, 70 },
        { EFFECT_BREATHE_SHARDS,      42,  55, 70 },
        { EFFECT_NONE } } },

    { "Offense: Other", '4', TERM_RED,
      { { EFFECT_DRAIN_LIFE,          30,  20, 50 },
        { EFFECT_ARROW,               30,  20, 50 },
        { EFFECT_DISPEL_UNDEAD,       32,  30, 50 }, 
        { EFFECT_DISPEL_DEMON,        32,  30, 50 },
        { EFFECT_DISPEL_GOOD,         33,  30, 55 }, 
        { EFFECT_DISPEL_EVIL,         35,  35, 60 }, 
        { EFFECT_DISPEL_LIFE,         35,  35, 60 }, 
        { EFFECT_CONFUSING_LITE,      37,  40, 60 },
        { EFFECT_DISPEL_EVIL_HERO,    40,  40, 60 },
        { EFFECT_ROCKET,              42,  45, 65 },
        { EFFECT_MANA_STORM,          45,  55, 75 },
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
        { EFFECT_RESTORE_STATS,       40,  60, 70 },
        { EFFECT_RESTORING,           40,  60, 70 },
        { EFFECT_HEAL_CURING_HERO,    45,  65, 80 },
        { EFFECT_NONE } } },

    { "Detection/Knowledge", 'D', TERM_L_BLUE,
      { { EFFECT_LITE_AREA,            1,   2, 25 },
        { EFFECT_DETECT_TRAPS,         3,   4, 30 },
        { EFFECT_DETECT_MONSTERS,      5,   5, 35 },
        { EFFECT_DETECT_OBJECTS,       8,   7, 40 },
        { EFFECT_IDENTIFY,            15,  12, 55 },
        { EFFECT_LITE_MAP_AREA,       20,  10, 50 },
        { EFFECT_ENLIGHTENMENT,       20,  10, 50 },
        { EFFECT_DETECT_ALL,          25,  15, 60 },
        { EFFECT_PROBING,             27,  20, 60 },
        { EFFECT_IDENTIFY_FULL,       33,  25, 65 },
        { EFFECT_LIST_UNIQUES,        40,  50, 75 },
        { EFFECT_LIST_ARTIFACTS,      42,  50, 75 }, 
        { EFFECT_CLAIRVOYANCE,        45,  50, 70 },
        { EFFECT_NONE } } },

    { "Teleportations", 'T', TERM_L_BLUE,
      { { EFFECT_PHASE_DOOR,           3,   3, 35 },
        { EFFECT_TELEPORT,            12,   7, 45 },
        { EFFECT_TELEPORT_AWAY,       20,  15, 55 },
        { EFFECT_STRAFING,            20,  15, 55 },
        { EFFECT_RECALL,              25,  20, 55 },
        { EFFECT_TELEKINESIS,         27,  22, 60 },
        { EFFECT_ESCAPE,              30,  25, 60 },
        { EFFECT_BANISH_EVIL,         37,  35, 65 },
        { EFFECT_BANISH_ALL,          40,  40, 70 },
        { EFFECT_DIMENSION_DOOR,      43,  50, 70 },
        { EFFECT_NONE } } },

    { "Utility", 'U', TERM_L_BLUE, 
      { { EFFECT_SATISFY_HUGER,        5,   5, 35 },
        { EFFECT_STONE_TO_MUD,        15,  10, 50 },
        { EFFECT_DESTROY_TRAP,        20,  12, 50 },
        { EFFECT_DESTROY_TRAPS,       25,  15, 55 },
        { EFFECT_EARTHQUAKE,          25,  20, 60 },
        { EFFECT_RUNE_EXPLOSIVE,      29,  25, 65 },
        { EFFECT_ENCHANTMENT,         30,  80, 85 },
        { EFFECT_RECHARGING,          35,  30, 65 },
        { EFFECT_RUNE_PROTECTION,     37,  50, 70 },
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

/* Menu Code 1: Choose which group of magic to use */
static cptr _group_choice = NULL;

static void _group_menu_fn(int cmd, int which, vptr cookie, variant *res)
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
static void _spell_menu_fn(int cmd, int which, vptr cookie, variant *res)
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
        effect_t effect = {0};

        effect.type = s->effect;
        effect.level = s->level;

        sprintf(name, "%s", do_effect(&effect, SPELL_NAME, 0));
        sprintf(info, "%s", do_effect(&effect, SPELL_INFO, _boost(s->effect)));

        sprintf(buf, "%-30.30s %3d %3d %3d%% ", name, s->level, s->cost, s->fail);
        if (info)
            strcat(buf, info);
        var_set_string(res, buf);
        break;            
    }
    case MENU_HELP:
    {
        effect_t effect;

        effect.type = s->effect;
        effect.level = s->level;
        var_set_string(res, do_effect(&effect, SPELL_DESC, 0));
        break;
    }
    case MENU_COLOR:
        if (s->level > p_ptr->lev)
            var_set_int(res, TERM_L_DARK);
        else if (s->cost > p_ptr->csp)
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
            choice->cost = spell->cost;
            choice->fail = calculate_fail_rate(spell->level, spell->fail, p_ptr->stat_ind[A_INT]);

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

    if (p_ptr->confused)
    {
        msg_print("You are too confused!");
        return;
    }

    spell = _prompt();
    if (spell.effect == EFFECT_NONE)
        return;

    if (spell.level > p_ptr->lev)
    {
        msg_print("You can't use that spell yet!");
        return;
    }
    if (spell.cost > p_ptr->csp)
    {
        msg_print("You do not have enough mana to use this power.");
        return;
    }

    p_ptr->csp -= spell.cost;

    if (randint0(100) < spell.fail)
    {
        if (flush_failure) flush();
        msg_format("You failed to get the spell off!");
        sound(SOUND_FAIL);
    }
    else
    {
        effect_t effect = {0};
        effect.type = spell.effect;
        effect.level = spell.level;
        device_known = TRUE; /* Hack */
        if (!do_effect(&effect, SPELL_CAST, _boost(spell.effect)))
        {
            p_ptr->csp += spell.cost;
        }
    }
    energy_use = 100;
    p_ptr->redraw |= PR_MANA;
    p_ptr->redraw |= PR_HP;
    p_ptr->window |= PW_PLAYER;
    p_ptr->window |= PW_SPELL;
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

            effect.type = spell.effect;
            effect.level = spell.level;
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
    int l = p_ptr->lev;
    int to_a = l;

    to_a += _calc_amount(_essences[TR_ES_AC], 1, 15);
    p_ptr->to_a += to_a;
    p_ptr->dis_to_a += to_a;

    p_ptr->no_cut = TRUE;
    res_add(RES_BLIND);
    res_add(RES_POIS);
    p_ptr->hold_life = TRUE;

    if (p_ptr->lev >= 25)
    {
        res_add(RES_COLD);
        res_add(RES_ELEC);
    }

    for (i = 0; i < 6; i++) /* Assume in order */
        p_ptr->stat_add[A_STR + i] += _calc_stat_bonus(TR_STR + i);

    for (i = 0; i < RES_MAX; i++)
    {
        int j = res_get_object_flag(i);
        int n = _calc_amount(_essences[j], _res_power(i), 1);

        for (; n; --n)
            res_add(i);
    }
    if (_essences[TR_IM_ACID] >= 2)
        res_add_immune(RES_ACID);
    if (_essences[TR_IM_ELEC] >= 2)
        res_add_immune(RES_ELEC);
    if (_essences[TR_IM_FIRE] >= 2)
        res_add_immune(RES_FIRE);
    if (_essences[TR_IM_COLD] >= 2)
        res_add_immune(RES_COLD);

    p_ptr->life += 3*_calc_amount(_essences[TR_LIFE], 7, 1);

    p_ptr->skills.stl += _calc_amount(_essences[TR_STEALTH], 2, 1);
    p_ptr->pspeed += _calc_amount(_essences[TR_SPEED], 1, 5);
    p_ptr->skills.dev += 8*_calc_amount(_essences[TR_MAGIC_MASTERY], 2, 1);
    p_ptr->device_power += _calc_amount(_essences[TR_DEVICE_POWER], 2, 1);
    p_ptr->spell_power += _calc_amount(_essences[TR_SPELL_POWER], 2, 1);
    p_ptr->spell_cap += _calc_amount(_essences[TR_SPELL_CAP], 2, 1);
    p_ptr->skills.srh += 5*_calc_amount(_essences[TR_SEARCH], 2, 1);
    p_ptr->skills.fos += 5*_calc_amount(_essences[TR_SEARCH], 2, 1);
    p_ptr->see_infra += _calc_amount(_essences[TR_INFRA], 2, 1);

    if (_essences[TR_DEC_MANA] >= 7)
        p_ptr->dec_mana = TRUE;
    if (_essences[TR_EASY_SPELL] >= 7)
        p_ptr->easy_spell = TRUE;

    if (_essences[TR_SUST_STR] >= 5)
        p_ptr->sustain_str = TRUE;
    if (_essences[TR_SUST_INT] >= 5)
        p_ptr->sustain_int = TRUE;
    if (_essences[TR_SUST_WIS] >= 5)
        p_ptr->sustain_wis = TRUE;
    if (_essences[TR_SUST_DEX] >= 5)
        p_ptr->sustain_dex = TRUE;
    if (_essences[TR_SUST_CON] >= 5)
        p_ptr->sustain_con = TRUE;
    if (_essences[TR_SUST_CHR] >= 5)
        p_ptr->sustain_chr = TRUE;

    if (_essences[TR_TELEPATHY] >= 2)
        p_ptr->telepathy = TRUE;
    if (_essences[TR_ESP_ANIMAL] >= 2)
        p_ptr->esp_animal = TRUE;
    if (_essences[TR_ESP_UNDEAD] >= 2)
        p_ptr->esp_undead = TRUE;
    if (_essences[TR_ESP_DEMON] >= 2)
        p_ptr->esp_demon = TRUE;
    if (_essences[TR_ESP_ORC] >= 2)
        p_ptr->esp_orc = TRUE;
    if (_essences[TR_ESP_TROLL] >= 2)
        p_ptr->esp_troll = TRUE;
    if (_essences[TR_ESP_GIANT] >= 2)
        p_ptr->esp_giant = TRUE;
    if (_essences[TR_ESP_DRAGON] >= 2)
        p_ptr->esp_dragon = TRUE;
    if (_essences[TR_ESP_HUMAN] >= 2)
        p_ptr->esp_human = TRUE;
    if (_essences[TR_ESP_EVIL] >= 2)
        p_ptr->esp_evil = TRUE;
    if (_essences[TR_ESP_GOOD] >= 2)
        p_ptr->esp_good = TRUE;
    if (_essences[TR_ESP_NONLIVING] >= 2)
        p_ptr->esp_nonliving = TRUE;
    if (_essences[TR_ESP_UNIQUE] >= 2)
        p_ptr->esp_unique = TRUE;

    if (_essences[TR_LEVITATION] >= 2)
        p_ptr->levitation = TRUE;
    if (_essences[TR_FREE_ACT] >= 1)
        p_ptr->free_act = TRUE;
    if (_essences[TR_SEE_INVIS] >= 1)
        p_ptr->see_inv = TRUE;
    if (_essences[TR_SLOW_DIGEST] >= 2)
        p_ptr->slow_digest = TRUE;
    if (_essences[TR_REGEN] >= 7)
        p_ptr->regenerate = TRUE;
    if (_essences[TR_REFLECT] >= 3)
        p_ptr->reflect = TRUE;

    if (_essences[TR_SH_FIRE] >= 7)
        p_ptr->sh_fire = TRUE;
    if (_essences[TR_SH_ELEC] >= 7)
        p_ptr->sh_elec = TRUE;
    if (_essences[TR_SH_COLD] >= 7)
        p_ptr->sh_cold = TRUE;
}

static void _get_immunities(u32b flgs[TR_FLAG_SIZE]) 
{
    if (_essences[TR_IM_ACID] >= 3)
        add_flag(flgs, TR_RES_ACID);
    if (_essences[TR_IM_ELEC] >= 3)
        add_flag(flgs, TR_RES_ELEC);
    if (_essences[TR_IM_FIRE] >= 3)
        add_flag(flgs, TR_RES_FIRE);
    if (_essences[TR_IM_COLD] >= 3)
        add_flag(flgs, TR_RES_COLD);
}

static void _get_flags(u32b flgs[TR_FLAG_SIZE]) 
{
    int i;

    add_flag(flgs, TR_LITE);
    add_flag(flgs, TR_RES_BLIND);
    add_flag(flgs, TR_RES_POIS);
    add_flag(flgs, TR_HOLD_LIFE);

    if (p_ptr->lev >= 25)
    {
        add_flag(flgs, TR_RES_COLD);
        add_flag(flgs, TR_RES_ELEC);
    }

    for (i = 0; i < 6; i++) /* Assume in order */
    {
        if (i != 0) /* Bug: Giving TR_STR marks the player as cursed ... */
            _add_stat_flag(TR_STR + i, flgs);
        if (_essences[TR_SUST_STR + i] >= 5)
            add_flag(flgs, TR_SUST_STR + i);
    }

    for (i = 0; i < RES_MAX; i++)
    {
        int j = res_get_object_flag(i);
        int n = _calc_amount(_essences[j], _res_power(i), 1);

        if (n)
            add_flag(flgs, j);
    }

    if (_essences[TR_DEC_MANA] >= 7)
        add_flag(flgs, TR_DEC_MANA);
    if (_essences[TR_EASY_SPELL] >= 7)
        add_flag(flgs, TR_EASY_SPELL);

    if (_calc_amount(_essences[TR_SPEED], 1, 5))
        add_flag(flgs, TR_SPEED);
    if (_calc_amount(_essences[TR_STEALTH], 2, 1))
        add_flag(flgs, TR_STEALTH);
    if (_calc_amount(_essences[TR_MAGIC_MASTERY], 2, 1))
        add_flag(flgs, TR_MAGIC_MASTERY);
    if (_calc_amount(_essences[TR_SEARCH], 2, 1))
        add_flag(flgs, TR_SEARCH);
    if (_calc_amount(_essences[TR_INFRA], 2, 1))
        add_flag(flgs, TR_INFRA);

    if (_essences[TR_TELEPATHY] >= 2)
        add_flag(flgs, TR_TELEPATHY);

    for (i = TR_ESP_ANIMAL; i <= TR_ESP_UNIQUE; i++)
    {
        if (_essences[i] >= 2)
            add_flag(flgs, i);
    }

    if (_essences[TR_LEVITATION] >= 2)
        add_flag(flgs, TR_LEVITATION);
    if (_essences[TR_FREE_ACT] >= 1)
        add_flag(flgs, TR_FREE_ACT);
    if (_essences[TR_SEE_INVIS] >= 1)
        add_flag(flgs, TR_SEE_INVIS);
    if (_essences[TR_SLOW_DIGEST] >= 2)
        add_flag(flgs, TR_SLOW_DIGEST);
    if (_essences[TR_REGEN] >= 7)
        add_flag(flgs, TR_REGEN);
    if (_essences[TR_REFLECT] >= 3)
        add_flag(flgs, TR_REFLECT);

    if (_essences[TR_SH_FIRE] >= 7)
        add_flag(flgs, TR_SH_FIRE);
    if (_essences[TR_SH_ELEC] >= 7)
        add_flag(flgs, TR_SH_ELEC);
    if (_essences[TR_SH_COLD] >= 7)
        add_flag(flgs, TR_SH_COLD);
}

/**********************************************************************
 * Character Dump
 **********************************************************************/
static void _dump_ability_flag(FILE* fff, int which, int threshold, cptr name)
{
    int n = _essences[which];
    if (n > 0)
    {
        fprintf(fff, "   %-22.22s %5d %5d %5.5s\n", 
            name,
            n, 
            threshold,
            n >= threshold ? "Y" : ""
        );
    }
}
static void _dump_bonus_flag(FILE* fff, int which, int power, int rep, cptr name)
{
    int n = _essences[which];
    if (n > 0)
    {
        fprintf(fff, "   %-22.22s %5d %5d %+5d\n", 
            name,
            n, 
            _calc_needed(n, power, rep),
            _calc_amount(n, power, rep)
        );
    }
}

static void _dump_effects(FILE* fff)
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
            if (_effects[s->effect])
            {
                char     buf[1024];
                char     name[255];
                char     info[255];
                effect_t effect = {0};

                effect.type = s->effect;
                effect.level = s->level;

                sprintf(name, "%s", do_effect(&effect, SPELL_NAME, 0));
                sprintf(info, "%s", do_effect(&effect, SPELL_INFO, _boost(s->effect)));

                sprintf(buf, "%-30.30s %3d %3d %3d %3d%% ", name, _effects[s->effect], s->level, s->cost, calculate_fail_rate(s->level, s->fail, p_ptr->stat_ind[A_INT]));
                if (info)
                    strcat(buf, info);

                if (!ct)
                {
                    fprintf(fff, "\n   %-30.30s  Ct Lvl Cst Fail Info\n", g->name);
                    fprintf(fff, "   ------------------------------ --- --- --- ---- ----------------\n");
                }
                fprintf(fff, "   %s\n", buf);
                ct++;
            }
        }
    }
}

static void _character_dump(FILE* fff)
{
    int i;
    fprintf(fff, "\n\n=================================== Essences ==================================\n");
    fprintf(fff, "\n   %-22.22s Total  Need Bonus\n", "Stats");
    fprintf(fff, "   ---------------------- ----- ----- -----\n");
    for (i = 0; i < 6; i++) /* Assume in order */
        _dump_bonus_flag(fff, TR_STR + i, 2, 1, stat_name_true[A_STR + i]);

    fprintf(fff, "\n   %-22.22s Total  Need Bonus\n", "Skills");
    fprintf(fff, "   ---------------------- ----- ----- -----\n");
    _dump_bonus_flag(fff, TR_ES_AC, 1, 15, "To AC");
    _dump_bonus_flag(fff, TR_STEALTH, 2, 1, "Stealth");
    _dump_bonus_flag(fff, TR_SPEED, 1, 5, "Speed");
    _dump_bonus_flag(fff, TR_LIFE, 7, 1, "Life");
    _dump_bonus_flag(fff, TR_SEARCH, 2, 1, "Searching");
    _dump_bonus_flag(fff, TR_INFRA, 2, 1, "Infravision");
    _dump_bonus_flag(fff, TR_TUNNEL, 2, 1, "Digging");
    _dump_bonus_flag(fff, TR_LITE, 1, 1, "Light");
    _dump_bonus_flag(fff, TR_MAGIC_MASTERY, 2, 1, "Magic Mastery");
    _dump_bonus_flag(fff, TR_DEVICE_POWER, 2, 1, "Device Power");
    _dump_bonus_flag(fff, TR_SPELL_POWER, 2, 1, "Spell Power");
    _dump_bonus_flag(fff, TR_SPELL_CAP, 2, 1, "Spell Capacity");
 
    fprintf(fff, "\n   %-22.22s Total  Need Bonus\n", "Resistances");
    fprintf(fff, "   ---------------------- ----- ----- -----\n");
    for (i = 0; i < RES_MAX; i++)
        _dump_bonus_flag(fff, res_get_object_flag(i), _res_power(i), 1, format("%^s", res_name(i)));

    _dump_ability_flag(fff, TR_IM_ACID, 2, "Immune Acid");
    _dump_ability_flag(fff, TR_IM_ELEC, 2, "Immune Elec");
    _dump_ability_flag(fff, TR_IM_FIRE, 2, "Immune Fire");
    _dump_ability_flag(fff, TR_IM_COLD, 2, "Immune Cold");

    fprintf(fff, "\n   %-22.22s Total  Need Bonus\n", "Abilities");
    fprintf(fff, "   ---------------------- ----- ----- -----\n");
    _dump_ability_flag(fff, TR_FREE_ACT, 1, "Free Action");
    _dump_ability_flag(fff, TR_SEE_INVIS, 1, "See Invisible");
    _dump_ability_flag(fff, TR_LEVITATION, 2, "Levitation");
    _dump_ability_flag(fff, TR_SLOW_DIGEST, 2, "Slow Digestion");
    _dump_ability_flag(fff, TR_REGEN, 7, "Regeneration");
    _dump_ability_flag(fff, TR_DEC_MANA, 7, "Economical Mana");
    _dump_ability_flag(fff, TR_EASY_SPELL, 7, "Wizardry");
    _dump_ability_flag(fff, TR_REFLECT, 3, "Reflection");
    _dump_ability_flag(fff, TR_SH_FIRE, 7, "Aura Fire");
    _dump_ability_flag(fff, TR_SH_ELEC, 7, "Aura Elec");
    _dump_ability_flag(fff, TR_SH_COLD, 7, "Aura Cold");
    for (i = 0; i < 6; i++) /* Assume in order */
        _dump_ability_flag(fff, TR_SUST_STR + i, 5, format("Sustain %s", stat_name_true[A_STR + i]));

    fprintf(fff, "\n   %-22.22s Total  Need Bonus\n", "ESP");
    fprintf(fff, "   ---------------------- ----- ----- -----\n");
    _dump_ability_flag(fff, TR_TELEPATHY, 2, "Telepathy");
    _dump_ability_flag(fff, TR_ESP_ANIMAL, 2, "ESP Animals");
    _dump_ability_flag(fff, TR_ESP_UNDEAD, 2, "ESP Undead");
    _dump_ability_flag(fff, TR_ESP_DEMON, 2, "ESP Demon");
    _dump_ability_flag(fff, TR_ESP_ORC, 2, "ESP Orc");
    _dump_ability_flag(fff, TR_ESP_TROLL, 2, "ESP Troll");
    _dump_ability_flag(fff, TR_ESP_GIANT, 2, "ESP Giant");
    _dump_ability_flag(fff, TR_ESP_DRAGON, 2, "ESP Dragon");
    _dump_ability_flag(fff, TR_ESP_HUMAN, 2, "ESP Human");
    _dump_ability_flag(fff, TR_ESP_EVIL, 2, "ESP Evil");
    _dump_ability_flag(fff, TR_ESP_GOOD, 2, "ESP Good");
    _dump_ability_flag(fff, TR_ESP_NONLIVING, 2, "ESP Nonliving");
    _dump_ability_flag(fff, TR_ESP_UNIQUE, 2, "ESP Unique");

    fprintf(fff, "\n\n==================================== Spells ===================================\n");
    _dump_effects(fff);
}

/**********************************************************************
 * Public
 **********************************************************************/
race_t *mon_ring_get_race_t(void)
{
    static race_t me = {0};
    static bool   init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  40,  38,   5,  16,  20,  34,  20};
    skills_t xs = {  7,  15,  11,   0,   0,   0,   6,   7};

        me.skills = bs;
        me.extra_skills = xs;

        me.name = "Ring";
        me.desc = "Rings are sentient creatures animated by magical means.\n \n"
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

        me.stats[A_STR] = -3;
        me.stats[A_INT] =  4;
        me.stats[A_WIS] =  1;
        me.stats[A_DEX] =  0;
        me.stats[A_CON] = -1;
        me.stats[A_CHR] =  3;

        me.life = 85;
        me.infra = 5;
        me.exp = 150;
        me.base_hp = 10;

        me.calc_bonuses = _calc_bonuses;

        me.get_powers = _get_powers;
        me.caster_info = _caster_info;

        me.character_dump = _character_dump;
        me.get_flags = _get_flags;
        me.get_immunities = _get_immunities;
        me.gain_level = _gain_level;
        me.birth = _birth;

        me.load_player = _load;
        me.save_player = _save;

        me.flags = RACE_IS_MONSTER;
        me.pseudo_class_idx = CLASS_MAGE;
        me.boss_r_idx = MON_ONE_RING;

        init = TRUE;
    }

    if (p_ptr->riding)
        me.subname = _mon_name(m_list[p_ptr->riding].r_idx);
    else
        me.subname = NULL;

    me.equip_template = mon_get_equip_template();
    return &me;
}

void ring_absorb_object(object_type *o_ptr)
{
    if (object_is_jewelry(o_ptr))
    {
        char o_name[MAX_NLEN];
        object_desc(o_name, o_ptr, OD_NAME_ONLY);
        msg_format("You attempt to drain power from %s.", o_name);
        _absorb(o_ptr);
    }
}

int ring_calc_torch(void)
{
    return 1 + _calc_amount(_essences[TR_LITE], 1, 1);
}

bool ring_disenchant(void)
{
    bool result = FALSE;
    if (!res_save(RES_DISEN, 70) && _drain_essences(20))
    {
        msg_print("You feel power draining from your body!");
        result = TRUE;
    }
    return result;
}

static int _plev(void)
{
    if (p_ptr->lev <= 40)
        return p_ptr->lev;

    return 40 + (p_ptr->lev - 40)*2;
}

static int _r_level(monster_race *r_ptr)
{
int ml = r_ptr->level;
    
    if (r_ptr->flags2 & RF2_POWERFUL)
        ml += 7;

    if (r_ptr->flags1 & RF1_UNIQUE)
        ml += ml/5;

    return ml;
}

static bool _mon_save_p(monster_type *m_ptr)
{
    int           pl = _plev();
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    int           ml = _r_level(r_ptr);
    bool          result = FALSE;

    if (r_ptr->flags1 & RF1_QUESTOR)
        return TRUE;
    
    /* Player may not exert their force of will out of sight! */
    if (projectable(py, px, m_ptr->fy, m_ptr->fx))
        pl += adj_stat_save[p_ptr->stat_ind[A_CHR]];

    if (pl <= 1) 
        return TRUE;

    if (randint1(pl) <= randint1(ml)) 
        result = TRUE;

    return result;
}

bool ring_dominate_m(int m_idx)
{
    monster_type *m_ptr = &m_list[m_idx];
    if ( p_ptr->prace == RACE_MON_RING 
      && !p_ptr->riding
      && !is_aware(m_ptr) 
      && mon_is_type(m_ptr->r_idx, SUMMON_RING_BEARER) )
    {
        char m_name[MAX_NLEN];
        monster_desc(m_name, m_ptr, 0);
        if (_mon_save_p(m_ptr))
        {
            msg_format("%^s sees you for what you truly are!", m_name);
            m_ptr->mflag2 |= MFLAG2_AWARE;
        }
        else
        {
            /* Pick the pretty up! */
            set_pet(m_ptr);

            switch (randint1(5))
            {
            case 1: msg_format("%^s says, 'My Precious!'", m_name); break;
            case 2: msg_format("%^s says, 'Look! A Pretty!'", m_name); break;
            case 3: msg_format("%^s says, 'Hark! What have we here?'", m_name); break;
            case 4: msg_format("%^s says, 'Finally! A lucky drop at last!'", m_name); break;
            case 5: msg_format("%^s says, 'This better be a Ring of Speed!'", m_name); break;
            }
            msg_format("%^s picks you up.", m_name);
            p_ptr->riding = m_idx;
            if (p_ptr->riding == p_ptr->health_who) health_track(0);
            set_action(ACTION_NONE);

            p_ptr->update |= PU_UN_VIEW | PU_UN_LITE;
            p_ptr->update |= PU_BONUS;
            p_ptr->redraw |= PR_MAP | PR_EXTRA;
            p_ptr->redraw |= PR_UHEALTH;
            move_player_effect(m_ptr->fy, m_ptr->fx, MPE_HANDLE_STUFF | MPE_ENERGY_USE | MPE_DONT_PICKUP | MPE_DONT_SWAP_MON);
        
            return TRUE;
        }
    }
    return FALSE;
}

void ring_process_m(int m_idx)
{
    if ( p_ptr->prace == RACE_MON_RING 
      && p_ptr->riding == m_idx )
    {
        monster_type *m_ptr = &m_list[m_idx];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];
        int           odds = 10000;

        if (r_ptr->flags1 & RF1_UNIQUE)
            odds = 1000;

        if (one_in_(odds) && !_mon_save_p(m_ptr))
        {
            char m_name[MAX_NLEN];
            int  x, y, ox, oy;
            int  sn = 0, sx = 0, sy = 0;
            int  i;
            monster_desc(m_name, m_ptr, 0);

            for (i = 0; i < 8; i++)
            {
                cave_type *c_ptr;
                y = py + ddy_ddd[i];
                x = px + ddx_ddd[i];

                if (!in_bounds(y, x)) continue;

                c_ptr = &cave[y][x];
                if (c_ptr->m_idx) continue;
                if (!cave_have_flag_grid(c_ptr, FF_MOVE) && !cave_have_flag_grid(c_ptr, FF_CAN_FLY))
                {
                    if (!player_can_ride_aux(c_ptr, FALSE)) continue;
                }

                if (cave_have_flag_grid(c_ptr, FF_PATTERN)) continue;

                /* This location is safe! */
                sn++;
                if (randint0(sn) > 0) continue;
                sy = y; sx = x;
            }
            if (sn && sy && sx)
            {
                msg_format("%^s removes you in disgust.", m_name);
                oy = py;
                ox = px;
                py = sy;
                px = sx;
                lite_spot(oy, ox);
                lite_spot(py, px);
                verify_panel();
                set_hostile(m_ptr);
                p_ptr->riding = 0;
                calc_bonuses();

                p_ptr->update |= PU_BONUS | PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE | PU_MONSTERS;
                p_ptr->window |= PW_OVERHEAD | PW_DUNGEON;
                p_ptr->redraw |= PR_EXTRA;
                p_ptr->redraw |= PR_UHEALTH;

                move_player_effect(py, px, MPE_DONT_PICKUP | MPE_DONT_SWAP_MON);
            }
        }
    }
}