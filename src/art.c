#include "angband.h"

#include <assert.h>

#define _ONCE 0x0001
#define ACTIVATION_CHANCE 5
#define TIER_CURSED 0
#define TIER_LOW    1
#define TIER_MED    2
#define TIER_HIGH   3

/* identify the current object allocation so that some powers are
 * only granted _ONCE */
static int _current = 0;

/******************************************************************************
 * Forge Info: Begin to forge a new artifact
 ******************************************************************************/
typedef struct {
    obj_ptr obj;
    int     lvl;
    int     mode;
    int     powers;
    int     bias;  /* more for naming than anything else ... */
    art_ptr art;
    ego_ptr ego;
} _forge_t, *_forge_ptr;

static _forge_t _forge(obj_ptr obj, int lvl, int mode)
{
    _forge_t f;
    f.obj = obj;
    f.lvl = lvl;
    f.mode = mode;
    f.powers = 0;
    f.bias = 0;
    f.art = NULL;
    f.ego = NULL;
    _current++;
    return f;
}

typedef void (*_forge_f)(_forge_ptr forge);

/******************************************************************************
 * Table Driven Power Allocation (Recursive)
 ******************************************************************************/
typedef struct {
    int      flag;
    _forge_f f;
    obj_p    p;
    int      weight;
    int      flags;
    int      last;
} _table_t, *_table_ptr;

/* choose a random entry from a table. support best of N assuming the table
 * is ordered in ascending quality. setting rolls=1 turns this feature off. */
static _table_ptr _table_choose(_forge_ptr forge, _table_ptr tbl, int rolls)
{
    int i, tot = 0, n;
    for (i = 0; ; i++)
    {
        _table_ptr entry = tbl + i;
        if (!entry->weight) break;
        if (entry->p && !entry->p(forge->obj)) continue;
        if ((entry->flags & _ONCE) && entry->last == _current) continue;
        tot += entry->weight;
    }
    if (!tot) return NULL;
    n = randint0(tot);
    for (i = 1; i < rolls; i++)
    {
        int x = randint0(tot);
        if (x > n) n = x;
    }
    for (i = 0; ; i++)
    {
        _table_ptr entry = tbl + i;
        if (!entry->weight) break;
        if (entry->p && !entry->p(forge->obj)) continue;
        if ((entry->flags & _ONCE) && entry->last == _current) continue;
        n -= entry->weight;
        if (n < 0) return entry;
    }
    return NULL;
}

/* choose and apply a random entry from a table */
static void _best_one(_forge_ptr forge, _table_ptr tbl, int rolls)
{
    _table_ptr entry = _table_choose(forge, tbl, rolls);
    if (!entry) return;
    if (entry->flag > 0) /* OF_INVALID or 0=OF_HIDE_TYPE signal no flag on this entry */
    {
        if (have_flag(forge->obj->flags, entry->flag))
        {
            if (one_in_(4))
                forge->powers--;
        }
        else
        {
            add_flag(forge->obj->flags, entry->flag);
            forge->powers--;
        }
    }
    if (entry->f) entry->f(forge);
    entry->last = _current;
}
static void _one(_forge_ptr forge, _table_ptr tbl)
{
    _best_one(forge, tbl, 1);
}

static void _many(_forge_ptr forge, _forge_f f)
{
    int ct = 0;
    do
    {
        f(forge);
        ct++;
    } while (forge->powers > 0 && one_in_(ct));
}

static void _freebie(_forge_ptr forge) { forge->powers++; }

/******************************************************************************
 * Stats
 ******************************************************************************/
static void _one_stat(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_STR, NULL, NULL, 1, _ONCE },
        { OF_INT, NULL, NULL, 1, _ONCE },
        { OF_WIS, NULL, NULL, 1, _ONCE },
        { OF_DEX, NULL, NULL, 1, _ONCE },
        { OF_CON, NULL, NULL, 1, _ONCE },
        { OF_CHR, NULL, NULL, 1, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _one_sustain(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_SUST_STR, NULL, NULL, 1, _ONCE },
        { OF_SUST_INT, NULL, NULL, 1, _ONCE },
        { OF_SUST_WIS, NULL, NULL, 1, _ONCE },
        { OF_SUST_DEX, NULL, NULL, 1, _ONCE },
        { OF_SUST_CON, NULL, NULL, 1, _ONCE },
        { OF_SUST_CHR, NULL, NULL, 1, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _sustaining(_forge_ptr forge)
{
    forge->powers++;
    _one_sustain(forge);
    _many(forge, _one_sustain);
}

/******************************************************************************
 * Auras
 ******************************************************************************/
static void _one_aura(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_AURA_ELEC,    NULL, NULL, 10 },
        { OF_AURA_FIRE,    NULL, NULL, 10 },
        { OF_AURA_COLD,    NULL, NULL, 10 },
        { OF_AURA_SHARDS,  NULL, NULL,  2 },
        { OF_AURA_REVENGE, NULL, NULL,  1 },
        { 0 } };

    _one(forge, tbl);
}

/******************************************************************************
 * Resistances
 ******************************************************************************/
static void _one_ele_resist(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_RES_(GF_ACID), NULL, NULL, 1 },
        { OF_RES_(GF_ELEC), NULL, NULL, 1 },
        { OF_RES_(GF_FIRE), NULL, NULL, 1 },
        { OF_RES_(GF_COLD), NULL, NULL, 1 },
        { 0 } };

    _one(forge, tbl);
}

static void _one_low_resist(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0, _one_ele_resist, NULL, 10 },
        { OF_RES_(GF_POIS), NULL, NULL, 1 },
        { 0 } };

    _one(forge, tbl);
}

static void _many_low_resists(_forge_ptr forge)
{
    _many(forge, _one_low_resist);
}

static void _one_high_resist(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_RES_(GF_LIGHT), NULL, NULL, 100 },
        { OF_RES_(GF_DARK), NULL, NULL, 100 },
        { OF_RES_(GF_CONFUSION), NULL, NULL, 100 },
        { OF_RES_(GF_NETHER), NULL, NULL, 100 },
        { OF_RES_(GF_NEXUS), NULL, NULL, 100 },
        { OF_RES_(GF_SOUND), NULL, NULL, 80 },
        { OF_RES_(GF_SHARDS), NULL, NULL, 70 },
        { OF_RES_(GF_CHAOS), NULL, NULL, 80 },
        { OF_RES_(GF_DISENCHANT), NULL, NULL, 90 },
        { OF_RES_(GF_TIME), NULL, NULL, 1 },
        { OF_RES_(GF_BLIND), NULL, NULL, 50 },
        { OF_RES_(GF_BLIND), NULL, obj_is_helmet, 400 },
        { OF_RES_(GF_FEAR), NULL, NULL, 50 },
        { OF_RES_(GF_FEAR), NULL, obj_is_boots, 200 }, /* so you can stand your ground! */
        { 0 } };

    _one(forge, tbl);
}

static void _many_high_resists(_forge_ptr forge)
{
    _many(forge, _one_high_resist);
}

static void _one_resist(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0, _one_low_resist, NULL, 10 },
        { 0, _many_low_resists, NULL, 1 },
        { 0, _one_high_resist, NULL, 10 },
        { 0, _many_high_resists, NULL, 1 },
        { 0 } };

    _one(forge, tbl);
}

static void _many_resists(_forge_ptr forge)
{
    _many(forge, _one_resist);
}

static void _bias_acid(_forge_ptr forge) { forge->bias |= BIAS_ACID; }
static void _bias_elec(_forge_ptr forge) { forge->bias |= BIAS_ELEC; }
static void _bias_fire(_forge_ptr forge) { forge->bias |= BIAS_FIRE; }
static void _bias_cold(_forge_ptr forge) { forge->bias |= BIAS_COLD; }

static void _one_immunity(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_IM_(GF_ACID), _bias_acid, NULL, 1 },
        { OF_IM_(GF_ELEC), _bias_elec, NULL, 1 },
        { OF_IM_(GF_FIRE), _bias_fire, NULL, 1 },
        { OF_IM_(GF_COLD), _bias_cold, NULL, 1 },
        { 0 } };

    if (randint1(100) < (forge->lvl - 40))
    {
        _one(forge, tbl);
        forge->powers -= 2;
    }
    else
    {
        forge->powers++;
        _one_high_resist(forge);
        _many(forge, _one_high_resist);
    }
}

/******************************************************************************
 * ESP
 ******************************************************************************/
static void _one_low_esp(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_ESP_GOOD, NULL, NULL, 10 },
        { OF_ESP_DRAGON, NULL, NULL, 10 },
        { OF_ESP_DEMON, NULL, NULL, 10 },
        { OF_ESP_UNDEAD, NULL, NULL, 10 },
        { OF_ESP_ANIMAL, NULL, NULL, 10 },
        { OF_ESP_HUMAN, NULL, NULL, 10 },
        { OF_ESP_ORC, NULL, NULL, 10 },
        { OF_ESP_TROLL, NULL, NULL, 10 },
        { OF_ESP_GIANT, NULL, NULL, 10 },
        { OF_ESP_UNIQUE, NULL, NULL, 1 },
        { 0 } };

    _one(forge, tbl);
}

static void _esp(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_TELEPATHY, NULL, NULL, 1 },
        { OF_ESP_EVIL, NULL, NULL, 3 },
        { OF_ESP_NONLIVING, NULL, NULL, 3 },
        { 0 } };

    _one(forge, tbl);
    forge->powers++;
    _many(forge, _one_low_esp);
}

static void _one_esp(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0, _esp, obj_is_helmet, 10 },
        { 0, _esp, NULL, 1 },
        { 0, _one_low_esp, NULL, 20 },
        { 0 } };

    _one(forge, tbl);
}

/******************************************************************************
 * Slays
 ******************************************************************************/
static void _one_ele_brand(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_BRAND_ACID, NULL, NULL, 2 },
        { OF_BRAND_ELEC, NULL, NULL, 2 },
        { OF_BRAND_FIRE, NULL, NULL, 3 },
        { OF_BRAND_COLD, NULL, NULL, 3 },
        { 0 } };

    _one(forge, tbl);
}

static bool obj_is_sword(obj_ptr o) { return o->tval == TV_SWORD; }
static bool obj_is_hafted(obj_ptr o) { return o->tval == TV_HAFTED; }

static void _one_brand(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_BRAND_POIS,    NULL,             NULL,  400 },
        { OF_BRAND_COLD,    NULL,             NULL,  400 },
        { OF_BRAND_FIRE,    NULL,             NULL,  400 },
        { OF_VORPAL,        NULL,  obj_is_sword,     400 },
        { OF_BRAND_ACID,    NULL,             NULL,  400 },
        { OF_BRAND_ELEC,    NULL,             NULL,  400 },
        { OF_BRAND_CHAOS,   NULL,             NULL,  200 },
        { OF_IMPACT,        NULL, obj_is_hafted,     200 },
        { OF_STUN,          NULL, obj_is_hafted,      60 },
        { OF_BRAND_VAMP,    NULL,             NULL,  300 },
        { OF_BRAND_MANA,    NULL,             NULL,   10 },
        { OF_VORPAL2,       NULL,  obj_is_sword,      10 },
        { 0 } };

    _best_one(forge, tbl, 1 + forge->lvl/30);
}

static void _one_slay(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_SLAY_ORC,      NULL, NULL, 100 },
        { OF_KILL_ORC,      NULL, NULL,  25 },
        { OF_SLAY_TROLL,    NULL, NULL, 100 },
        { OF_KILL_TROLL,    NULL, NULL,  25 },
        { OF_SLAY_GOOD,     NULL, NULL,  75 },
        { OF_SLAY_ANIMAL,   NULL, NULL, 100 },
        { OF_SLAY_GIANT,    NULL, NULL, 100 },
        { OF_SLAY_HUMAN,    NULL, NULL, 100 },
        { OF_SLAY_DRAGON,   NULL, NULL, 100 },
        { OF_SLAY_UNDEAD,   NULL, NULL, 100 },
        { OF_SLAY_DEMON,    NULL, NULL, 100 },
        { OF_KILL_ANIMAL,   NULL, NULL,  15 },
        { OF_KILL_GIANT,    NULL, NULL,   5 },
        { OF_SLAY_LIVING,   NULL, NULL,  20 },
        { OF_SLAY_EVIL,     NULL, NULL,  50 },
        { OF_KILL_HUMAN,    NULL, NULL,   5 },
        { OF_KILL_DRAGON,   NULL, NULL,  10 },
        { OF_KILL_UNDEAD,   NULL, NULL,   4 },
        { OF_KILL_DEMON,    NULL, NULL,   4 },
        { OF_KILL_EVIL,     NULL, NULL,   1 },
        { 0 } };

    _best_one(forge, tbl, 1 + forge->lvl/30);
}

/******************************************************************************
 * Abilities
 ******************************************************************************/
static void _one_ability(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_LIGHT, NULL, NULL, 3 },
        { OF_SLOW_DIGEST, NULL, NULL, 2 },
        { OF_WARNING, NULL, NULL, 5 },
        { OF_SEE_INVIS, NULL, NULL, 2 },
        { OF_FREE_ACT, NULL, NULL, 2 },
        { OF_LEVITATION, NULL, NULL, 3 },
        { OF_HOLD_LIFE, NULL, NULL, 3 },
        { OF_REGEN, NULL, NULL, 3 },
        { OF_REFLECT, NULL, NULL, 5 },
        { 0 } };

    _best_one(forge, tbl, 1 + forge->lvl/40);
}

static void _one_brilliance(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_INT,          NULL, NULL,  9, _ONCE },
        { OF_WIS,          NULL, NULL,  9, _ONCE },
        { OF_CHR,          NULL, NULL,  9, _ONCE },
        { OF_SUST_INT, _freebie, NULL,  1, _ONCE },
        { OF_SUST_WIS, _freebie, NULL,  1, _ONCE },
        { OF_SUST_CHR, _freebie, NULL,  1, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _brilliance(_forge_ptr forge)
{
    forge->powers++;
    _one_brilliance(forge);
    _many(forge, _one_brilliance);
    forge->bias |= BIAS_MAGE;
}

static void _one_might(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_STR,          NULL, NULL,  9, _ONCE },
        { OF_DEX,          NULL, NULL,  9, _ONCE },
        { OF_CON,          NULL, NULL,  9, _ONCE },
        { OF_SUST_STR, _freebie, NULL,  1, _ONCE },
        { OF_SUST_DEX, _freebie, NULL,  1, _ONCE },
        { OF_SUST_CON, _freebie, NULL,  1, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _might(_forge_ptr forge)
{
    forge->powers++;
    _one_might(forge);
    _many(forge, _one_might);
    forge->bias |= BIAS_WARRIOR;
}

static void _one_plus(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_INFRA,   NULL, NULL,  6 },
        { OF_SEARCH,  NULL, NULL,  6 },
        { OF_STEALTH, NULL, NULL,  6 },
        { 0,     _one_stat, NULL, 20 },
        { OF_SPEED,   NULL, NULL,  2 },
        { 0 } };

    _best_one(forge, tbl, forge->lvl/40);
}

/******************************************************************************
 * Armor
 ******************************************************************************/
static void _boost_ac(_forge_ptr forge)
{
    int amt;
    if (obj_is_armor(forge->obj))
    {
        amt = 8;
        if (forge->obj->to_a >= 40) return;
        if (forge->obj->to_a >= 35) amt--;
        if (forge->obj->to_a >= 30) amt--;
        if (forge->obj->to_a >= 25) amt--;
        if (forge->obj->to_a >= 20) amt--;
        if (forge->obj->to_a >= 15) amt--;
    }
    else
    {
        amt = 5;
        if (forge->obj->to_a >= 20) return;
        if (forge->obj->to_a >= 15) amt--;
        if (forge->obj->to_a >= 10) amt--;
    }
    forge->obj->to_a += randint1(amt);
    forge->powers--;

    /* supercharge */
    if (forge->powers > 0 && one_in_(2)) _boost_ac(forge);
}

static void _slaying(_forge_ptr forge)
{
    int xtra = 0;

    if (have_flag(forge->obj->flags, OF_SPELL_DAM)) return;
    if (have_flag(forge->obj->flags, OF_ARCHERY)) return;

    add_flag(forge->obj->flags, OF_MELEE);

    forge->obj->to_h += randint1(5);
    forge->obj->to_d += randint1(5);
    if (forge->lvl >= 30)
        xtra += 3;
    if (forge->lvl >= 60 && one_in_(2))
        xtra += randint1(4);
    if (forge->lvl >= 90 && one_in_(3))
        xtra += randint1(5);
    if (xtra)
    {
        forge->obj->to_h += m_bonus(xtra, forge->lvl);
        forge->obj->to_d += m_bonus(xtra, forge->lvl);
    }
    if (obj_is_armor(forge->obj))
        add_flag(forge->obj->flags, OF_SHOW_MODS); /* Show (+0, +15) rather than (+15) */
    forge->powers--;
    forge->bias |= BIAS_WARRIOR;
}

static void _one_resist_base(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_RES_(GF_ACID), NULL, NULL,  3, _ONCE },
        { OF_RES_(GF_ELEC), NULL, NULL,  3, _ONCE },
        { OF_RES_(GF_FIRE), NULL, NULL,  3, _ONCE },
        { OF_RES_(GF_COLD), NULL, NULL,  3, _ONCE },
        { OF_RES_(GF_POIS), NULL, NULL,  1, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _resist_base(_forge_ptr forge)
{
    int start = ++forge->powers;
    _one_resist_base(forge);
    _many(forge, _one_resist_base);
    if (start - forge->powers > 3)
        forge->bias |= BIAS_ELEMENTAL;
}

static void _one_balance(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_RES_(GF_SOUND),  NULL, NULL,  1, _ONCE },
        { OF_RES_(GF_SHARDS), NULL, NULL,  1, _ONCE },
        { OF_RES_(GF_CHAOS),  NULL, NULL,  1, _ONCE },
        { OF_RES_(GF_DISENCHANT),  NULL, NULL,  1, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _resist_balance(_forge_ptr forge)
{
    forge->powers++;
    _many(forge, _one_balance);
}

static void _one_undead_power(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_RES_(GF_COLD),     NULL, NULL,  2, _ONCE },
        { OF_RES_(GF_POIS),     NULL, NULL,  3, _ONCE },
        { OF_RES_(GF_DARK),     NULL, NULL,  2, _ONCE },
        { OF_RES_(GF_NETHER),   NULL, NULL,  5, _ONCE },
        { OF_HOLD_LIFE,    NULL, NULL,  3, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _resist_undead(_forge_ptr forge)
{
    forge->powers++;
    _one_undead_power(forge);
    _many(forge, _one_undead_power);
    forge->bias |= BIAS_NECROMANTIC;
}

static bool obj_is_not_gloves(obj_ptr o) { return !obj_is_gloves(o); }

static void _one_armor(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,   _one_low_esp,                  NULL,  3 },
        { 0,   _one_sustain,                  NULL, 16 },
        { 0,      _boost_ac,                  NULL, 24 },
        { 0,   _one_ability,                  NULL,  3 },
        { 0,      _one_plus,                  NULL, 40 },
        { 0,    _sustaining,                  NULL,  3 },
        { 0,    _one_resist,                  NULL, 56 },
        { 0,  _many_resists,                  NULL, 12 },
        { 0,       _slaying,  obj_is_not_gloves,     1, _ONCE},
        { 0,  _one_immunity,                  NULL,  1, _ONCE},
        { 0 } };

    _best_one(forge, tbl, 1 + forge->lvl/45);
}

static void _one_boots(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_SPEED,          NULL, NULL,  3, _ONCE },
        { OF_STEALTH,        NULL, NULL,  6, _ONCE },
        { OF_FREE_ACT,   _freebie, NULL,  9, _ONCE },
        { OF_LEVITATION, _freebie, NULL,  9, _ONCE },
        { OF_DEX,            NULL, NULL,  3, _ONCE },
        { OF_RES_(GF_NEXUS),      NULL, NULL,  3, _ONCE },
        { OF_NO_TELE,        NULL, NULL,  1, _ONCE },
        { 0,           _one_armor, NULL, 45 },
        { 0 } };

    _one(forge, tbl);
}

static void _one_gloves(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,                _slaying, NULL, 100, _ONCE },
        { 0,          _one_ele_brand, NULL,   4, _ONCE },
        { OF_FREE_ACT,      _freebie, NULL, 100, _ONCE },
        { OF_MAGIC_MASTERY,     NULL, NULL,   4, _ONCE },
        { OF_BLOWS,             NULL, NULL,   1, _ONCE },
        { OF_NO_MAGIC,          NULL, NULL,   5, _ONCE },
        { OF_DEX,               NULL, NULL,  40, _ONCE },
        { OF_STR,               NULL, NULL,  20, _ONCE },
        { OF_TUNNEL,            NULL, NULL,  10, _ONCE },
        { 0,                  _might, NULL,  20, _ONCE },
        { 0,              _one_armor, NULL, 300 },
        { 0 } };

    _one(forge, tbl);
}

static void _one_helmet(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,            _one_esp, NULL,  6, _ONCE },
        { OF_RES_(GF_BLIND), _freebie, NULL,  9, _ONCE },
        { OF_SEE_INVIS, _freebie, NULL,  9, _ONCE },
        { OF_SEARCH,        NULL, NULL,  9, _ONCE },
        { OF_INFRA,         NULL, NULL,  9, _ONCE },
        { OF_INT,           NULL, NULL,  3, _ONCE },
        { OF_WIS,           NULL, NULL,  3, _ONCE },
        { OF_NO_MAGIC,      NULL, NULL,  1, _ONCE },
        { 0,         _brilliance, NULL,  3, _ONCE },
        { 0,              _might, NULL,  3, _ONCE },
        { 0,          _one_armor, NULL, 45 },
        { 0 } };

    _one(forge, tbl);
}

static void _one_cloak(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_STEALTH,       NULL, NULL,  6, _ONCE },
        { OF_LEVITATION,    NULL, NULL,  1, _ONCE }, /* a flying cape a la superman (for RACE_MON_HYDRA) */
        { 0,           _one_aura, NULL,  2 },
        { 0,          _one_armor, NULL, 30 },
        { 0 } };

    _one(forge, tbl);
}

static void _one_shield(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_REFLECT,       NULL, NULL,  2, _ONCE },
        { 0,        _resist_base, NULL,  1, _ONCE},
        { 0,          _one_armor, NULL, 15 },
        { 0 } };

    _one(forge, tbl);
}

static void _body_armor_resist(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,        _resist_base, NULL, 20 },
        { 0,  _many_high_resists, NULL, 15 },
        { 0,     _resist_balance, NULL,  1 },
        { 0,      _resist_undead, NULL,  1 },
        { 0 } };
    _one(forge, tbl);
}
static void _one_body_armor(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,           _boost_ac, NULL, 15, _ONCE },
        { 0,  _body_armor_resist, NULL, 35, _ONCE },
        { 0,        _one_sustain, NULL,  3 },
        { 0,           _one_aura, NULL,  3 },
        { 0,              _might, NULL,  3, _ONCE },
        { 0,          _one_armor, NULL, 45 },
        { 0 } };
    _one(forge, tbl);
}

/* Rogue Armor */
static void _one_rogue_resist(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_RES_(GF_POIS),  NULL, NULL,  5, _ONCE },
        { OF_RES_(GF_DARK),  NULL, NULL,  5, _ONCE },
        { OF_RES_(GF_CONFUSION),  NULL, NULL,  2, _ONCE },
        { OF_RES_(GF_NEXUS), NULL, NULL,  2, _ONCE },
        { OF_RES_(GF_DISENCHANT), NULL, NULL,  1, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _one_rogue_stat(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_STR, NULL, NULL, 1, _ONCE },
        { OF_INT, NULL, NULL, 2, _ONCE },
        { OF_DEX, NULL, NULL, 6, _ONCE },
        { OF_CON, NULL, NULL, 1, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _one_rogue_armor(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,   _one_rogue_resist, NULL, 10 },
        { 0,     _one_rogue_stat, NULL, 10 },
        { OF_STEALTH,   _freebie, NULL, 15, _ONCE },
        { OF_SEARCH,    _freebie, NULL, 15, _ONCE },
        { OF_INFRA,     _freebie, NULL, 10, _ONCE },
        { OF_SPEED,         NULL, NULL,  5, _ONCE },
        { OF_SUST_DEX,  _freebie, NULL, 15, _ONCE },
        { OF_DARKNESS,  _freebie, NULL,  5, _ONCE },
        { 0,            _one_esp, NULL, 10, _ONCE },
        { 0,           _boost_ac, NULL, 10, _ONCE },
        { 0,          _one_armor, NULL,  3 },
        { 0 } };

    _one(forge, tbl);
    forge->bias = BIAS_ROGUE;
}

/* Demon Armor */
static void _one_demon_resist(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_RES_(GF_FIRE),  NULL, NULL,  3, _ONCE },
        { OF_RES_(GF_CONFUSION),  NULL, NULL,  1, _ONCE },
        { OF_RES_(GF_NEXUS), NULL, NULL,  2, _ONCE },
        { OF_RES_(GF_CHAOS), NULL, NULL,  3, _ONCE },
        { OF_RES_(GF_DISENCHANT), NULL, NULL,  2, _ONCE },
        { OF_RES_(GF_FEAR),  NULL, NULL,  2, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _one_demon_stat(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_STR,             NULL, NULL, 6, _ONCE },
        { OF_INT,             NULL, NULL, 3, _ONCE },
        { OF_DEX,             NULL, NULL, 1, _ONCE },
        { OF_CON,             NULL, NULL, 1, _ONCE },
        { OF_DEC_WIS,     _freebie, NULL, 1, _ONCE },
        { OF_DEC_STEALTH, _freebie, NULL, 1, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _demon_immunity(_forge_ptr forge)
{
    if (randint1(100) < (forge->lvl - 35))
    {
        add_flag(forge->obj->flags, OF_IM_(GF_FIRE));
        forge->powers--;
    }
    else
    {
        forge->powers++;
        _one_demon_resist(forge);
        _many(forge, _one_demon_resist);
    }
}
static void _one_demon_armor(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,   _one_demon_resist, NULL, 10 },
        { 0,     _one_demon_stat, NULL, 10 },
        { OF_AURA_FIRE, _freebie, NULL, 15, _ONCE },
        { OF_RES_(GF_FIRE),  _freebie, NULL, 25, _ONCE },
        { 0,            _slaying, NULL, 10, _ONCE },
        { 0,           _boost_ac, NULL, 10, _ONCE },
        { 0,     _demon_immunity, NULL,  1, _ONCE },
        { OF_SLAY_GOOD,     NULL, NULL,  1, _ONCE },
        { 0,          _one_armor, NULL,  3 },
        { 0 } };

    _one(forge, tbl);
    forge->bias = BIAS_DEMON;
}

/* Undead Armor */
static void _one_undead_resist(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_RES_(GF_COLD),   NULL, NULL,  3, _ONCE },
        { OF_RES_(GF_POIS),   NULL, NULL,  3, _ONCE },
        { OF_RES_(GF_DARK),   NULL, NULL,  3, _ONCE },
        { OF_RES_(GF_NETHER), NULL, NULL,  6, _ONCE },
        { OF_RES_(GF_DISENCHANT),  NULL, NULL,  1, _ONCE },
        { OF_RES_(GF_FEAR),   NULL, NULL,  1, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _one_undead_stat(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_STR,         NULL, NULL, 2, _ONCE },
        { OF_INT,         NULL, NULL, 3, _ONCE },
        { OF_CON,         NULL, NULL, 3, _ONCE },
        { OF_CHR,         NULL, NULL, 2, _ONCE },
        { OF_DEC_WIS, _freebie, NULL, 1, _ONCE },
        { OF_STEALTH,     NULL, NULL, 3, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _undead_immunity(_forge_ptr forge)
{
    if (randint1(100) < (forge->lvl - 35))
    {
        add_flag(forge->obj->flags, OF_IM_(GF_COLD));
        forge->powers--;
    }
    else
    {
        forge->powers++;
        _one_undead_resist(forge);
        _many(forge, _one_undead_resist);
    }
}

static void _one_undead_armor(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,  _one_undead_resist, NULL, 10 },
        { 0,    _one_undead_stat, NULL, 10 },
        { OF_AURA_COLD, _freebie, NULL, 15, _ONCE },
        { OF_RES_(GF_COLD),  _freebie, NULL, 25, _ONCE },
        { 0,            _slaying, NULL, 10, _ONCE },
        { 0,           _boost_ac, NULL, 10, _ONCE },
        { 0,    _undead_immunity, NULL,  1, _ONCE },
        { OF_BRAND_VAMP,    NULL, NULL,  1, _ONCE },
        { 0,          _one_armor, NULL,  3 },
        { 0 } };

    _one(forge, tbl);
    forge->bias = BIAS_NECROMANTIC;
}

/* Holy Armor */
static void _one_holy_resist(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,        _resist_base, NULL, 10, _ONCE },
        { OF_RES_(GF_LIGHT),      NULL, NULL,  3, _ONCE },
        { OF_RES_(GF_SOUND),     NULL, NULL,  3, _ONCE },
        { OF_RES_(GF_SHARDS),    NULL, NULL,  3, _ONCE },
        { OF_RES_(GF_DISENCHANT),     NULL, NULL,  3, _ONCE },
        { OF_RES_(GF_FEAR),      NULL, NULL,  3, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _one_holy_stat(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_STR,         NULL, NULL, 1, _ONCE },
        { OF_WIS,         NULL, NULL, 7, _ONCE },
        { OF_CHR,         NULL, NULL, 7, _ONCE },
        { 0,       _sustaining, NULL, 1, _ONCE },
        { OF_SPEED,       NULL, NULL, 1, _ONCE },
        { OF_LIFE,        NULL, NULL, 3, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _one_holy_armor(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,    _one_holy_resist, NULL, 10 },
        { 0,      _one_holy_stat, NULL, 10 },
        { OF_HOLD_LIFE, _freebie, NULL, 10, _ONCE },
        { OF_BLESSED,   _freebie, NULL, 10, _ONCE },
        { 0,            _slaying, NULL, 10, _ONCE },
        { 0,           _boost_ac, NULL, 10, _ONCE },
        { 0,           _boost_ac, NULL, 10, _ONCE },
        { 0,          _one_armor, NULL,  3 },
        { 0 } };

    _one(forge, tbl);
    forge->bias = BIAS_PRIESTLY;
}


/* Mage Robes */
static void _bias_mage(_forge_ptr forge) { forge->bias |= BIAS_MAGE; }
static void _one_robe(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_DEC_MANA, _bias_mage, NULL,  1, _ONCE },
        { OF_INT,            NULL, NULL,  5, _ONCE },
        { OF_HOLD_LIFE,  _freebie, NULL,  5, _ONCE },
        { 0,          _sustaining, NULL,  5 },
        { 0,            _boost_ac, NULL,  5 },
        { 0,         _resist_base, NULL,  5, _ONCE},
        { 0,            _one_aura, NULL,  5 },
        { 0,          _brilliance, NULL,  5, _ONCE },
        { 0,           _one_armor, NULL, 50 },
        { 0 } };

    _one(forge, tbl);
}

/******************************************************************************
 * Weapons and Ammo
 ******************************************************************************/
static void _boost_dam(_forge_ptr forge)
{
    int amt = 6;
    if (forge->obj->to_d >= 30) return;
    if (forge->obj->to_d >= 25) amt--;
    if (forge->obj->to_d >= 20) amt--;
    if (forge->obj->to_d >= 15) amt--;
    forge->obj->to_d += randint1(amt);
    forge->powers--;

    /* supercharge */
    if (forge->powers > 0 && one_in_(2)) _boost_dam(forge);
}

static void _boost_hit(_forge_ptr forge)
{
    int amt = 6;
    if (forge->obj->to_h >= 30) return;
    if (forge->obj->to_h >= 25) amt--;
    if (forge->obj->to_h >= 20) amt--;
    if (forge->obj->to_h >= 15) amt--;
    forge->obj->to_h += randint1(amt);
    forge->powers--;

    /* supercharge */
    if (forge->powers > 0 && one_in_(2)) _boost_hit(forge);
}

static void _boost_dice(_forge_ptr forge)
{
    object_kind *k_ptr = &k_info[forge->obj->k_idx];
    int          max = MIN(81, k_ptr->dd * k_ptr->ds * 4); /* 9d9 */
    int          dd = forge->obj->dd;
    int          ds = forge->obj->ds;

    dd++;
    while (randint0(1000) < 4000 / (dd * ds)) /* <--> 1 in (dd*ds/4) */
    {
        if (one_in_(2))
        {
            dd++;
            if (dd * ds > max)
            {
                dd--;
                break;
            }
        }
        else
        {
            ds++;
            if (dd * ds > max)
            {
                ds--;
                break;
            }
        }
        forge->powers--;
    }
    forge->obj->dd = dd;
    forge->obj->ds = ds;
    forge->powers--;
}

static void _one_defender(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,        _one_ability, NULL,  5 },
        { 0,           _boost_ac, NULL, 10 },
        { 0,         _one_resist, NULL, 20 },
        { 0,           _one_plus, NULL,  5 },
        { 0,        _one_sustain, NULL,  5 },
        { 0,       _many_resists, NULL, 10 },
        { 0,        _resist_base, NULL,  5, _ONCE },
        { 0,         _sustaining, NULL,  5, _ONCE },
        { OF_STEALTH,       NULL, NULL,  5, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _defender(_forge_ptr forge)
{
    forge->powers++;
    _one_defender(forge);
    _many(forge, _one_defender);
    forge->bias |= BIAS_PROTECTION;
}

static void _one_weapon(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,          _boost_hit, NULL, 10 },
        { 0,        _one_ability, NULL,  5 },
        { 0,           _boost_ac, NULL,  2 },
        { 0,          _boost_dam, NULL, 15 },
        { 0,         _one_resist, NULL, 20 },
        { 0,           _one_plus, NULL, 20 },
        { 0,          _one_brand, NULL, 15 },
        { 0,           _one_slay, NULL, 30 },
        { 0,           _defender, NULL,  3, _ONCE },
        { 0,         _boost_dice, NULL, 15, _ONCE },
        { 0 } };

    _best_one(forge, tbl, 1 + forge->lvl/45);
}

static void _one_demon_weapon(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,    _one_demon_resist, NULL, 16 },
        { 0,      _one_demon_stat, NULL, 16 },
        { OF_BRAND_FIRE, _freebie, NULL, 45, _ONCE },
        { OF_BRAND_PLASMA,   NULL, NULL,  2, _ONCE },
        { OF_RES_(GF_FIRE),   _freebie, NULL, 45, _ONCE },
        { 0,          _boost_dice, NULL, 30, _ONCE },
        { 0,           _boost_dam, NULL, 30, _ONCE },
        { 0,      _demon_immunity, NULL,  3, _ONCE },
        { OF_SLAY_GOOD,      NULL, NULL, 16, _ONCE },
        { 0,          _one_weapon, NULL, 16 },
        { 0 } };

    _one(forge, tbl);
    forge->bias = BIAS_DEMON;
}

static void _one_undead_weapon(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,   _one_undead_resist, NULL, 16 },
        { 0,     _one_undead_stat, NULL, 16 },
        { OF_BRAND_COLD, _freebie, NULL, 45, _ONCE },
        { OF_BRAND_DARK,     NULL, NULL,  2, _ONCE },
        { OF_RES_(GF_COLD),   _freebie, NULL, 45, _ONCE },
        { 0,          _boost_dice, NULL, 30, _ONCE },
        { 0,           _boost_dam, NULL, 30, _ONCE },
        { 0,     _undead_immunity, NULL,  3, _ONCE },
        { OF_BRAND_VAMP,     NULL, NULL, 45, _ONCE },
        { OF_SLAY_GOOD,      NULL, NULL, 16, _ONCE },
        { 0,          _one_weapon, NULL, 16 },
        { 0 } };

    _one(forge, tbl);
    forge->bias = BIAS_NECROMANTIC;
}

static void _one_holy_slay(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_SLAY_UNDEAD,   NULL, NULL, 100 },
        { OF_SLAY_DEMON,    NULL, NULL, 100 },
        { OF_SLAY_EVIL,     NULL, NULL,  50 },
        { OF_KILL_UNDEAD,   NULL, NULL,   4 },
        { OF_KILL_DEMON,    NULL, NULL,   4 },
        { OF_BRAND_LIGHT,   NULL, NULL,   2 },
        { OF_KILL_EVIL,     NULL, NULL,   1 },
        { 0 } };

    _best_one(forge, tbl, 1 + forge->lvl/30);
}

static void _one_holy_weapon(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,     _one_holy_resist, NULL, 17 },
        { 0,       _one_holy_stat, NULL, 17 },
        { 0,       _one_holy_slay, NULL, 17 },
        { 0,          _boost_dice, NULL, 30, _ONCE },
        { 0,           _boost_dam, NULL, 30, _ONCE },
        { OF_BLESSED,    _freebie, NULL, 30, _ONCE },
        { 0,          _one_weapon, NULL,  7 },
        { 0 } };

    _one(forge, tbl);
    forge->bias = BIAS_PRIESTLY;
}

static void _one_wizardstaff(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { OF_SPELL_CAP,           NULL, NULL,  5, _ONCE },
        { OF_SPELL_POWER,         NULL, NULL,  1, _ONCE },
        { OF_MAGIC_MASTERY,       NULL, NULL,  1, _ONCE },
        { 0,               _brilliance, NULL,  2, _ONCE },
        { OF_INT,             _freebie, NULL,  5, _ONCE },
        { OF_SUST_INT,        _freebie, NULL,  5, _ONCE },
        { OF_EASY_SPELL,      _freebie, NULL,  5, _ONCE },
        { OF_RES_(GF_CONFUSION),        _freebie, NULL,  5, _ONCE },
        { OF_RES_(GF_BLIND),       _freebie, NULL,  5, _ONCE },
        { 0,               _one_resist, NULL,  5 }, 
        { 0,              _one_ability, NULL,  5 },
        { 0,                 _one_plus, NULL,  5 },
        { 0 } };

    _one(forge, tbl);
    forge->bias = BIAS_MAGE;
}

static void _one_ammo(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,          _boost_hit, NULL, 10 },
        { 0,          _boost_dam, NULL, 15 },
        { 0,         _boost_dice, NULL, 15, _ONCE },
        { 0,           _one_slay, NULL, 30 },
        { 0,          _one_brand, NULL, 15 },
        { 0 } };

    _one(forge, tbl);
}

/******************************************************************************
 * Bows
 ******************************************************************************/
static void _xtra_might(_forge_ptr forge)
{
    if (have_flag(forge->obj->flags, OF_XTRA_SHOTS) && randint1(100) > forge->lvl - 40)
        return;

    /* double check _ONCE. also, could be upgrading an ego */
    if (forge->obj->mult > k_info[forge->obj->k_idx].mult)
        return;

    forge->obj->mult += (25 + m_bonus(25, forge->lvl) + m_bonus(50, forge->lvl)) * bow_energy(forge->obj->sval) / 10000;
    forge->powers--;
}

static void _xtra_shots(_forge_ptr forge)
{
    if (forge->obj->mult > k_info[forge->obj->k_idx].mult && randint1(100) > forge->lvl - 40)
        return;
    add_flag(forge->obj->flags, OF_XTRA_SHOTS);
    forge->powers--;
}

static void _one_bow(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,          _boost_hit, NULL, 15 },
        { 0,        _one_ability, NULL, 15 },
        { 0,          _boost_dam, NULL, 15 },
        { 0,         _one_resist, NULL, 15 },
        { 0,           _one_plus, NULL, 15 },
        { 0,         _xtra_might, NULL,  5, _ONCE },
        { 0,         _xtra_shots, NULL,  5, _ONCE },
        { 0 } };

    _best_one(forge, tbl, 1 + forge->lvl/45);
}

/******************************************************************************
 * Harps (... are not bows)
 ******************************************************************************/
static void _clairvoyance(_forge_ptr forge) { effect_add(forge->obj, EFFECT_CLAIRVOYANCE); }
static void _one_harp(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,                  _one_plus, NULL, 200 },
        { 0,               _one_ability, NULL, 150 },
        { 0,                _one_resist, NULL, 300 },
        { OF_RES_(GF_SOUND),   _freebie, NULL,  50, _ONCE },
        { OF_SPELL_CAP,            NULL, NULL,  10 },
        { 0,                _brilliance, NULL,   5, _ONCE },
        { 0,                   _one_esp, NULL,   5 },
        { 0,              _clairvoyance, NULL,   2, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

/******************************************************************************
 * Lights and Jewelry
 ******************************************************************************/
static void _one_light(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,              _one_plus, NULL, 200 },
        { 0,           _one_ability, NULL, 150 },
        { 0,            _one_resist, NULL, 300 },
        { OF_HOLD_LIFE,    _freebie, NULL,  50, _ONCE },
        { OF_SPELL_POWER,      NULL, NULL,   1, _ONCE },
        { 0,                   _esp, NULL,   5, _ONCE },
        { 0,          _clairvoyance, NULL,   2, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _one_dark(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,       _one_undead_stat, NULL, 200 },
        { 0,     _one_undead_resist, NULL, 250 },
        { 0,            _one_resist, NULL,  50 },
        { OF_HOLD_LIFE,    _freebie, NULL,  50, _ONCE },
        { OF_SEE_INVIS,    _freebie, NULL,  50, _ONCE },
        { OF_AURA_COLD,    _freebie, NULL,  15, _ONCE },
        { OF_RES_(GF_COLD),     _freebie, NULL,  15, _ONCE },
        { OF_FREE_ACT,         NULL, NULL,  25, _ONCE },
        { OF_SLOW_DIGEST,      NULL, NULL,  25, _ONCE },
        { OF_LEVITATION,       NULL, NULL,  25, _ONCE },
        { OF_REGEN,            NULL, NULL,  25, _ONCE },
        { OF_SPEED,            NULL, NULL,  25, _ONCE },
        { OF_SPELL_POWER,      NULL, NULL,   5, _ONCE },
        { 0,               _slaying, NULL,   5, _ONCE },
        { 0,                   _esp, NULL,   5, _ONCE },
        { 0 } };

    _one(forge, tbl);
    forge->bias = BIAS_NECROMANTIC;
}

static void _one_ring(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,               _one_plus, NULL, 300 },
        { 0,            _one_ability, NULL, 150 },
        { 0,             _one_resist, NULL, 300 },
        { 0,                _slaying, NULL, 100, _ONCE },
        { 0,                _slaying, NULL, 100, _ONCE },
        { 0,               _boost_ac, NULL, 200 },
        { 0,           _one_immunity, NULL,  75, _ONCE },
        { 0,            _resist_base, NULL,  50, _ONCE },
        { 0,      _many_high_resists, NULL, 200, _ONCE },
        { 0,                  _might, NULL,  50, _ONCE },
        { OF_FREE_ACT,      _freebie, NULL,  50, _ONCE },
        { OF_SEE_INVIS,     _freebie, NULL,  50, _ONCE },
        { OF_SPEED,             NULL, NULL,  50, _ONCE },
        { OF_XTRA_MIGHT,        NULL, NULL,  10, _ONCE },
        { OF_XTRA_SHOTS,        NULL, NULL,  10, _ONCE },
        { OF_WEAPONMASTERY,     NULL, NULL,   2, _ONCE },
        { OF_BLOWS,             NULL, NULL,   1, _ONCE },
        { 0 } };

    _one(forge, tbl);
}

static void _one_amulet(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0,               _one_plus, NULL, 300 },
        { 0,            _one_ability, NULL, 150 },
        { 0,             _one_resist, NULL, 300 },
        { 0,                _slaying, NULL, 100, _ONCE },
        { 0,               _boost_ac, NULL, 200 },
        { 0,            _resist_base, NULL,  50, _ONCE },
        { 0,      _many_high_resists, NULL, 200, _ONCE },
        { 0,             _brilliance, NULL,  50, _ONCE },
        { OF_TELEPATHY,         NULL, NULL,  10, _ONCE },
        { OF_FREE_ACT,      _freebie, NULL,  50, _ONCE },
        { OF_HOLD_LIFE,     _freebie, NULL,  50, _ONCE },
        { OF_REFLECT,           NULL, NULL,  25, _ONCE },
        { OF_MAGIC_MASTERY,     NULL, NULL,  10, _ONCE },
        { OF_NO_TELE,           NULL, NULL,   5, _ONCE },
        { OF_NO_MAGIC,          NULL, NULL,   5, _ONCE },
        { 0 } };

    _one(forge, tbl);
}
/******************************************************************************
 * One Power based on object kind 
 ******************************************************************************/
static bool obj_is_robe(obj_ptr o) { return obj_is_(o, TV_SOFT_ARMOR, SV_ROBE); }
static bool obj_is_wizardstaff(obj_ptr o) { return obj_is_(o, TV_HAFTED, SV_WIZSTAFF); }
static bool obj_is_hard_armor(obj_ptr o) { return o->tval == TV_HARD_ARMOR; }
static bool obj_is_soft_armor(obj_ptr o) { return o->tval == TV_SOFT_ARMOR; }
static bool _obj_is_light(obj_ptr o) { return o->tval == TV_LIGHT && o->sval != SV_LIGHT_DARK; }
static bool _obj_is_dark(obj_ptr o) { return o->tval == TV_LIGHT && o->sval == SV_LIGHT_DARK; }
static bool _obj_is_harp(obj_ptr o) { return o->tval == TV_BOW && o->sval == SV_HARP; }
static bool _obj_is_bow(obj_ptr o) { return  o->tval == TV_BOW && o->sval != SV_HARP; }

/* Pick the forge function for this artifact. The goal here is twofold:
 * [1] To pick a forge function appropriate to the kind of object being forged
 * [2] To allow for themed artifact creation */
static _forge_f _choose_forge_f(_forge_ptr forge)
{
    static _table_t tbl[] = {
        { 0, _one_boots,        obj_is_boots,          1 },
        { 0, _one_gloves,       obj_is_gloves,         1 },
        { 0, _one_helmet,       obj_is_helmet,         1 },
        { 0, _one_cloak,        obj_is_cloak,          1 },
        { 0, _one_shield,       obj_is_shield,         1 },

        { 0, _one_robe,         obj_is_robe,        2000 }, /* XXX make mage gear more common XXX */
        { 0, _one_body_armor,   obj_is_body_armor,   100 },
        { 0, _one_demon_armor,  obj_is_hard_armor,     5 },
        { 0, _one_undead_armor, obj_is_body_armor,     5 },
        { 0, _one_holy_armor,   obj_is_hard_armor,     5 },
        { 0, _one_rogue_armor,  obj_is_soft_armor,    15 },

        { 0, _one_light,        _obj_is_light,         1 },
        { 0, _one_dark,         _obj_is_dark,          1 },
        { 0, _one_ring,         obj_is_ring,           1 },
        { 0, _one_amulet,       obj_is_amulet,         1 },

        { 0, _one_wizardstaff,  obj_is_wizardstaff, 1000 }, /* XXX make mage gear more common XXX */
        { 0, _one_weapon,       obj_is_weapon,       100 },
        { 0, _one_demon_weapon, obj_is_weapon,         5 }, 
        { 0, _one_undead_weapon,obj_is_weapon,         5 }, 
        { 0, _one_holy_weapon,  obj_is_weapon,         5 }, 

        { 0, _one_harp,         _obj_is_harp,          1 },
        { 0, _one_bow,          _obj_is_bow,           1 },
        { 0, _one_ammo,         obj_is_ammo,           1 },
        { 0 } };

    _table_ptr entry = _table_choose(forge, tbl, 1);
    if (entry) return entry->f;

    return _one_armor; /* XXX Bug! */
}

/******************************************************************************
 * Slot Powers: Vary object quality based on slot type
 ******************************************************************************/
typedef struct {
    cptr  name;
    obj_p pred;
    int   weight;
} _slot_weight_t, *_slot_weight_ptr;
static _slot_weight_t _slot_weight_tbl[] = {
    {"Weapons",    obj_is_weapon,     100},
    {"Shields",    obj_is_shield,      57},
    {"Bows",       obj_is_bow,         57},
    {"Rings",      obj_is_ring,        50},
    {"Amulets",    obj_is_amulet,      50},
    {"Lights",     obj_is_light,       38},
    {"Body Armor", obj_is_body_armor, 100},
    {"Cloaks",     obj_is_cloak,       44},
    {"Helmets",    obj_is_helmet,      50},
    {"Gloves",     obj_is_gloves,      44},
    {"Boots",      obj_is_boots,       50},
    {NULL}
};
static int _get_slot_pct(obj_ptr obj)
{
    int i;
    for (i = 0; ; i++)
    {
        _slot_weight_ptr row = &_slot_weight_tbl[i];
        if (!row->name) break;
        if (row->pred(obj)) return row->weight;
    }
    return 100;
}

static int _slot_normalize(int amt, int pct)
{
    int n = amt * 10 * pct / 100;
    amt = n / 10;
    if (n%10 && randint0(10) < n%10) amt++;
    return amt;
}

static int _roll_powers(obj_ptr obj, int lev)
{
    int powers;
    int pct = _get_slot_pct(obj);
    int spread = _slot_normalize(6, pct);
    int max_powers = _slot_normalize(10, pct);

    lev = MAX(1, lev);
    powers = randint1(spread) + 1;

    while (one_in_(powers) || one_in_(7 * 90/lev) || one_in_(10 * 70/lev))
        powers++;

    if (one_in_(GREAT_OBJ))
        powers *= 2;

    if (powers > max_powers)
        powers = max_powers;

    return MAX(3, powers);
}

static int _roll_ego_powers(obj_ptr obj, int lev)
{
    int p = _roll_powers(obj, lev);
    p = (p + 1) / 2;
    return MAX(2, p);
}

int get_slot_power(obj_ptr obj) /* for ego.c */
{
    int w = _get_slot_pct(obj);
    if (obj_is_weapon(obj))
    {
        int d = k_info[obj->k_idx].dd * k_info[obj->k_idx].ds;
        if (obj_is_wizardstaff(obj)) d = 6; /* fudge ... but allow strong mage gear */
        if (d < 12)
            w = w * d / 12;
    }
    else if (obj_is_body_armor(obj))
    {
        int ac = k_info[obj->k_idx].ac;
        if (ac < 16)
            w = w * (ac + 20) / 36;
    }
    return w;
}

/******************************************************************************
 * Finalize
 ******************************************************************************/
static bool _speed_check(_forge_ptr forge)
{
    if (forge->obj->tval != TV_BOOTS && forge->obj->tval != TV_RING) return FALSE;
    if (!have_flag(forge->obj->flags, OF_SPEED)) return FALSE;
    if (of_has_nonspeed_pval(forge->obj->flags)) return FALSE;
    return TRUE;
}
static void _pval(_forge_ptr forge)
{
    if (!of_has_pval(forge->obj->flags))
    {
        if (!of_has_pval(k_info[forge->obj->k_idx].flags)) /* e.g. Elven Cloak */
            forge->obj->pval = 0;
        return;
    }

    /* Roll the pval */
    if (have_flag(forge->obj->flags, OF_BLOWS))
    {
        if (forge->obj->tval == TV_RING || forge->obj->tval == TV_GLOVES)
        {
            forge->obj->pval = 1;
            if (one_in_(16)) forge->obj->pval++;
        }
        else
        {
            forge->obj->pval = randint1(3);
            if (one_in_(15)) forge->obj->pval++;
            if (obj_is_(forge->obj, TV_SWORD, SV_FALCON_SWORD))
                forge->obj->pval += randint1(2);
        }
    }
    else
    {
        /* Hengband:  1: 0.0%  2:20.0%  3:32.0%  4+:48.0%
           Chengband: 1:16.7%  2:28.6%  3:28.6%  4+:26.0%
           4+ usually becomes a 4. The only tweak I made was changing
           "|| one_in_(pval)" to "|| one_in_(pval+5)."
        */
        if (forge->mode & AM_CRAFTING)
            forge->obj->pval = 0; /* Surprise! Always re-roll the pval ... */
        /* else we are possibly improving a pre-existing pval! */

        if (_speed_check(forge))
        {
            while (forge->obj->pval < 2 + randint1(10) || one_in_(1 + forge->obj->pval/2))
                forge->obj->pval++;
        }
        else
        {
            while (forge->obj->pval < randint1(5) || one_in_(5 + forge->obj->pval))
                forge->obj->pval++;
        }
    }

    /* Limit the pval unless lucky */
    if (_speed_check(forge))
    {
    }
    else if (forge->obj->pval > 4 && !one_in_(GREAT_OBJ))
        forge->obj->pval = 4;

    if (forge->obj->tval == TV_DIGGING && !have_flag(forge->obj->flags, OF_BLOWS))
    {
        forge->obj->pval += randint1(2);
        if (forge->obj->pval > 6)
            forge->obj->pval = 6;
    }

    if (have_flag(forge->obj->flags, OF_WEAPONMASTERY) && forge->obj->pval > 2)
    {
        if (one_in_(6))
            forge->obj->pval = 3;
        else
            forge->obj->pval = 2;
    }

    if (have_flag(forge->obj->flags, OF_MAGIC_MASTERY) && forge->obj->pval > 2)
    {
        if (one_in_(30))
            forge->obj->pval = 3;
        else
            forge->obj->pval = 2;
    }

    if (have_flag(forge->obj->flags, OF_SPELL_POWER) && forge->obj->pval > 2)
    {
        if (one_in_(30))
            forge->obj->pval = 3;
        else
            forge->obj->pval = 2;
    }
}

static int _tier(_forge_ptr forge)
{
    int  cost = new_object_cost(forge->obj, COST_REAL);
    int  tier = 0;
    int  pct = _get_slot_pct(forge->obj);
    int  hi = 50000 * pct / 100;
    int  mid = 10000 * pct / 100;

    if (obj_is_weapon(forge->obj))
    {
        hi = hi * 7 / 5;
        mid = mid * 7 / 5;
    }

    if (cost >= hi) tier = TIER_HIGH;
    else if (cost >= mid) tier = TIER_MED;
    else if (cost > 0) tier = TIER_LOW;
    else tier = TIER_CURSED;

    return tier;
}


static bool _get_random_name(_forge_ptr forge, int tier, char *result)
{
    strcpy(result, art_get_name(forge->obj, forge->bias));
    return TRUE;
}

static void _name(_forge_ptr forge)
{
    /* use ego specific names if art_create_ego()
     * Note that art_name_ego() will avoid duplicates if possible */
    if (forge->ego && !(forge->mode & AM_CRAFTING))
    {
        int tier = _tier(forge);
        if (tier == TIER_HIGH && forge->ego->art_names_high)
        {
            cptr name = art_get_name_ego(forge->ego->art_names_high);
            forge->obj->art_name = quark_add(name);
        }
        else if (forge->ego->art_names)
        {
            cptr name = art_get_name_ego(forge->ego->art_names);
            forge->obj->art_name = quark_add(name);
        }
    }
    /* use art names from art_name.c if art_create_random() or ?ArtifactCreation */
    if (!forge->obj->art_name)
    {
        char name[80];
        int  tier = _tier(forge);

        if (tier == TIER_HIGH)
        {
            if (one_in_(17))
                add_flag(forge->obj->flags, OF_AGGRAVATE);
        }

        if (!_get_random_name(forge, tier, name))
            sprintf(name, "'%s'", player_name);

        if (forge->mode & AM_CRAFTING)
        {
            obj_identify_fully(forge->obj);
            obj_display(forge->obj);

            if ( !get_string("What do you want to call the artifact? ", name, sizeof name)
              || !name[0])
            {
                if (!_get_random_name(forge, tier, name))
                    sprintf(name, "'%s'", player_name);
            }

            virtue_add(VIRTUE_INDIVIDUALISM, 2);
            virtue_add(VIRTUE_ENCHANTMENT, 5);
        }
        else
        {
            if (forge->mode & AM_CURSED)
            {
                curse_object(forge->obj);
                if (of_has_pval(forge->obj->flags) && !forge->obj->pval) /* DEC_STR, etc. */
                    forge->obj->pval = randint1(5);
            }
        }
        if (name[0] != '\'' && name[0] != '(' && strstr(name, "of ") != name)
            forge->obj->art_name = quark_add(format("'%s'", name));
        else
            forge->obj->art_name = quark_add(name);
    }
}

static void _ignore(_forge_ptr forge)
{
    add_flag(forge->obj->flags, OF_IGNORE_ACID);
    add_flag(forge->obj->flags, OF_IGNORE_ELEC);
    add_flag(forge->obj->flags, OF_IGNORE_FIRE);
    add_flag(forge->obj->flags, OF_IGNORE_COLD);
}

static void _activation(_forge_ptr forge)
{
    if ( !obj_has_effect(forge->obj)
      && !obj_is_ammo(forge->obj) )
    {
        int odds = ACTIVATION_CHANCE;
        if (obj_is_armor(forge->obj))
            odds *= 2;
        if (one_in_(odds))
        {
            effect_add_random(forge->obj, forge->bias);
        }
    }
}

static void _hack(_forge_ptr forge)
{
    if (have_flag(forge->obj->flags, OF_BRAND_FIRE) || have_flag(forge->obj->flags, OF_BRAND_LIGHT))
        add_flag(forge->obj->flags, OF_LIGHT);

    if (obj_is_weapon(forge->obj))
        ego_weapon_adjust_weight(forge->obj);

    if (forge->obj->tval == TV_BOW)
    {
        /* rescale damage ... heavy crossbows shoot 0.75 while slings shoot 1.40x
         * damage on the bow should reflect this! */
        forge->obj->to_d = forge->obj->to_d * bow_energy(forge->obj->sval) / 7150;
    }

}

static void _flag_cancel(_forge_ptr forge, int flag1, int flag2)
{
    if ( have_flag(forge->obj->flags, flag1)
      && have_flag(forge->obj->flags, flag2) )
    {
        remove_flag(forge->obj->flags, flag1);
        remove_flag(forge->obj->flags, flag2);
    }
}

static void _flag_superfluous(_forge_ptr forge, int flag1, int flag2)
{
    if ( have_flag(forge->obj->flags, flag1)
      && have_flag(forge->obj->flags, flag2) )
    {
        remove_flag(forge->obj->flags, flag2);
    }
}

static void _cleanup(_forge_ptr forge)
{
    int i;

    /* remove flags that cancel each other out */
    for (i = 0; i < 6; i++)
        _flag_cancel(forge, OF_STR + i, OF_DEC_STR + i);

    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
        _flag_cancel(forge, OF_RES_(i), OF_VULN_(i));

    _flag_cancel(forge, OF_LIFE, OF_DEC_LIFE);
    _flag_cancel(forge, OF_MAGIC_MASTERY, OF_DEC_MAGIC_MASTERY);
    _flag_cancel(forge, OF_SPEED, OF_DEC_SPEED);
    _flag_cancel(forge, OF_SPELL_CAP, OF_DEC_SPELL_CAP);
    _flag_cancel(forge, OF_SPELL_POWER, OF_DEC_SPELL_POWER);
    _flag_cancel(forge, OF_STEALTH, OF_DEC_STEALTH);

    /* remove flags obscured by stronger flags */
    _flag_superfluous(forge, OF_KILL_EVIL, OF_SLAY_EVIL);
    _flag_superfluous(forge, OF_KILL_DRAGON, OF_SLAY_DRAGON);
    _flag_superfluous(forge, OF_KILL_DEMON, OF_SLAY_DEMON);
    _flag_superfluous(forge, OF_KILL_UNDEAD, OF_SLAY_UNDEAD);
    _flag_superfluous(forge, OF_KILL_ANIMAL, OF_SLAY_ANIMAL);
    _flag_superfluous(forge, OF_KILL_HUMAN, OF_SLAY_HUMAN);
    _flag_superfluous(forge, OF_KILL_ORC, OF_SLAY_ORC);
    _flag_superfluous(forge, OF_KILL_TROLL, OF_SLAY_TROLL);
    _flag_superfluous(forge, OF_KILL_GIANT, OF_SLAY_GIANT);
    _flag_superfluous(forge, OF_VORPAL2, OF_VORPAL);

    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
        _flag_superfluous(forge, OF_IM_(i), OF_RES_(i));

    /* double check if pval is still required */
    if ( forge->obj->pval
      && !of_has_pval(forge->obj->flags)
      && !of_has_pval(k_info[forge->obj->k_idx].flags) )
    {
        forge->obj->pval = 0;
    }

    remove_flag(forge->obj->flags, OF_FULL_NAME); /* e.g. ?Art on a Hell Harness (cf object_desc) */
}

static void _finalize(_forge_ptr forge)
{
    _pval(forge);
    _activation(forge);
    _hack(forge);
    _ignore(forge);

    _name(forge);
    _cleanup(forge); /* after AM_CURSED is applied in _name() */
}

static void _initialize(_forge_ptr forge)
{
    forge->obj->art_id = 0;
    forge->obj->replacement_art_id = 0;
    forge->obj->name2 = 0;

    if (obj_is_ammo(forge->obj))
    {
        forge->obj->number = 1;
        forge->powers = (forge->powers + 1)/2;
    }

    /* hack for torches of demeter (pval = 4000) */
    if (forge->obj->tval == TV_LIGHT)
        forge->obj->pval = 0;

    /* XXX We should already be 'plussed' as an ego. Boost slightly at no charge */
    if (obj_is_body_armor(forge->obj))
        forge->obj->to_a += m_bonus(7, forge->lvl);
    else if (obj_is_armor(forge->obj))
        forge->obj->to_a += m_bonus(5, forge->lvl);
    else if (obj_is_weapon_ammo(forge->obj))
    {
        forge->obj->to_h += m_bonus(5, forge->lvl);
        forge->obj->to_d += m_bonus(5, forge->lvl);
    }
}

/******************************************************************************
 * Public
 ******************************************************************************/
void art_create_random(obj_ptr obj, int level, int mode)
{
    _forge_t forge = _forge(obj, level, mode);
    _forge_f f = _choose_forge_f(&forge);
    int      ct = 0;

    forge.powers = _roll_powers(obj, level);

    _initialize(&forge);
    while (forge.powers > 0 && ct < 1000)
    {
        f(&forge);
        ct++;
    }
    _finalize(&forge);
}

void art_create_ego(obj_ptr obj, int level, int mode)
{
    _forge_t forge = _forge(obj, level, mode);
    _forge_f f = _choose_forge_f(&forge);
    int      ct = 0, i;

    forge.powers = _roll_ego_powers(obj, level);
    forge.ego = &e_info[obj->name2];

    /* convert the ego into an artifact */
    for (i = 0; i < OF_ARRAY_SIZE; i++)
        obj->flags[i] |= forge.ego->flags[i];

    if (!obj->activation.type)
        obj->activation = forge.ego->activation;

    if (obj->tval == TV_BOW)
    {
        /* Hack: Undo damage scaling from ego since we will redo it again later! */
        obj->to_d = obj->to_d * 7150 / bow_energy(obj->sval);
    }

    _initialize(&forge);
    while (forge.powers > 0 && ct < 1000)
    {
        f(&forge);
        ct++;
    }
    _finalize(&forge);
}

static bool _forge_basic(_forge_ptr forge, art_ptr art)
{
    int k_idx;

    forge->art = art;

    k_idx = lookup_kind(forge->art->tval, forge->art->sval);
    if (!k_idx) return FALSE;

    object_prep(forge->obj, k_idx);

    forge->obj->art_id = art->id;
    forge->obj->pval = forge->art->pval;
    forge->obj->ac = forge->art->ac;
    forge->obj->dd = forge->art->dd;
    forge->obj->ds = forge->art->ds;
    forge->obj->mult = forge->art->mult;
    forge->obj->to_a = forge->art->to_a;
    forge->obj->to_h = forge->art->to_h;
    forge->obj->to_d = forge->art->to_d;
    forge->obj->weight = forge->art->weight;

    if (obj_is_(forge->obj, TV_HAFTED, SV_WIZSTAFF))
    {
        device_init(forge->obj, forge->art->level, AM_GOOD | AM_GREAT);
        if (forge->art->activation.type)
            forge->obj->activation = forge->art->activation;
    }

    return TRUE;
}
static void _forge_extra(_forge_ptr forge)
{
    int resistance_ct = 0, power_ct = 0, i;

    /* Object Generation Flags */
    if (forge->art->gen_flags & OFG_CURSED) forge->obj->curse_flags |= OFC_CURSED;
    if (forge->art->gen_flags & OFG_HEAVY_CURSE) forge->obj->curse_flags |= OFC_HEAVY_CURSE;
    if (forge->art->gen_flags & OFG_PERMA_CURSE) forge->obj->curse_flags |= OFC_PERMA_CURSE;
    if (forge->art->gen_flags & (OFG_RANDOM_CURSE0)) forge->obj->curse_flags |= get_curse(0, forge->obj);
    if (forge->art->gen_flags & (OFG_RANDOM_CURSE1)) forge->obj->curse_flags |= get_curse(1, forge->obj);
    if (forge->art->gen_flags & (OFG_RANDOM_CURSE2)) forge->obj->curse_flags |= get_curse(2, forge->obj);
    if (forge->art->gen_flags & OFG_XTRA_POWER) power_ct++;
    if (forge->art->gen_flags & OFG_XTRA_H_RES) resistance_ct++;
    if (forge->art->gen_flags & OFG_XTRA_RES_OR_POWER)
    {
        if (one_in_(2)) resistance_ct++;
        else power_ct++;
    }

    /* Special Cases for Class Specific Artifacts */
    if (obj_is_specified_art(forge->obj, "/.Bloody Moon"))
        get_bloody_moon_flags(forge->obj);
    else if (obj_is_specified_art(forge->obj, "].Destroyer"))
    {
        if (!prace_is_(RACE_MON_GOLEM))
        {
            add_flag(forge->obj->flags, OF_DEC_BLOWS);
            add_flag(forge->obj->flags, OF_AGGRAVATE);
            add_flag(forge->obj->flags, OF_DRAIN_EXP);
            add_flag(forge->obj->flags, OF_TY_CURSE);
            forge->obj->curse_flags |=
                (OFC_CURSED | OFC_HEAVY_CURSE | OFC_PERMA_CURSE);
        }
    }
    else if (obj_is_specified_art(forge->obj, "/.Dragonlance"))
    {
        if (warlock_is_(WARLOCK_DRAGONS))
        {
            add_flag(forge->obj->flags, OF_SLAY_EVIL);
            add_flag(forge->obj->flags, OF_SLAY_DEMON);
            add_flag(forge->obj->flags, OF_SLAY_UNDEAD);
        }
    }
    else if (obj_is_specified_art(forge->obj, "\\.Gothmog"))
    {
        if (prace_is_(RACE_MON_DEMON))
        {
            forge->obj->dd = 6;
            forge->obj->ds = 6;
            forge->obj->to_h = 22;
            forge->obj->to_d = 25;
        }
        else
            forge->obj->curse_flags |= (OFC_CURSED | OFC_HEAVY_CURSE);
    }
    else if (obj_is_specified_art(forge->obj, "(.Maiden"))
    {
        if (plr->psex != SEX_FEMALE)
            add_flag(forge->obj->flags, OF_AGGRAVATE);
    }
    else if (obj_is_specified_art(forge->obj, "\\.Jones"))
    {
        if (plr->pclass == CLASS_ARCHAEOLOGIST)
        {
            power_ct++;
            resistance_ct++;
            forge->obj->pval += 2;
            add_flag(forge->obj->flags, OF_SEARCH);
            add_flag(forge->obj->flags, OF_FREE_ACT);
        }
    }
    else if (obj_is_specified_art(forge->obj, "|.Muramasa"))
    {
        if (plr->pclass != CLASS_SAMURAI && plr->pclass != CLASS_BLOOD_KNIGHT)
        {
            add_flag(forge->obj->flags, OF_NO_MAGIC);
            forge->obj->curse_flags |= (OFC_HEAVY_CURSE);
        }
    }
    else if (obj_is_specified_art(forge->obj, "].Stone Mask"))
    {
        if (plr->prace == RACE_MON_VAMPIRE)
        {
            add_flag(forge->obj->flags, OF_CHR);
            forge->obj->to_a = 20;
        }
        else
            add_flag(forge->obj->flags, OF_VULN_(GF_LIGHT));
    }
    else if (obj_is_specified_art(forge->obj, "|.Stormbringer"))
    {
        if (prace_is_(RACE_MON_SWORD))
            add_flag(forge->obj->flags, OF_BLOWS); /* Just like the good 'ol days :) */
        else
        {
            add_flag(forge->obj->flags, OF_AGGRAVATE);
            add_flag(forge->obj->flags, OF_DRAIN_EXP);
            forge->obj->curse_flags |= (OFC_CURSED | OFC_HEAVY_CURSE);
        }
    }
    else if (obj_is_specified_art(forge->obj, "].Terror"))
    {
        if ( plr->pclass == CLASS_WARRIOR
          || plr->pclass == CLASS_CAVALRY
          || plr->pclass == CLASS_MAULER
          || plr->prace == RACE_MON_HYDRA
          || plr->prace == RACE_MON_TROLL
          || plr->prace == RACE_MON_JELLY )
        {
            power_ct++;
            resistance_ct++;
        }
        else
        {
            add_flag(forge->obj->flags, OF_AGGRAVATE);
            add_flag(forge->obj->flags, OF_TY_CURSE);
            forge->obj->curse_flags |= (OFC_CURSED | OFC_HEAVY_CURSE);
            forge->obj->curse_flags |= get_curse(2, forge->obj);
            power_ct = 0;
            resistance_ct = 0;
        }
    }
    else if (obj_is_specified_art(forge->obj, "|.Twilight"))
    {
        if (giant_is_(GIANT_FIRE)) /* Boss reward for Fire Giants */
        {
            forge->obj->to_h = 10;
            forge->obj->to_d = 10;
            forge->obj->to_a = 0;
            add_flag(forge->obj->flags, OF_SLAY_EVIL);
            resistance_ct++;
        }
        else if (warlock_is_(WARLOCK_GIANTS)) /* Pseudo-boss reward for Giant Warlocks */
        {
            forge->obj->to_h = 5;
            forge->obj->to_d = 5;
            forge->obj->to_a = 0;
        }
        else
        {
            add_flag(forge->obj->flags, OF_AGGRAVATE);
            add_flag(forge->obj->flags, OF_TY_CURSE);
            forge->obj->curse_flags |=
                (OFC_CURSED | OFC_HEAVY_CURSE);
            forge->obj->curse_flags |= get_curse(2, forge->obj);
        }
    }
    else if (obj_is_specified_art(forge->obj, "\\.Xiaolong"))
    {
        if (plr->pclass == CLASS_MONK)
            add_flag(forge->obj->flags, OF_BLOWS);
    }

    for (i = 0; i < power_ct; i++)
        _one_ability(forge);

    for (i = 0; i < resistance_ct; i++)
        _one_high_resist(forge);
}

bool art_create_std(obj_ptr obj, art_ptr art, int mode)
{
    _forge_t forge = _forge(obj, cave->difficulty, mode);

    if (!art) return FALSE;
    if (!_forge_basic(&forge, art))
        return FALSE;

    if (!(mode & AM_DEBUG))
        _forge_extra(&forge);

    if (!(mode & (AM_DEBUG | AM_NO_DROP)))
        art->generated = TRUE;

    return TRUE;
}

/* These are for wizard stat reporting to make sure replacement art runs
 * are getting reasonably matched replacements */
int original_score = 0;
int replacement_score = 0;

bool art_create_replacement(obj_ptr obj, art_ptr art, int mode)
{
    object_type    forge1 = {0};
    object_type    forge2 = {0};
    object_type    best = {0}, worst = {0};
    int            base_power, best_power, power = 0, worst_power = 10000000;
    int            min_power, max_power;
    int            i;

    if (!art) return FALSE;
    if (no_artifacts) return FALSE;

    /* Score the Original */
    if (!art_create_std(&forge1, art, AM_NO_DROP)) return FALSE;
    if (obj_is_weapon_ammo(&forge1))
    {
        forge1.to_h = MAX(10, forge1.to_h);
        forge1.to_d = MAX(10, forge1.to_d);
    }
    if (obj_is_armor(&forge1))
    {
        forge1.to_a = MAX(10, forge1.to_a);
    }
    base_power = obj_value_real(&forge1);
    if (!obj_is_ammo(&forge1) && base_power < 7500)
        base_power = 7500;
    min_power = MAX(base_power *  9 / 10, base_power - 10000);
    max_power = MIN(base_power * 12 / 10, base_power + 10000);

    original_score += base_power;
    best_power = -10000000;
    power = 0;

    for (i = 0; i < 10000; i++)
    {
        object_prep(&forge2, forge1.k_idx);
        /* since we are bypassing apply_magic, we need to give the object
         * some random starting plusses */
        if (obj_is_weapon_ammo(&forge2))
            obj_create_weapon_aux(&forge2, art->level, 3);
        else if (obj_is_armor(&forge2))
            obj_create_armor_aux(&forge2, art->level, 3);
        art_create_random(&forge2, art->level, 0);
        if (art->gen_flags & OFG_FIXED_ACT)
        {
            forge2.activation = art->activation;
            add_flag(forge2.flags, OF_ACTIVATE); /* for object lore */
        }
        power = obj_value_real(&forge2);

        if (power > best_power)
        {
            object_copy(&best, &forge2);
            best_power = power;
        }
        if (power < worst_power)
        {
            object_copy(&worst, &forge2);
            worst_power = power;
        }

        if (min_power <= power && power <= max_power)
        {
            /* Success! First replacement to match the power range wins. */
            replacement_score += power;
            object_copy(obj, &forge2);
            obj->replacement_art_id = art->id;
            obj->weight = forge1.weight;
            obj->level = art->level;
            art_remember_name(quark_str(obj->art_name));
            return TRUE;
        }
    }

    /* Failed! Return best or worst */
    if (worst_power > base_power)
    {
        replacement_score += worst_power;
        object_copy(obj, &worst);
    }
    else
    {
        replacement_score += best_power;
        object_copy(obj, &best);
    }
    obj->replacement_art_id = art->id;
    obj->weight = forge1.weight;
    obj->level = art->level;
    art_remember_name(quark_str(obj->art_name));

    if (!(mode & (AM_DEBUG | AM_NO_DROP)))
        art->generated = TRUE;

    return TRUE;
}

bool art_reforge(obj_ptr src, obj_ptr dest, int fame)
{
    bool        result = FALSE;
    object_type forge = {0};
    object_type best = {0}, worst = {0};
    int         base_power, best_power = -10000000, power = 0, worst_power = 10000000;
    int         min_power, max_power, i, level;
    int         src_weight = _get_slot_pct(src);
    int         dest_weight = _get_slot_pct(dest);

    /* Score the Original */
    base_power = obj_value_real(src);

    /* Penalize reforging a powerful slot into a weak slot (e.g. weapons into lights)
     * Also consider moving power from something strong into a weak weapon, such as
     * a falcon sword or a dagger (e.g. ninja's do this) */
    if (obj_is_weapon(dest))
    {
        int dice = k_info[dest->k_idx].dd * k_info[dest->k_idx].ds;
        if (dice < 12)
            dest_weight = dest_weight * dice / 12;
    }
    if (src_weight > dest_weight)
        base_power = base_power * dest_weight / src_weight;

    /* Pay a Power Tax! */
    base_power = base_power*2/3 + randint1(base_power*fame/900);

    /* Setup thresholds. For weak objects, its better to use a generous range ... */
    if (base_power < 1000)
    {
        min_power = 0;
        max_power = base_power + 5000;
    }
    else
    {
        min_power = 7 * base_power / 10;
        max_power = 3 * base_power / 2;
    }

    /* Better Fame means better results! */
    level = MIN(fame/2, 75);

    for (i = 0; i < 1000 && !result; i++)
    {
        object_copy(&forge, dest);
        art_create_random(&forge, level, 0);

        power = obj_value_real(&forge);

        if (power > best_power)
        {
            object_copy(&best, &forge);
            best_power = power;
        }
        if (power < worst_power && min_power < power)
        {
            object_copy(&worst, &forge);
            worst_power = power;
        }

        if (power >= min_power && power <= max_power)
        {
            object_copy(dest, &forge);
            result = TRUE;
        }
    }

    if (!result)
    {
        /* Failed! Return best or worst */
        if (worst_power > min_power && worst.k_idx)
            object_copy(dest, &worst);
        else
            object_copy(dest, &best);

        result = TRUE;
    }

    /* Flavor: Keep name of source artifact if possible */
    if (src->art_id)
    {
        art_ptr art = arts_lookup(src->art_id);
        dest->art_name = quark_add(art->name);
    }
    else
        dest->art_name = src->art_name;

    return result;
}

/******************************************************************************
 * a_info.txt
 ******************************************************************************/
static int_map_ptr _artifacts = NULL;

art_ptr art_alloc(sym_t id)
{
    art_ptr a = malloc(sizeof(art_t));
    memset(a, 0, sizeof(art_t));
    a->id = id;
    add_flag(a->flags, OF_IGNORE_ACID);
    add_flag(a->flags, OF_IGNORE_ELEC);
    add_flag(a->flags, OF_IGNORE_FIRE);
    add_flag(a->flags, OF_IGNORE_COLD);
    return a;
}
void art_free(art_ptr a)
{
    if (!a) return;
    if (a->name) z_string_free(a->name);
    if (a->text) z_string_free(a->text);
    if (a->activation_msg) z_string_free(a->activation_msg);
    free(a);
}
art_ptr arts_lookup(sym_t id)
{
    assert(_artifacts);
    assert(id);
    return int_map_find(_artifacts, id);
}
art_ptr arts_parse(cptr token)
{
    sym_t id = sym_find(token);
    if (!id) return NULL;
    return arts_lookup(id);
}
static obj_ptr _filter_obj;
static bool _filter(int id, art_ptr art)
{
    if (art->generated) return FALSE;
    if (art->gen_flags & OFG_QUESTITEM) return FALSE;
    if (art->gen_flags & OFG_INSTA_ART) return FALSE;
    if (art->tval != _filter_obj->tval) return FALSE;
    if (art->sval != _filter_obj->sval) return FALSE;
    return TRUE;
}
static int _cmp_lvl(art_ptr left, art_ptr right)
{
    if (left->level < right->level) return -1;
    if (left->level > right->level) return 1;
    return 0;
}
vec_ptr arts_filter(obj_ptr obj)
{
    vec_ptr v;
    _filter_obj = obj;
    v = int_map_filter(_artifacts, (int_map_filter_f)_filter);
    vec_sort(v, (vec_cmp_f)_cmp_lvl); /* XXX try to make_artifact() in order of increasing power */
    return v;
}
static bool _filter_special(int id, art_ptr art)
{
    if (art->generated) return FALSE;
    if (art->gen_flags & OFG_QUESTITEM) return FALSE;
    if (art->gen_flags & OFG_INSTA_ART) return TRUE;
    return FALSE;
}
vec_ptr arts_filter_special(void)
{
    vec_ptr v = int_map_filter(_artifacts, (int_map_filter_f)_filter_special);
    vec_sort(v, (vec_cmp_f)_cmp_lvl); /* XXX try to make_artifact_special() in order of increasing power */
    return v;
}
vec_ptr arts_filter_ex(bool (*f)(int id, art_ptr art))
{
    return int_map_filter(_artifacts, (int_map_filter_f)f);
}
static errr _parse_art(char *line, int options)
{
    static art_ptr current = NULL;

    /* N:~.Galadriel:of Galadriel */
    if (line[0] == 'N')
    {
        char *zz[10];
        int   num = tokenize(line + 2, 10, zz, TOKENIZE_NO_SLASH | TOKENIZE_NO_ESCAPE);
        sym_t id;

        if (num != 2) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

        id = sym_add(zz[0]);
        if (int_map_find(_artifacts, id)) return PARSE_ERROR_NON_SEQUENTIAL_RECORDS;
        current = art_alloc(id);
        current->name = z_string_make(zz[1]);
        int_map_add(_artifacts, id, current);
    }

    else if (!current) return PARSE_ERROR_MISSING_RECORD_HEADER; /* missing initial N: line */

    else if (line[0] == 'E')
    {
        /* First E: line is required and defines the activation. */
        if (!current->activation.type)
        {
            errr rc = effect_parse(line + 2, &current->activation);
            if (rc) return rc;
            add_flag(current->flags, OF_ACTIVATE); /* for object lore */
        }
        /* Second E: line is optional and describes the activation. */
        else if (!current->activation_msg)
        {
            current->activation_msg = z_string_make(line + 2);
        }
        else
            return PARSE_ERROR_GENERIC;
    }

    /* D:<line> ... multiple lines are concatenated into a single description */
    else if (line[0] == 'D')
    {
        current->text = z_string_append(current->text, line + 2, ' ');
    }

    /* I:<tval>:<sval>[:<pval>] */
    else if (line[0] == 'I')
    {
        char *zz[3];
        int   num = tokenize(line + 2, 3, zz, TOKENIZE_CHECKQUOTE | TOKENIZE_NO_SLASH | TOKENIZE_NO_ESCAPE);

        if (num < 2) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
        if (is_numeric(zz[0]))
            current->tval = atoi(zz[0]);
        else
        {
            tv_info_ptr info = tv_parse_name(zz[0]);
            if (!info)
            {
                msg_format("Unknown tval=%s", zz[0]);
                return PARSE_ERROR_GENERIC;
            }
            current->tval = info->id;
        }
        current->sval = atoi(zz[1]);
        if (num >= 3)
            current->pval = atoi(zz[2]);
    }

    else if (line[0] == 'W')
    {
        int level, rarity, wgt;
        int cost;

        if (4 != sscanf(line+2, "%d:%d:%d:%d",
                &level, &rarity, &wgt, &cost)) return (1);

        current->level = level;
        current->rarity = rarity;
        current->weight = wgt;
        current->cost = cost;
    }

    else if (line[0] == 'P')
    {
        int ac, hd1, hd2, th, td, ta, mult = 0;

        if (current->tval == TV_BOW)
        {
            if (6 != sscanf(line+2, "%d:x%d.%d:%d:%d:%d",
                    &ac, &hd1, &hd2, &th, &td, &ta)) return (1);
            mult = hd1 * 100 + hd2; /* x3.25 -> 325 (alas, x3.2 -> 302 so use x3.20 instead) */
            hd1 = 0;
            hd2 = 0;
        }
        else
        {
            if (6 != sscanf(line+2, "%d:%dd%d:%d:%d:%d",
                    &ac, &hd1, &hd2, &th, &td, &ta)) return (1);
        }
        current->ac = ac;
        current->dd = hd1;
        current->ds = hd2;
        current->mult = mult;
        current->to_h = th;
        current->to_d = td;
        current->to_a =  ta;
    }

    else if (line[0] == 'F')
    {
        char *zz[20];
        int   num = z_string_split(line + 2, zz, 20, "|");
        int   i;

        for (i = 0; i < num; i++)
        {
            cptr token = zz[i];
            errr ooops = grab_one_artifact_flag(current, token);
            if (ooops)
            {
                msg_format("Unknown artifact flag: %s.", token);
                return ooops;
            }
        }
    }

    else return PARSE_ERROR_UNDEFINED_DIRECTIVE;

    return ERROR_SUCCESS;
}

bool arts_init(void)
{
    assert(!_artifacts);
    _artifacts = int_map_alloc((int_map_free_f)art_free);
    return !parse_edit_file("a_info.txt", _parse_art, 0); /* errr -> bool */
}

static void _art_reset(int id, art_ptr art)
{
    art->generated = FALSE;
    art->found = FALSE;
}
void arts_reset(void)
{
    assert(_artifacts);
    int_map_iter(_artifacts, (int_map_iter_f)_art_reset);
    art_names_reset();
}

/******************************************************************************
 * Savefiles
 ******************************************************************************/
static bool _art_has_lore(art_ptr art)
{
    int i;
    for (i = 0; i < OF_ARRAY_SIZE; i++)
    {
        if (art->known_flags[i]) return TRUE;
    }
    return FALSE;
}
static savefile_ptr _file;
static void _art_save(int id, art_ptr art)
{
    int i;
    if (_art_has_lore(art) || art->generated || art->found)
    {
        savefile_write_sym(_file, art->id);
        if (_art_has_lore(art))
        {
            for (i = 0; i < OF_ARRAY_SIZE; i++)
            {
                if (!art->known_flags[i]) continue;
                savefile_write_byte(_file, i);
                savefile_write_u32b(_file, art->known_flags[i]);
            }
        }
        savefile_write_byte(_file, 0xFF);
        savefile_write_byte(_file, art->generated);
        savefile_write_byte(_file, art->found);
    }
}
void arts_save(savefile_ptr file)
{
    _file = file;
    int_map_iter(_artifacts, (int_map_iter_f)_art_save);
    savefile_write_sym(_file, 0);
    art_names_save(file);
}
void arts_load(savefile_ptr file)
{
    for (;;)
    {
        sym_t id = savefile_read_sym(file);
        art_ptr art;
        if (!id) break;
        art = arts_lookup(id);
        if (!art)
            quit(format("Art (%d) out of range!", id));
        for (;;)
        {
            byte b = savefile_read_byte(file);
            if (b == 0xFF) break;
            assert(/*0 <= b &&*/ b < OF_ARRAY_SIZE);
            art->known_flags[b] = savefile_read_u32b(file);
        }
        art->generated = savefile_read_byte(file);
        art->found = savefile_read_byte(file);
    }
    art_names_load(file);
}
