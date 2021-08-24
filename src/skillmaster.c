#include "angband.h"

#include <assert.h>

/* UI helpers for gaining skills and casting spells */
static doc_ptr _doc = NULL;

static int _inkey(void)
{
    return inkey_special(TRUE);
}

static void _sync_term(doc_ptr doc)
{
    rect_t r = ui_map_rect();
    Term_load();
    doc_sync_term(doc, doc_range_top_lines(doc, r.cy), doc_pos_create(r.x, r.y));
}

/************************************************************************
 * The Skill System
 ***********************************************************************/
#define _MAX_SKILLS 15

enum _type_e {
    _TYPE_NONE = 0,
    _TYPE_MELEE,
    _TYPE_SHOOT,
    _TYPE_MAGIC,
    _TYPE_PRAYER,
    _TYPE_SKILLS,
    _TYPE_TECHNIQUE,
    _TYPE_ABILITY,
};

/* It is convenient to use TV_* and REALM_* for various skills,
 * so other values must be chosen so as not to cause collisions */
enum {
    _SKILL_START = 300,
    _MARTIAL_ARTS = 300,
    _ARCHERY,
    _THROWING,
    _DUAL_WIELDING,
    _RIDING,
    _DEVICE_SKILL,
    _STEALTH,
    _SPEED,
    _AWARENESS,
    _HEALTH,
    _AGILITY,
    _MAGIC_RESISTANCE,
    _SPELL_CAPACITY,
    _SPELL_POWER,
    _DEVICE_POWER,
    _SKILL_STOP
};

/* Abilities are one time purchases granting a power or some other
 * attribute, like regeneration */
enum {
    _ABILITY_START = 400,
    _CLEAR_MIND = 400,
    _EAT_MAGIC,
    _LOREMASTER,
    _LUCK,
    _MASSACRE,
    _PANIC_HIT,
    _REGENERATION,
    _RESISTANCE,
    _STONE_SKIN,
    _CREATE_AMMO,
    _RODEO,
    _RUSH_ATTACK,
    _NOCTOVISION,
    _ABILITY_STOP
};

typedef struct _skill_s {
    int  type;
    int  subtype;
    cptr name;
    int  max;
    int  current;
} _skill_t, *_skill_ptr;

typedef struct _group_s {
    int  type;
    cptr name;
    cptr desc;
    _skill_t skills[_MAX_SKILLS];
} _group_t, *_group_ptr;

static _group_t _groups[] = {
    { _TYPE_MELEE, "Melee",
        "<color:B>Melee</color> skills increase your ability to fight monsters in hand to hand combat. "
        "The total number of points spent in this group will determine your base fighting "
        "skill, as well as how much melee skill you gain with experience. In addition, this "
        "total gives bonuses to STR and DEX. The individual skills determine your base "
        "proficiency with the given type of weapon, as well as the maximum number of blows "
        "per round you may gain. For martial arts, the number of points determines your "
        "effective fighting level which is used to determine which types of hits you land.",
        {{ _TYPE_MELEE, TV_SWORD, "Swords", 5, 0 },
         { _TYPE_MELEE, TV_POLEARM, "Polearms", 5, 0 },
         { _TYPE_MELEE, TV_HAFTED, "Hafted", 5, 0 },
         { _TYPE_MELEE, TV_DIGGING, "Diggers", 5, 0 },
         { _TYPE_MELEE, _MARTIAL_ARTS, "Martial Arts", 5, 0 },
         { 0 }}},
    { _TYPE_SHOOT, "Ranged",
        "<color:B>Ranged</color> skills increase your ability to fight monsters using distance weapons "
        "such as bows and slings, as well as the ability to throw melee weapons "
        "effectively. The total number of points spent in this group will determine "
        "your base shooting skill, as well as how fast this skill increases with "
        "experience. Archery grants proficiency with the various classes of shooters "
        "as well as increasing your number of shots per round. Throwing grants the power "
        "to throw your leading melee weapon and perhaps have it return to you as well.",
        {{ _TYPE_SHOOT, _ARCHERY, "Archery", 5, 0 },
         { _TYPE_SHOOT, _THROWING, "Throwing", 5, 0 },
         { 0 }}},
    { _TYPE_MAGIC, "Magic",
        "<color:B>Magic</color> skills grant access to various spellcasting realms using INT rather "
        "than WIS as the spellcasting stat. Each point in a given realm increases "
        "your maximum spellcasting level as well as the decreasing the cost and fail "
        "rates of individual spells. You need not learn individual spells the way a "
        "mage might. Rather, you may cast any spell in a known realm provided your "
        "spellcasting level is high enough. The total number of points spent in this "
        "group provides a modest boost to device skills as well as increasing your "
        "INT. Unfortunately. this total also has a negative impact on your life rating, "
        "STR and CON.",
        {{ _TYPE_MAGIC, REALM_ARCANE, "Arcane", 5, 0 },
         { _TYPE_MAGIC, REALM_ARMAGEDDON, "Armageddon", 5, 0 },
         { _TYPE_MAGIC, REALM_CHAOS, "Chaos", 5, 0 },
         { _TYPE_MAGIC, REALM_CRAFT, "Craft", 5, 0 },
         { _TYPE_MAGIC, REALM_CRUSADE, "Crusade", 5, 0 },
         { _TYPE_MAGIC, REALM_DAEMON, "Daemon", 5, 0 },
         { _TYPE_MAGIC, REALM_DEATH, "Death", 5, 0 },
         { _TYPE_MAGIC, REALM_NATURE, "Nature", 5, 0 },
         { _TYPE_MAGIC, REALM_SORCERY, "Sorcery", 5, 0 },
         { _TYPE_MAGIC, REALM_TRUMP, "Trump", 5, 0 },
         { _TYPE_MAGIC, REALM_ILLUSION, "Illusion", 5, 0 },
         { 0 }}},
    { _TYPE_PRAYER, "Prayer",
        "<color:B>Prayer</color> skills work like Magic skills, granting access to various realms of "
        "magic. However, prayer realms use WIS for the spellcasting stat rather than "
        "INT. Moreover, the total points in the Prayer group grants enhanced saving "
        "throws as well as increased WIS, but does not affect device skills or life "
        "rating. As in Magic, the number of points invested in a particular realm "
        "determines your maximum casting level and also influences casting costs and "
        "fail rates. Life magic is only available as a Prayer talent. The other prayer "
        "realms are duplicated as Magic talents, but you can only learn them in one "
        "way: as either INT based or WIS based.",
        {{ _TYPE_PRAYER, REALM_BLESS, "Benediction", 5, 0 },
         { _TYPE_PRAYER, REALM_CRUSADE, "Crusade", 5, 0 },
         { _TYPE_PRAYER, REALM_DAEMON, "Daemon", 5, 0 },
         { _TYPE_PRAYER, REALM_DEATH, "Death", 5, 0 },
         { _TYPE_PRAYER, REALM_HEX, "Malediction", 5, 0 },
         { _TYPE_PRAYER, REALM_LIFE, "Life", 5, 0 },
         { _TYPE_PRAYER, REALM_NATURE, "Nature", 5, 0 },
         { 0 }}},
    { _TYPE_SKILLS, "Skills",
        "There are various <color:B>Skills</color> important for every player, including "
        "stealth, perception, health and magic devices. While these skills may be influenced by "
        "talents in other groups (e.g. Magic increases your device skills), it is often "
        "desirable to directly increase these skills even more. In addition, you can directly "
        "improve your starting stats by investing in some of the skills in this group "
        "(e.g. Agility improves DEX while Health improves CON).",
        {{ _TYPE_SKILLS, _AGILITY, "Agility", 3, 0 },
         { _TYPE_SKILLS, _AWARENESS, "Awareness", 3, 0 },
         { _TYPE_SKILLS, _DEVICE_SKILL, "Device Skill", 3, 0 },
         { _TYPE_SKILLS, _DEVICE_POWER, "Device Power", 3, 0 },
         { _TYPE_SKILLS, _HEALTH, "Health", 3, 0 },
         { _TYPE_SKILLS, _MAGIC_RESISTANCE, "Magic Resistance", 3, 0 },
         { _TYPE_SKILLS, _SPEED, "Speed", 3, 0 },
         { _TYPE_SKILLS, _SPELL_CAPACITY, "Spell Capacity", 3, 0 },
         { _TYPE_SKILLS, _SPELL_POWER, "Spell Power", 3, 0 },
         { _TYPE_SKILLS, _STEALTH, "Stealth", 3, 0 },
         { 0 }}},
    { _TYPE_TECHNIQUE, "Techniques",
        "There are various specialty <color:B>Techniques</color> which you may want to "
        "learn, including the Riding and Dual Wielding talents as well as access to "
        "Burglary spells. Note that Burglary uses DEX as the primary spell stat.",
        {{ _TYPE_TECHNIQUE, REALM_BURGLARY, "Burglary", 5, 0 },
         { _TYPE_TECHNIQUE, _DUAL_WIELDING, "Dual Wielding", 5, 0 },
         { _TYPE_TECHNIQUE, REALM_MUSIC, "Music", 5, 0 },
         { _TYPE_TECHNIQUE, _RIDING, "Riding", 5, 0 },
         { 0 }}},
    { _TYPE_ABILITY, "Abilities",
        "Many <color:B>Abilities</color> can be purchased for a single skill point. Often "
        "these grant an extra power which you may activate at will without cost, and usually "
        "without fail. Alternatively, abilities may grant a bonus such as regeneration or "
        "good fortune.",
        {{ _TYPE_ABILITY, _CLEAR_MIND, "Clear Mind", 1, 0 },
         { _TYPE_ABILITY, _CREATE_AMMO, "Create Ammo", 1, 0 },
         { _TYPE_ABILITY, _EAT_MAGIC, "Eat Magic", 1, 0 },
         { _TYPE_ABILITY, _LUCK, "Good Luck", 1, 0 },
         { _TYPE_ABILITY, _LOREMASTER, "Loremastery", 1, 0 },
         { _TYPE_ABILITY, _MASSACRE, "Massacre", 1, 0 },
         { _TYPE_ABILITY, _NOCTOVISION, "Noctovision", 1, 0 },
         { _TYPE_ABILITY, _PANIC_HIT, "Panic Hit", 1, 0 },
         { _TYPE_ABILITY, _REGENERATION, "Regeneration", 1, 0 },
         { _TYPE_ABILITY, _RESISTANCE, "Resistance", 1, 0 },
         { _TYPE_ABILITY, _RODEO, "Rodeo", 1, 0 },
         { _TYPE_ABILITY, _RUSH_ATTACK, "Rush Attack", 1, 0 },
         { _TYPE_ABILITY, _STONE_SKIN, "Stone Skin", 1 , 0 },
         { 0 }}},
    { 0 }
};

static _group_ptr _get_group(int id)
{
    int i;
    for (i = 0; ; i++)
    {
        if (!_groups[i].type) return NULL;
        if (_groups[i].type == id) return &_groups[i];
    }
}

static int _get_group_ct(void)
{
    int i;
    int ct = 0;
    for (i = 0; ; i++)
    {
        if (!_groups[i].type) break;
        ct++;
    }
    return ct;
}

static _skill_ptr _get_skill_aux(_group_ptr g, int subtype)
{
    int i;
    for (i = 0; ; i++)
    {
        if (!g->skills[i].type) return NULL;
        if (g->skills[i].subtype == subtype) return &g->skills[i];
    }
}

static _skill_ptr _get_skill(int type, int subtype)
{
    _group_ptr g = _get_group(type);
    assert(g);
    return _get_skill_aux(g, subtype);
}

static int _get_group_pts_aux(_group_ptr g)
{
    int i, ct = 0;
    for (i = 0; i < _MAX_SKILLS; i++)
    {
        if (!g->skills[i].type) break;
        ct += g->skills[i].current;
    }
    return ct;
}

static int _get_group_pts(int id)
{
    _group_ptr g = _get_group(id);
    assert(g);
    return _get_group_pts_aux(g);
}

static int _get_pts(void)
{
    int i, ct = 0;
    for (i = 0; ; i++)
    {
        _group_ptr g = &_groups[i];
        if (!g->type) break;
        ct += _get_group_pts_aux(g);
    }
    return ct;
}

static int _get_max_pts(void)
{
    return 5 + plr->lev/5;
}

static int _get_free_pts(void)
{
    return MAX(0, _get_max_pts() - _get_pts());
}

int skillmaster_new_skills(void)
{
    return _get_free_pts();
}

static int _get_skill_pts(int type, int subtype)
{
    _skill_ptr s = _get_skill(type, subtype);
    assert(s);
    return s->current;
}

static int _get_skill_ct_aux(_group_ptr g)
{
    int i, ct = 0;
    for (i = 0; ; i++)
    {
        _skill_ptr s = &g->skills[i];
        if (!s->type) break;
        ct++;
    }
    return ct;
}

/*static int _get_skill_ct(int type)
{
    _group_ptr g = _get_group(type);
    assert(g);
    return _get_skill_ct_aux(g);
}*/

static void _reset_group(_group_ptr g)
{
    int i;
    for (i = 0; ; i++)
    {
        _skill_ptr s = &g->skills[i];
        if (!s->type) break;
        s->current = 0;
    }
}

static void _reset_groups(void)
{
    int i;
    for (i = 0; ; i++)
    {
        _group_ptr g = &_groups[i];
        if (!g->type) break;
        _reset_group(g);
    }
}

static void _load_group(savefile_ptr file,_group_ptr g)
{
    for (;;)
    {
        int subtype = savefile_read_u16b(file), current;
        _skill_ptr skill;
        if (subtype == 0xFFFE) break;
        /* XXX REALM_KENDO removed due to samurai.c spell re-write */
        skill = _get_skill_aux(g, subtype);
        current = savefile_read_u16b(file);
        if (skill) skill->current = current;
    }
}

static void _load_player(savefile_ptr file)
{
    _reset_groups();

    for (;;)
    {
        int type = savefile_read_u16b(file);
        if (type == 0xFFFF) break;
        _load_group(file, _get_group(type));
    }
}

static void _save_player(savefile_ptr file)
{
    int i, j;
    for (i = 0; ; i++)
    {
        _group_ptr g = &_groups[i];
        if (!g->type) break;
        savefile_write_u16b(file, g->type);
        for (j = 0; ; j++)
        {
            _skill_ptr s = &g->skills[j];
            if (!s->type) break;
            savefile_write_u16b(file, s->subtype);
            savefile_write_u16b(file, s->current);
        }
        savefile_write_u16b(file, 0xFFFE);
    }
    savefile_write_u16b(file, 0xFFFF);
}

/************************************************************************
 * Melee Skills
 ***********************************************************************/
static void _melee_init_class(class_t *class_ptr)
{
    typedef struct { int base_thn; int xtra_thn; int dev; int str; int dex; int int_; } _melee_skill_t;
    static _melee_skill_t _tbl[11] = {
       /*thn  thn dev  S  D   I */
        { 34,  30,  0, 0, 0,  0 },
        { 50,  50,  0, 1, 0,  0 },
        { 55,  60,  0, 1, 1,  0 },
        { 60,  75,  0, 2, 1,  0 },
        { 65,  90, -1, 2, 1,  0 },
        { 70, 105, -2, 2, 2,  0 },

        { 70, 115, -3, 3, 2, -1 },
        { 70, 125, -4, 3, 2, -1 },
        { 70, 135, -5, 3, 2, -1 },
        { 70, 145, -6, 4, 2, -2 },
        { 70, 150, -7, 4, 2, -2 }
    };
    int pts = MIN(10, _get_group_pts(_TYPE_MELEE));
    _melee_skill_t row = _tbl[pts];
    class_ptr->skills.thn += row.base_thn;
    class_ptr->extra_skills.thn += row.xtra_thn;
    class_ptr->skills.dev += row.dev;
    class_ptr->stats[A_STR] += row.str;
    class_ptr->stats[A_DEX] += row.dex;
    class_ptr->stats[A_INT] += row.int_;

    pts = _get_skill_pts(_TYPE_MELEE, _MARTIAL_ARTS);
    class_ptr->stats[A_DEX] += pts/4;
}

typedef struct { int to_h; int to_d; int prof; int blows_max; int blows_mul; int ma_wgt; } _melee_info_t;
static _melee_info_t _melee_info[6] = {
    {  0,  0, 2000, 400, 20,   0 },
    {  0,  0, 4000, 500, 30,  60 },
    {  0,  0, 5000, 525, 40,  70 },
    {  0,  0, 6000, 550, 50,  80 },
    {  0,  0, 7000, 575, 55,  90 },
    {  0,  0, 8000, 600, 60, 100 }
};

static void _calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    int           pts = _get_skill_pts(_TYPE_MELEE, obj->tval);
    int           magic_pts = _get_group_pts(_TYPE_MAGIC);
    _melee_info_t melee;

    assert(0 <= pts && pts <= 5);
    melee = _melee_info[pts];

    /* Blows Calculation */
    info->blows_calc.max = melee.blows_max - 5*magic_pts;
    info->blows_calc.wgt = 70;
    info->blows_calc.mul = melee.blows_mul;
    if (plr->riding)
    {
        if (obj_has_flag(obj, OF_RIDING))
        {
            pts = _get_skill_pts(_TYPE_TECHNIQUE, _RIDING);
            info->blows_calc.mul += 5 * pts;
        }
    }

    /* Combat Bonuses */
    info->to_h += melee.to_h;
    info->dis_to_h += melee.to_h;

    info->to_d += melee.to_d;
    info->dis_to_d += melee.to_d;

    if (_get_skill_pts(_TYPE_PRAYER, REALM_HEX))
        hex_calc_weapon_bonuses(obj, info);
    if (_get_skill_pts(_TYPE_PRAYER, REALM_BLESS))
        bless_calc_weapon_bonuses(obj, info);
}

int skillmaster_weapon_prof(int tval)
{
    int pts = _get_skill_pts(_TYPE_MELEE, tval);
    assert(0 <= pts && pts <= 5);
    return _melee_info[pts].prof;
}

bool skillmaster_weapon_is_icky(int tval)
{
    if (tval == TV_BOW) /* XXX "favorite" keyword in autopicker ends up here ... */
        return !_get_skill_pts(_TYPE_SHOOT, _ARCHERY);
    if (_get_skill_pts(_TYPE_MELEE, tval) || _get_skill_pts(_TYPE_SHOOT, _THROWING))
        return FALSE;
    return TRUE;
}

int skillmaster_martial_arts_prof(void)
{
    int pts = _get_skill_pts(_TYPE_MELEE, _MARTIAL_ARTS);
    assert(0 <= pts && pts <= 5);
    if (!pts)
        return 0; /* rather than 2000 which would allow the player to attempt bare-handed combat */
    return _melee_info[pts].prof;
}

static void _melee_calc_bonuses(void)
{
    int pts = _get_skill_pts(_TYPE_MELEE, _MARTIAL_ARTS);
    assert(0 <= pts && pts <= 5);
    plr->monk_lvl = (plr->lev * _melee_info[pts].ma_wgt + 50) / 100;
    if (!equip_find_first(obj_is_weapon) && plr->monk_lvl && !heavy_armor())
    {
        monk_ac_bonus();
        if (pts >= 5)
            plr->sh_retaliation = TRUE;
    }

    /* I'd prefer this in calc_weapon_bonuses, but we have a sequencing issue ...
     * It might be possible to move the dual_wielding block in calc_bonuses below 
     * the blows calculation, but those sorts of changes tend to have subtly 
     * unpredictable consequences ... */
    pts = _get_skill_pts(_TYPE_TECHNIQUE, _DUAL_WIELDING);
    if (pts >= 5)
    {
        add_flag(plr->attack_info[0].paf_flags, PAF_GENJI);
        add_flag(plr->attack_info[1].paf_flags, PAF_GENJI);
    }
}

void _melee_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    int pts = _get_skill_pts(_TYPE_MELEE, _MARTIAL_ARTS);
    if (pts >= 5)
        add_flag(flgs, OF_AURA_REVENGE);
}

/************************************************************************
 * Shoot Skills
 ***********************************************************************/
typedef struct { int base_thb; int xtra_thb; int prof; } _shoot_info_t;
static _shoot_info_t _shoot_info[6] = {
    { 20,  50, 2000 },
    { 40,  60, 4000 },
    { 50,  75, 5000 },
    { 60,  90, 6000 },
    { 65, 110, 7000 },
    { 70, 130, 8000 }
};
static void _shoot_init_class(class_t *class_ptr)
{
    /* Note: Bow skill (thb) now governs many aspects of archery,
     * including ammo breakage and shots per round. Throwing skill
     * (tht) needs to stop piggy-backing off archery skills, or the
     * player can just choose _THROWING to gain both benefits! */
    int pts = MIN(5, _get_skill_pts(_TYPE_SHOOT, _ARCHERY));
    _shoot_info_t row = _shoot_info[pts];
    class_ptr->skills.thb += row.base_thb;
    class_ptr->extra_skills.thb += row.xtra_thb;

    pts = _get_skill_pts(_TYPE_SHOOT, _THROWING);
    class_ptr->stats[A_DEX] += (pts + 1) / 2;

    pts = _get_skill_pts(_TYPE_SHOOT, _ARCHERY);
    class_ptr->skills.stl += (pts + 1) / 2;
}

int skillmaster_bow_prof(void)
{
    int pts = MIN(5, _get_skill_pts(_TYPE_SHOOT, _ARCHERY));
    return _shoot_info[pts].prof;
}

typedef struct { int skill; int back; int mult; int energy; } _throw_info_t;
static _throw_info_t _throw_info[6] = {
    {   0, 15, 100, 100 },
    {  12, 18, 100, 100 },
    {  28, 21, 150,  90 }, /* 18/220 Dex for 1% fail */
    {  48, 24, 200,  80 }, /* 18/180 Dex for 1% fail */
    {  72, 27, 300,  60 }, /* 18/150 Dex for 1% fail */
    { 100, 30, 400,  50 }, /* 18/110 Dex for 1% fail */
};

static void _shoot_calc_bonuses(void)
{
    int pts = MIN(5, _get_skill_pts(_TYPE_SHOOT, _THROWING));
    _shoot_info_t row = _shoot_info[pts];

    /* thb and tht should be separate ... _ARCHERY determines thb, while
     * _THROWING determines tht. Choosing _THROWING no longer benefits archery,
     * and vice versa */
    plr->skill_tht = row.base_thb + plr->lev * row.xtra_thb / 10;
    /* bonus skill, since throwing does not gain xtra to_h from ammo */
    plr->skill_tht += _throw_info[pts].skill;
}

static void _throw_weapon_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Throw Weapon");
        break;
    case SPELL_DESC:
        var_set_string(res, "Throws your weapon, which might return to you.");
        break;
    case SPELL_CAST: {
        plr_throw_t context = {0};
        int         pts = _get_skill_pts(_TYPE_SHOOT, _THROWING);

        context.type = THROW_BOOMERANG;
        context.mult = _throw_info[pts].mult;
        context.back_chance = _throw_info[pts].back;
        var_set_bool(res, plr_throw(&context));
        break; }
    case SPELL_ENERGY: {
        int pts = _get_skill_pts(_TYPE_SHOOT, _THROWING);
        var_set_int(res, _throw_info[pts].energy);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

/************************************************************************
 * Magic/Prayer Skills
 ***********************************************************************/
static void _magic_init_class(class_t *class_ptr)
{
    typedef struct { int base_dev; int xtra_dev; int int_; int str; int con; } _magic_skill_t;
    static _magic_skill_t _tbl[16] = {
        { 23, 45, 0,  0,  0 },

        { 25, 45, 1,  0,  0 },
        { 27, 46, 1,  0,  0 },
        { 29, 47, 2,  0,  0 },
        { 31, 48, 2,  0, -1 },
        { 33, 49, 3, -1, -1 },

        { 33, 50, 3, -1, -1 },
        { 35, 51, 3, -2, -1 },
        { 37, 52, 3, -2, -2 },
        { 39, 53, 3, -2, -2 },
        { 41, 54, 4, -3, -2 },

        { 43, 55, 5, -3, -3 },
        { 45, 56, 5, -4, -3 },
        { 47, 57, 5, -4, -3 },
        { 50, 58, 6, -4, -3 },
        { 55, 60, 7, -5, -3 }
    };
    int pts = _get_group_pts(_TYPE_MAGIC);
    _magic_skill_t row = _tbl[MIN(15, pts)];
    class_ptr->skills.dev += row.base_dev;
    class_ptr->extra_skills.dev += row.xtra_dev;
    class_ptr->stats[A_INT] += row.int_;
    class_ptr->stats[A_STR] += row.str;
    class_ptr->stats[A_CON] += row.con;
    class_ptr->life -= (pts + 1) / 2;

    pts = _get_skill_pts(_TYPE_MAGIC, REALM_TRUMP);
    class_ptr->pets -= 6 * pts;
    class_ptr->stats[A_CHR] += (pts + 1) / 2;

    pts = _get_skill_pts(_TYPE_MAGIC, REALM_SORCERY);
    class_ptr->skills.dev += pts;

    pts = _get_skill_pts(_TYPE_MAGIC, REALM_CRAFT);
    class_ptr->skills.thn += 3*pts;
    class_ptr->skills.thb += 2*pts;
}

static void _prayer_init_class(class_t *class_ptr)
{
    typedef struct { int base_sav; int xtra_sav; int wis; } _prayer_skill_t;
    static _prayer_skill_t _tbl[11] = {
        { 31, 50, 0 },

        { 32, 55, 1 },
        { 33, 56, 1 },
        { 34, 57, 2 },
        { 35, 58, 2 },
        { 36, 59, 3 },

        { 38, 60, 3 },
        { 39, 61, 4 },
        { 40, 62, 4 },
        { 40, 65, 4 },
        { 40, 70, 4 }
    };
    int pts = _get_group_pts(_TYPE_PRAYER);
    _prayer_skill_t row = _tbl[MIN(10, pts)];
    class_ptr->skills.sav += row.base_sav;
    class_ptr->extra_skills.sav += row.xtra_sav;
    class_ptr->stats[A_WIS] += row.wis;
    class_ptr->skills.dev += (pts + 1) / 2;
}

typedef struct { int lvl_mult; int cost_mult; int fail_adj; int fail_min; } _realm_skill_t;
static _realm_skill_t _realm_skills[6] = {
    {   0,   0,  0,  0 },
    { 167, 200, 15,  5 },
    { 134, 150,  7,  5 },
    { 115, 125,  4,  4 },
    { 105, 110,  2,  3 },
    { 100,  95,  0,  0 }
};

static encumbrance_info _encumbrance_tbl[11] = {
    { 500,   0, 1200 },

    { 490,  10, 1200 },
    { 480,  20, 1100 },
    { 470,  30, 1000 },
    { 460,  50,  900 },
    { 450,  70,  800 },

    { 440,  90,  700 },
    { 430, 110,  600 },
    { 420, 140,  600 },
    { 410, 170,  600 },
    { 400, 200,  600 }
};

caster_info *_caster_info(void)
{
    static caster_info info = {0};
    int magic_pts = _get_group_pts(_TYPE_MAGIC);
    int prayer_pts = _get_group_pts(_TYPE_PRAYER);
    int melee_pts = _get_group_pts(_TYPE_MELEE);
    int music_pts = _get_skill_pts(_TYPE_TECHNIQUE, REALM_MUSIC);
    int enc_pts = magic_pts + (prayer_pts + 1) / 2;

    info.options = 0;
    info.encumbrance = _encumbrance_tbl[MIN(10, enc_pts)];

    /* XXX use skill points appropriate to current melee weapon */
    info.encumbrance.weapon_pct -= 7 * melee_pts;
    if (info.encumbrance.weapon_pct < 0)
        info.encumbrance.weapon_pct = 0;
    if (info.encumbrance.weapon_pct > 100)
        info.encumbrance.weapon_pct = 100;

    if (music_pts && music_pts >= magic_pts && music_pts >= prayer_pts)
    {
        info.which_stat = A_CHR;
        info.magic_desc = "song";
        info.encumbrance.max_wgt = 400;
        info.encumbrance.weapon_pct = 67;
        info.encumbrance.enc_wgt = 800;
        return &info;
    }
    if (magic_pts && magic_pts >= prayer_pts)
    {
        info.which_stat = A_INT;
        info.magic_desc = "spell";
        info.options |= CASTER_GLOVE_ENCUMBRANCE;
        if (magic_pts >= 5)
            info.options |= CASTER_ALLOW_DEC_MANA;
        return &info;
    }
    if (prayer_pts)
    {
        info.which_stat = A_WIS;
        info.magic_desc = "prayer";
        if (prayer_pts >= 5)
            info.options |= CASTER_ALLOW_DEC_MANA;
        return &info;
    }
    if (_get_skill_pts(_TYPE_TECHNIQUE, REALM_BURGLARY))
    {
        info.which_stat = A_DEX;
        info.magic_desc = "thieving talent";
        /* Since nothing but Burglary is known, use rogue encumbrance */
        info.encumbrance.max_wgt = 400;
        info.encumbrance.weapon_pct = 33;
        info.encumbrance.enc_wgt = 1000;
        return &info;
    }
    return NULL;
}

/************************************************************************
 * Spellcasting UI
 ***********************************************************************/
typedef struct {
    int level;
    int cost;
    int fail;
    int realm;
    int idx;
} _spell_info_t, *_spell_info_ptr;

static bool _has_magic(void)
{
    if ( !_get_group_pts(_TYPE_MAGIC)
      && !_get_group_pts(_TYPE_PRAYER)
      && !_get_skill_pts(_TYPE_TECHNIQUE, REALM_BURGLARY)
      && !_get_skill_pts(_TYPE_TECHNIQUE, REALM_MUSIC) )
    {
        return FALSE;
    }
    return TRUE;
}

int skillmaster_antimagic_prob(void)
{
    int pts = _get_group_pts(_TYPE_MAGIC);
    if (pts)
        return 100 + pts*pts*10;

    pts = _get_group_pts(_TYPE_PRAYER);
    if (pts)
        return 100 + pts*20;

    return 0;
}

static bool _can_cast(void)
{
    if (!_has_magic())
    {
        msg_print("You don't know any spell realms. Use the 'G' command to gain the appropriate skills.");
        flush();
        return FALSE;
    }
    if (plr_tim_find(T_BLIND) || no_light())
    {
        msg_print("You cannot see!");
        flush();
        return FALSE;
    }
    if (plr_tim_find(T_CONFUSED))
    {
        msg_print("You are too confused!");
        flush();
        return FALSE;
    }
    return TRUE;
}

static int _get_realm_pts(int realm)
{
    _skill_ptr s;
    if (realm == REALM_BURGLARY || realm == REALM_MUSIC)
    {
        s = _get_skill(_TYPE_TECHNIQUE, realm);
        assert(s);
        return s->current;
    }
    /* Assert: A realm, if known, is only known in either Magic or Prayer
     * but never in both */
    s = _get_skill(_TYPE_MAGIC, realm);
    if (s && s->current)
        return s->current;
    s = _get_skill(_TYPE_PRAYER, realm);
    if (s && s->current)
        return s->current;
    return 0;
}

static int _get_realm_stat(int realm)
{
    _skill_ptr s;
    if (realm == REALM_BURGLARY)
        return A_DEX;
    if (realm == REALM_MUSIC)
        return A_CHR;
    s = _get_skill(_TYPE_MAGIC, realm);
    if (s && s->current)
        return A_INT;
    s = _get_skill(_TYPE_PRAYER, realm);
    if (s && s->current)
        return A_WIS;
    return A_NONE;
}

static bool _spellbook_hook(obj_ptr obj)
{
    if (obj_is_spellbook(obj))
    {
        int realm = tval2realm(obj->tval);
        int pts = _get_realm_pts(realm);
        if (pts > 0)
            return TRUE;
    }
    return FALSE;
}

static object_type *_prompt_spellbook(void)
{
    obj_prompt_t prompt = {0};

    prompt.prompt = "Use which book?";
    prompt.error = "You have no books that you can read.";
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;
    prompt.filter = _spellbook_hook;

    obj_prompt(&prompt);
    return prompt.obj;
}

/* shared with spell table spoilers, so we pass in the skill points to use */
static _spell_info_ptr _get_spell(int realm, int idx, int pts)
{
    magic_type      info;
    _spell_info_ptr spell = malloc(sizeof(_spell_info_t));
    _realm_skill_t  skill = _realm_skills[pts];

    if (is_magic(realm))
        info = mp_ptr->info[realm - 1][idx];
    else
        info = technic_info[realm - MIN_TECHNIC][idx];

    spell->realm = realm;
    spell->idx = idx;
    spell->level = MAX(1, info.slevel * skill.lvl_mult / 100);
    spell->cost = MAX(1, info.smana * skill.cost_mult / 100);
    spell->fail = MIN(95, MAX(5, info.sfail + skill.fail_adj));

    return spell;
}

static vec_ptr _get_spell_list(object_type *spellbook)
{
    vec_ptr        v = vec_alloc(free);
    int            realm = tval2realm(spellbook->tval);
    int            pts = _get_realm_pts(realm);
    _realm_skill_t skill;
    int            start = 8*spellbook->sval;
    int            stop = start + 8;
    int            stat = plr->stat_ind[_get_realm_stat(realm)];
    int            i;

    assert(0 < pts && pts <= 5);
    skill = _realm_skills[pts];
    for (i = start; i < stop; i++)
    {
        _spell_info_ptr spell = _get_spell(realm, i, pts);

        if (spell->cost && is_magic(realm))
        {
            if (plr->easy_realm1 == realm || (plr->dec_mana && pts >= 4))
                spell->cost = MAX(1, (spell->cost + 1) * dec_mana_cost(plr->dec_mana) / 100);
        }
        spell->fail = virtue_mod_spell_fail(realm, spell->fail); /* Ditto with virtues */
        spell->fail = calculate_fail_rate_aux(plr->lev, realm, spell->level, spell->fail, stat);
        if (spell->fail < skill.fail_min)
            spell->fail = skill.fail_min;

        vec_add(v, spell);
    }
    return v;
}

/* This is shared with the character dump ... */
#define _SHOW_STATS 0x01
static void _list_spells(doc_ptr doc, object_type *spellbook, vec_ptr spells, int browse_idx, int options)
{
    int          i;
    object_kind *k_ptr = &k_info[spellbook->k_idx];

    doc_printf(doc, "<color:%c>%-27.27s</color> <color:G>Lvl  SP Fail %-15.15s",
        attr_to_attr_char(k_ptr->d_attr), k_ptr->name, "Desc");
    if (options & _SHOW_STATS)
        doc_insert(doc, "  Cast Fail");
    doc_insert(doc, "</color>\n");

    for (i = 0; i < vec_length(spells); i++)
    {
        _spell_info_ptr spell = vec_get(spells, i);
        if (spell->level > PY_MAX_LEVEL)
        {
            doc_printf(doc, " <color:D>%c) Illegible</color>\n", I2A(i));
        }
        else
        {
            doc_printf(doc, " <color:%c>%c</color>) ",
                (spell->level <= plr->lev && spell->cost <= plr->csp) ? 'y' : 'D', I2A(i));
            doc_printf(doc, "<color:%c>%-23.23s</color> ",
                i == browse_idx ? 'B' : 'w',
                do_spell(spell->realm, spell->idx, SPELL_NAME));
            doc_printf(doc, "%3d <color:%c>%3d</color> %3d%% ",
                spell->level,
                spell->cost <= plr->csp ? 'w' : 'r',
                spell->cost, spell->fail);
            if (spell->level <= plr->lev)
                doc_printf(doc, "%-15.15s", do_spell(spell->realm, spell->idx, SPELL_INFO));
            else
                doc_printf(doc, "%-15.15s", "");
            if (options & _SHOW_STATS)
            {
                spell_stats_ptr stats = spell_stats_old(spell->realm, spell->idx);
                if (stats->ct_cast + stats->ct_fail)
                {
                    doc_printf(doc, " %5d %4d %3d%%",
                        stats->ct_cast,
                        stats->ct_fail,
                        spell_stats_fail(stats)
                    );
                }
            }
            doc_newline(doc);
        }
    }
}

static void _spoil_book(doc_ptr doc, int realm, int book)
{
    int idx, pts;
    int start = book * 8;
    int stop = start + 8;
    int k_idx = lookup_kind(realm2tval(realm), book);

    doc_printf(doc, "<color:G>%-20.20s</color>", k_info[k_idx].name);
    for (pts = 1; pts <= 5; pts++)
        doc_printf(doc, " <color:%c>Lv Cst Fail</color>", pts % 2 ? 'G' : 'R');
    doc_newline(doc);
    for (idx = start; idx < stop; idx++)
    {
        doc_printf(doc, "%-20.20s", do_spell(realm, idx, SPELL_NAME));
        for (pts = 1; pts <= 5; pts++)
        {
            _spell_info_ptr spell = _get_spell(realm, idx, pts);
            if (spell->level > PY_MAX_LEVEL)
                doc_insert(doc, " <color:D> Illegible </color>");
            else
            {
                doc_printf(doc, " <color:%c>%2d %3d %3d%%</color>",
                    pts % 2 ? 'w' : 'U', spell->level, spell->cost, spell->fail);
            }
            free(spell);
        }
        doc_newline(doc);
    }
    doc_newline(doc);
}

static void _spoil_realm(doc_ptr doc, int realm)
{
    int book;
    doc_insert(doc, "<style:table>");
    for (book = 0; book < 4; book++)
        _spoil_book(doc, realm, book);
    doc_insert(doc, "</style>");
}

#define _BROWSE_MODE 0x01
static bool _prompt_spell(object_type *spellbook, _spell_info_ptr chosen_spell, int options)
{
    bool    result = FALSE;
    vec_ptr spells = _get_spell_list(spellbook);
    int     browse_idx = -1, cmd, i;

    if (REPEAT_PULL(&cmd))
    {
        i = A2I(cmd);
        if (0 <= i && i < vec_length(spells))
        {
            _spell_info_ptr spell = vec_get(spells, i);
            if (spell->level <= plr->lev && spell->cost <= plr->csp)
            {
                *chosen_spell = *spell;
                vec_free(spells);
                return TRUE;
            }
        }
    }

    assert(!_doc);
    _doc = doc_alloc(72);

    msg_line_clear();
    Term_save();
    for (;;)
    {
        doc_clear(_doc);
        _list_spells(_doc, spellbook, spells, browse_idx, 0);
        if (0 <= browse_idx && browse_idx < vec_length(spells))
        {
            _spell_info_ptr spell = vec_get(spells, browse_idx);
            doc_newline(_doc);
            doc_insert(_doc, do_spell(spell->realm, spell->idx, SPELL_DESC));
            doc_newline(_doc);
        }
        _sync_term(_doc);
        cmd = _inkey();
        if (cmd == ESCAPE) break;
        else if (isupper(cmd))
        {
            i = A2I(tolower(cmd));
            if (0 <= i && i < vec_length(spells))
                browse_idx = i;
        }
        else if (islower(cmd))
        {
            i = A2I(cmd);
            if (0 <= i && i < vec_length(spells))
            {
                if (options & _BROWSE_MODE)
                    browse_idx = i;
                else
                {
                    _spell_info_ptr spell = vec_get(spells, i);
                    if (spell->level <= plr->lev && spell->cost <= plr->csp)
                    {
                        *chosen_spell = *spell;
                        result = TRUE;
                        REPEAT_PUSH(cmd);
                        break;
                    }
                }
            }
        }
    }
    Term_load();

    vec_free(spells);
    doc_free(_doc);
    _doc = NULL;

    return result;
}

static void _cast_spell(_spell_info_ptr spell)
{
    assert(spell->level <= plr->lev);
    assert(spell->cost <= plr->csp);

    /* spells require vocalization, ending any active songs */
    if (spell->realm != REALM_MUSIC && music_current())
        music_stop();
    if (spell->realm != REALM_HEX && hex_count())
        hex_stop();
    if (spell->realm != REALM_BLESS && bless_count())
        bless_stop();

    plr->csp -= spell->cost;
    energy_use = 100;

    if (randint0(100) < spell->fail)
    {
        if (flush_failure) flush();

        cmsg_format(TERM_VIOLET, "You failed to cast <color:B>%s</color>!", do_spell(spell->realm, spell->idx, SPELL_NAME));
        if (demigod_is_(DEMIGOD_ATHENA))
            plr->csp += spell->cost/2;
        spell_stats_on_fail_old(spell->realm, spell->idx);
        sound(SOUND_FAIL);
        do_spell(spell->realm, spell->idx, SPELL_FAIL);
        virtue_on_fail_spell(spell->realm, spell->fail);
    }
    else
    {
        current_spell_cost = spell->cost; /* XXX needed for music|hex upkeep */
        if (!do_spell(spell->realm, spell->idx, SPELL_CAST))
        {  /* Canceled */
            plr->csp += spell->cost;
            energy_use = 0;
            return;
        }
        sound(SOUND_ZAP);
        spell_stats_on_cast_old(spell->realm, spell->idx);
        virtue_on_cast_spell(spell->realm, spell->cost, spell->fail);
    }
    plr->redraw |= PR_MANA;
    plr->window |= PW_SPELL;
}

void skillmaster_cast(void)
{
    if (_can_cast())
    {
        object_type  *spellbook = _prompt_spellbook();
        _spell_info_t spell = {0};

        if (!spellbook)
            return;
        if (tval2realm(spellbook->tval) == REALM_HEX)
        {
            if (hex_count() == hex_max())
            {
                bool flag = FALSE;
                msg_print("You cannot chant any more foul curses.");
                flush();
                if (plr->lev >= 35)
                {
                    msg_print(NULL);
                    flag = hex_stop_one();
                }
                if (!flag) return;
            }
        } 
        if (tval2realm(spellbook->tval) == REALM_BLESS)
        {
            if (bless_count() == bless_max())
            {
                bool flag = FALSE;
                msg_print("You cannot chant any more holy prayers.");
                flush();
                if (plr->lev >= 35)
                {
                    msg_print(NULL);
                    flag = bless_stop_one();
                }
                if (!flag) return;
            }
        } 
        if (_prompt_spell(spellbook, &spell, 0))
            _cast_spell(&spell);
    }
}

void skillmaster_browse(void)
{
    if (_can_cast())
    {
        object_type  *spellbook = _prompt_spellbook();
        _spell_info_t spell = {0};
        
        if (!spellbook)
            return;
        _prompt_spell(spellbook, &spell, _BROWSE_MODE);
    }
}

static bool _is_spellbook(int tval)
{
    return tv_is_spellbook(tval);
}

bool skillmaster_is_valid_realm(int realm)
{
    return _get_realm_pts(realm) > 0;
}
bool skillmaster_is_allowed_book(int tval, int sval) /* For autopick.c */
{
    if (!_is_spellbook(tval)) return FALSE;
    return skillmaster_is_valid_realm(tval2realm(tval));
}

/************************************************************************
 * Skills
 ***********************************************************************/
static void _skills_init_class(class_t *class_ptr)
{
    int pts;

    pts = _get_skill_pts(_TYPE_SKILLS, _AGILITY);
    class_ptr->stats[A_DEX] += pts;
    class_ptr->skills.dis += 25 + 7*pts;
    class_ptr->extra_skills.dis += 35 + 10*pts;

    pts = _get_skill_pts(_TYPE_SKILLS, _AWARENESS);
    class_ptr->skills.srh += 12 + 15*pts;
    class_ptr->skills.fos += 6 + 15*pts;
    if (!pts)
        class_ptr->flags = CLASS_SENSE1_MED | CLASS_SENSE1_WEAK;
    else
    {
        class_ptr->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_STRONG |
                           CLASS_SENSE2_FAST | CLASS_SENSE2_STRONG;
    }
    pts = _get_skill_pts(_TYPE_MELEE, _MARTIAL_ARTS);
    if (pts) class_ptr->flags |= CLASS_MARTIAL_ARTS;

    pts = _get_skill_pts(_TYPE_SKILLS, _DEVICE_SKILL);
    class_ptr->skills.dev += 10*pts;

    pts = _get_skill_pts(_TYPE_SKILLS, _HEALTH);
    class_ptr->stats[A_CON] += pts;
    class_ptr->life += 4*pts;
    class_ptr->base_hp += 7*pts;

    pts = _get_skill_pts(_TYPE_SKILLS, _MAGIC_RESISTANCE);
    class_ptr->skills.sav += 15*pts;

    pts = _get_skill_pts(_TYPE_SKILLS, _STEALTH);
    class_ptr->skills.stl += 3*pts;
}

static void _skills_calc_bonuses(void)
{
    int pts;
    pts = _get_skill_pts(_TYPE_SKILLS, _AWARENESS);
    switch (pts)
    {
    case 3: plr->telepathy = TRUE;
    case 2: plr->auto_pseudo_id = TRUE;
    case 1: plr->see_inv++;
    }

    pts = _get_skill_pts(_TYPE_SKILLS, _SPEED);
    plr->pspeed += 2*pts;

    pts = _get_skill_pts(_TYPE_SKILLS, _STEALTH);
    if (pts >= 3)
    {
        plr->ambush = 300;
        plr->shoot_sleeping = 300;
    }

    pts = _get_skill_pts(_TYPE_SKILLS, _DEVICE_POWER);
    plr->device_power += pts;

    pts = _get_skill_pts(_TYPE_SKILLS, _SPELL_CAPACITY);
    plr->spell_cap += 2*pts;

    pts = _get_skill_pts(_TYPE_SKILLS, _SPELL_POWER);
    plr->spell_power += pts;

}

void _skills_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    int pts;
    pts = _get_skill_pts(_TYPE_SKILLS, _AWARENESS);
    switch (pts)
    {
    case 3: add_flag(flgs, OF_TELEPATHY);
    case 2: add_flag(flgs, OF_LORE1);
    case 1: add_flag(flgs, OF_SEE_INVIS);
    }

    pts = _get_skill_pts(_TYPE_SKILLS, _SPEED);
    if (pts > 0)
        add_flag(flgs, OF_SPEED);

    pts = _get_skill_pts(_TYPE_SKILLS, _DEVICE_POWER);
    if (pts > 0)
        add_flag(flgs, OF_DEVICE_POWER);

    pts = _get_skill_pts(_TYPE_SKILLS, _SPELL_CAPACITY);
    if (pts > 0)
        add_flag(flgs, OF_SPELL_CAP);

    pts = _get_skill_pts(_TYPE_SKILLS, _SPELL_POWER);
    if (pts > 0)
        add_flag(flgs, OF_SPELL_POWER);
}

/************************************************************************
 * Techniques
 ***********************************************************************/
void _tech_init_class(class_t *class_ptr)
{
    int pts;

    pts = _get_skill_pts(_TYPE_TECHNIQUE, REALM_BURGLARY);
    class_ptr->skills.stl += (pts + 1) / 2;
    class_ptr->skills.dis += 5*pts;

    /* XXX nerf things so we don't obsolete bards */
    pts = _get_skill_pts(_TYPE_TECHNIQUE, REALM_MUSIC);
    class_ptr->skills.stl -= pts;
    class_ptr->stats[A_STR] -= pts/2;
    class_ptr->stats[A_CON] -= pts/2;
    class_ptr->stats[A_CHR] += pts/2;
    class_ptr->skills.thn -= 4*pts;
}

void _tech_calc_bonuses(void)
{
    int pts;

    pts = _get_skill_pts(_TYPE_TECHNIQUE, _RIDING);
    if (pts >= 5)
        plr->easy_capture = TRUE;
}

int skillmaster_riding_prof(void)
{
    int pts = _get_skill_pts(_TYPE_TECHNIQUE, _RIDING);
    int prof[6] = { 0, RIDING_EXP_BEGINNER, RIDING_EXP_SKILLED, RIDING_EXP_EXPERT, 6500, RIDING_EXP_MASTER };
    return prof[MIN(5, pts)];
}

int skillmaster_dual_wielding_prof(void)
{
    int pts = _get_skill_pts(_TYPE_TECHNIQUE, _DUAL_WIELDING);
    int prof[6] = { 0, WEAPON_EXP_BEGINNER, WEAPON_EXP_SKILLED, WEAPON_EXP_EXPERT, WEAPON_EXP_MASTER, WEAPON_EXP_MASTER };
    return prof[MIN(5, pts)];
}

/************************************************************************
 * Gain a Skill (UI)
 ***********************************************************************/
static int _confirm_skill_ui(_skill_ptr s)
{
    for (;;)
    {
        int cmd;

        doc_clear(_doc);
        doc_printf(_doc, "<color:v>Warning</color>: <indent>You are about to learn the <color:B>%s</color> skill. "
                         "Are you sure you want to do this? The skills you learn are permanent choices and you "
                         "won't be able to change your mind later!</indent>\n", s->name);
        doc_newline(_doc);
        doc_insert(_doc, "<color:y>RET</color>) <color:v>Accept</color>\n");
        doc_insert(_doc, "<color:y>ESC</color>) Cancel\n");

        _sync_term(_doc);
        cmd = _inkey();
        if (cmd == ESCAPE) return UI_CANCEL;
        else if (cmd == '\r') return UI_OK;
    }
}

static bool _can_gain_skill(_skill_ptr s)
{
    if (!_get_free_pts()) return FALSE;
    if (s->current == s->max) return FALSE;
    if (s->type == _TYPE_MAGIC)
    {
        _skill_ptr s2 = _get_skill(_TYPE_PRAYER, s->subtype);
        if (s2 && s2->current) return FALSE;
    }
    else if (s->type == _TYPE_PRAYER)
    {
        _skill_ptr s2 = _get_skill(_TYPE_MAGIC, s->subtype);
        if (s2 && s2->current) return FALSE;
    }
    return TRUE;
}

static int _get_skill_realm(_skill_ptr s)
{
    int realm = REALM_NONE;
    if (s->type == _TYPE_MAGIC || s->type == _TYPE_PRAYER)
        realm = s->subtype;
    else if (s->type == _TYPE_TECHNIQUE && s->subtype == REALM_BURGLARY)
        realm = s->subtype;
    else if (s->type == _TYPE_TECHNIQUE && s->subtype == REALM_MUSIC)
        realm = s->subtype;
    return realm;
}

static void _skill_display_help(_skill_ptr s)
{
    int realm = _get_skill_realm(s);
    if (realm != REALM_NONE)
    {
        if (plr->wizard || 0)
        {
            doc_ptr doc = doc_alloc(80);
            _spoil_realm(doc, realm);
            doc_display(doc, s->name, 0);
            doc_free(doc);
        }
        else if (realm == REALM_BURGLARY)
            doc_display_help("Skillmasters.txt", s->name);
        else
            doc_display_help("magic.txt", s->name);
    }
    else
        doc_display_help("Skillmasters.txt", s->name);
}

static int _gain_skill_ui(_group_ptr g)
{
    int cmd = 0;
    REPEAT_PULL(&cmd);
    for (;;)
    {
        int     i, ct = _get_skill_ct_aux(g);
        int     free_pts = _get_free_pts();
        int     group_pts = _get_group_pts_aux(g);
        doc_ptr cols[2];

        if (!cmd) /* repeat? */
        {
            cols[0] = doc_alloc(40);
            cols[1] = doc_alloc(30);

            doc_clear(_doc);
            doc_printf(_doc, "%s\n\n", g->desc);
            doc_insert(cols[0], "<color:G>Choose the Skill</color>\n");
            for (i = 0; ; i++)
            {
                _skill_ptr s = &g->skills[i];
                if (!s->type) break;
                doc_printf(cols[0], "  <color:%c>%c</color>) %s",
                    _can_gain_skill(s) ? 'y' : 'D',
                    I2A(i),
                    s->name
                );
                if (s->current && s->max)
                    doc_printf(cols[0], " (%d%%)", 100*s->current/s->max);
                doc_newline(cols[0]);
            }
            doc_printf(cols[1], "%-10.10s: <color:%c>%d</color>\n", "Learned", group_pts ? 'G' : 'r', group_pts);
            doc_printf(cols[1], "%-10.10s: <color:%c>%d</color>\n", "Available", free_pts ? 'G' : 'r', free_pts);
            doc_insert(cols[1], "\n\n");
            doc_insert(cols[1], "  <color:y>?</color>) Help\n");
            doc_insert(cols[1], "<color:y>ESC</color>) Cancel\n");
            doc_insert_cols(_doc, cols, 2, 1);
            doc_free(cols[0]);
            doc_free(cols[1]);

            _sync_term(_doc);
            cmd = _inkey();
        }
        if (cmd == ESCAPE) return UI_CANCEL;
        else if (cmd == '?') doc_display_help("Skillmasters.txt", g->name);
        else if (plr->wizard && cmd == KTRL('R'))
        {
            if (get_check("Really reset this group? "))
            {
                _reset_group(g);
                plr->update |= PU_BONUS | PU_HP | PU_MANA;
                plr->redraw |= PR_EFFECTS | PR_HP | PR_MANA;
            }
        }
        else if (isupper(cmd))
        {
            i = A2I(tolower(cmd));
            if (0 <= i && i < ct)
                _skill_display_help(&g->skills[i]);
        }
        else
        {
            i = A2I(cmd);
            if (0 <= i && i < ct)
            {
                _skill_ptr s = &g->skills[i];
                if (_can_gain_skill(s))
                {
                    REPEAT_PUSH(cmd);
                    if(_confirm_skill_ui(s) == UI_OK)
                    {
                        s->current++;
                        if (s->type == _TYPE_ABILITY && s->subtype == _NOCTOVISION)
                            plr->update |= PU_UN_VIEW | PU_VIEW;
                        return UI_OK;
                    }
                    REPEAT_POP();
                }
            }
        }
        cmd = 0;
    }
}

static int _gain_type_ui(void)
{
    int cmd = 0;
    REPEAT_PULL(&cmd);
    for (;;)
    {
        int i, pts, ct = _get_group_ct();
        int free_pts = _get_free_pts();
        int learned_pts = _get_pts();
        doc_ptr cols[2];

        if (!cmd) /* repeat? */
        {
            cols[0] = doc_alloc(40);
            cols[1] = doc_alloc(30);

            doc_clear(_doc);
            doc_insert(_doc,
                "You must choose which <color:B>skills</color> to learn. The various skills are grouped "
                "into categories. Often, the total number of points in a given group determines broad level "
                "skills and perhaps grants boosts to your starting stats, while the specific skill points "
                "grant targetted benefits (such as proficiency with a class of weapons, or access to "
                "a specific spell realm). Details are given in the submenu for each group. Feel free to "
                "explore the various groups since you may always <color:keypress>ESC</color> to return here.\n\n");
            doc_insert(cols[0], "<color:G>Choose the Skill Type</color>\n");
            for (i = 0; ; i++)
            {
                _group_ptr g = &_groups[i];
                if (!g->type) break;
                pts = _get_group_pts_aux(g);
                doc_printf(cols[0], "  <color:y>%c</color>) %s", I2A(i), g->name);
                if (pts) doc_printf(cols[0], " (%d)", pts);
                doc_newline(cols[0]);
            }

            doc_printf(cols[1], "%-10.10s: <color:%c>%d</color>\n", "Learned", learned_pts ? 'G' : 'r', learned_pts);
            doc_printf(cols[1], "%-10.10s: <color:%c>%d</color>\n", "Available", free_pts ? 'G' : 'r', free_pts);
            doc_insert(cols[1], "\n\n");
            doc_insert(cols[1], "  <color:y>?</color>) Help\n");
            doc_insert(cols[1], "<color:y>ESC</color>) <color:v>Quit</color>\n");
            doc_insert_cols(_doc, cols, 2, 1);
            doc_free(cols[0]);
            doc_free(cols[1]);

            _sync_term(_doc);
            cmd = _inkey();
        }
        if (cmd == ESCAPE) return UI_CANCEL;
        else if (cmd == '?') doc_display_help("Skillmasters.txt", NULL);
        else if (plr->wizard && cmd == KTRL('R'))
        {
            if (get_check("Really reset all skills? "))
            {
                _reset_groups();
                plr->update |= PU_BONUS | PU_HP | PU_MANA;
                plr->redraw |= PR_EFFECTS | PR_HP | PR_MANA;
            }
        }
        else if (isupper(cmd))
        {
            i = A2I(tolower(cmd));
            if (0 <= i && i < ct)
            {
                _group_ptr g = &_groups[i];
                doc_display_help("Skillmasters.txt", g->name);
            }
        }
        else
        {
            i = A2I(cmd);
            if (0 <= i && i < ct)
            {
                _group_ptr g = &_groups[i];
                REPEAT_PUSH(cmd);
                if (_gain_skill_ui(g) == UI_OK) return UI_OK;
                REPEAT_POP();
            }
        }
        cmd = 0;
    }
}

void skillmaster_gain_skill(void)
{
    assert(!_doc);
    _doc = doc_alloc(80);

    msg_line_clear();
    Term_save();
    if (_gain_type_ui() == UI_OK)
    {
        plr->update |= PU_BONUS | PU_HP | PU_MANA;
        plr->redraw |= PR_EFFECTS | PR_HP | PR_MANA;
    }
    Term_load();

    doc_free(_doc);
    _doc = NULL;
}

/************************************************************************
 * Class
 ***********************************************************************/
static void _birth(void)
{
    _reset_groups();
    plr->au += 500; /* build your own class, so buy your own gear! */
}

static void _gain_level(int new_lvl)
{
    if (new_lvl % 5 == 0)
        plr->redraw |= PR_EFFECTS;
}

static void _calc_bonuses(void)
{
    _melee_calc_bonuses();
    _shoot_calc_bonuses();
    _skills_calc_bonuses();
    _tech_calc_bonuses();

    if (_get_skill_pts(_TYPE_ABILITY, _LOREMASTER))
        plr->auto_id = TRUE;
    if (_get_skill_pts(_TYPE_ABILITY, _LUCK))
        plr->good_luck = TRUE;
    if (_get_skill_pts(_TYPE_ABILITY, _REGENERATION))
        plr->regen += 150;
    if (_get_skill_pts(_TYPE_ABILITY, _CLEAR_MIND))
        plr->clear_mind = TRUE;
    if (_get_skill_pts(_TYPE_PRAYER, REALM_HEX))
        hex_calc_bonuses();
    if (_get_skill_pts(_TYPE_PRAYER, REALM_BLESS))
        bless_calc_bonuses();
    if (_get_skill_pts(_TYPE_ABILITY, _NOCTOVISION))
        plr->see_nocto = DUN_VIEW_MAX;
}

void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    _melee_get_flags(flgs);
    _skills_get_flags(flgs);
    if (_get_skill_pts(_TYPE_PRAYER, REALM_HEX))
        hex_get_flags(flgs);
}

static void _add_power(spell_info* spell, ang_spell fn)
{
    /* Powers are generally granted without cost, or fail. They are available
     * immediately upon purchase, and there is no relevant stat to consider */
    spell->level = 1;
    spell->cost = 0;
    spell->fail = 0;
    spell->fn = fn;
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    if (ct < max && _get_skill_pts(_TYPE_SHOOT, _THROWING) > 0)
        _add_power(&spells[ct++], _throw_weapon_spell);
    if (ct < max && _get_skill_pts(_TYPE_ABILITY, _CLEAR_MIND) > 0)
        _add_power(&spells[ct++], clear_mind_spell);
    if (ct < max && _get_skill_pts(_TYPE_ABILITY, _EAT_MAGIC) > 0)
        _add_power(&spells[ct++], eat_magic_spell);
    if (ct < max && _get_skill_pts(_TYPE_ABILITY, _MASSACRE) > 0)
        _add_power(&spells[ct++], massacre_spell);
    if (ct < max && _get_skill_pts(_TYPE_ABILITY, _PANIC_HIT) > 0)
        _add_power(&spells[ct++], panic_hit_spell);
    if (ct < max && _get_skill_pts(_TYPE_ABILITY, _RESISTANCE) > 0)
        _add_power(&spells[ct++], resistance_spell);
    if (ct < max && _get_skill_pts(_TYPE_ABILITY, _STONE_SKIN) > 0)
        _add_power(&spells[ct++], stone_skin_spell);
    if (ct < max && _get_skill_pts(_TYPE_ABILITY, _CREATE_AMMO) > 0)
        _add_power(&spells[ct++], create_ammo_spell);
    if (ct < max && _get_skill_pts(_TYPE_ABILITY, _RODEO) > 0)
        _add_power(&spells[ct++], rodeo_spell);
    if (ct < max && _get_skill_pts(_TYPE_ABILITY, _RUSH_ATTACK) > 0)
        _add_power(&spells[ct++], rush_attack_spell);
    if (ct < max && _get_skill_pts(_TYPE_TECHNIQUE, REALM_MUSIC) > 0)
        _add_power(&spells[ct++], music_stop_spell);
    if (ct < max && _get_skill_pts(_TYPE_PRAYER, REALM_HEX) > 0)
        _add_power(&spells[ct++], hex_stop_spell);
    if (ct < max && _get_skill_pts(_TYPE_PRAYER, REALM_BLESS) > 0)
        _add_power(&spells[ct++], bless_stop_spell);

    return ct;
}

static void _dump_book(doc_ptr doc, object_type *spellbook)
{
    vec_ptr spells = _get_spell_list(spellbook);
    _list_spells(doc, spellbook, spells, -1, _SHOW_STATS);
    doc_newline(doc);
    vec_free(spells);
}

static object_type *_find_book(int realm, int book)
{
    int    tval = realm2tval(realm);
    int    sval = book;
    slot_t slot = pack_find_obj(tval, sval);

    if (slot) return pack_obj(slot);
    return NULL;
}

static void _dump_realm(doc_ptr doc, int realm)
{
    int i;
    bool first = TRUE;
    for (i = 0; i < 4; i++)
    {
        object_type *spellbook = _find_book(realm, i);
        if (spellbook)
        {
            if (first)
            {
                doc_printf(doc, "<color:r>Realm:</color> <color:B>%s</color>\n\n", realm_names[realm]);
                first = FALSE;
            }
            _dump_book(doc, spellbook);
        }
    }

    i = virtue_mod_spell_fail(realm, 0);
    if (!first && i)
        doc_printf(doc, " Your alignment is adding <color:R>%+d%%</color> to your fail rates in this realm.\n\n", i);
}

static void _dump_group(doc_ptr doc, _group_ptr g)
{
    int pts = _get_group_pts_aux(g);
    if (pts > 0)
    {
        int i;
        doc_printf(doc, "<color:B>%s</color> (%d)\n", g->name, pts);
        for (i = 0; ; i++)
        {
            _skill_ptr s = &g->skills[i];
            if (!s->type) break;
            if (!s->current) continue;
            doc_printf(doc, "  %s", s->name);
            if (s->max)
                doc_printf(doc, " (%d%%)\n", s->current * 100 / s->max);
            else
                doc_printf(doc, " (%d)\n", s->current);
        }
        doc_newline(doc);
    }
}

static void _character_dump(doc_ptr doc)
{
    int i;
    if (_get_skill_pts(_TYPE_SHOOT, _THROWING))
    {
        plr_throw_t context = {0};
        int         pts = _get_skill_pts(_TYPE_SHOOT, _THROWING);

        context.type = THROW_BOOMERANG | THROW_DISPLAY;
        context.mult = _throw_info[pts].mult;
        context.back_chance = _throw_info[pts].back;
        context.energy = _throw_info[pts].energy;

        doc_insert(doc, "<topic:Throwing>=================================== <color:keypress>T</color>hrowing ==================================\n\n");
        for (i = 0; i < MAX_HANDS; i++)
        {
            if (plr->attack_info[i].type != PAT_WEAPON) continue;
            context.obj = equip_obj(plr->attack_info[i].slot);
            plr_throw_doc(&context, doc);
        }
    }
    doc_printf(doc, "<topic:Skills>==================================== <color:keypress>S</color>kills ===================================\n\n");
    for (i = 0; ; i++)
    {
        _group_ptr g = &_groups[i];
        if (!g->type) break;
        _dump_group(doc, g);
    }

    if (_has_magic())
    {
        doc_printf(doc, "<topic:Spells>==================================== <color:keypress>S</color>pells ===================================\n\n");
        for (i = REALM_LIFE; i <= MAX_REALM; i++)
        {
            int pts = _get_realm_pts(i);
            if (pts > 0)
                _dump_realm(doc, i);
        }
    }
}

/* For the sake of flavor, we will tweak the extra hp allocation
 * to match the player's current skill allocation. This makes buying
 * early health more useful and also make mage-like skillmasters
 * less advantageous than proper mages. */
int skillmaster_calc_xtra_hp(int amt)
{
    int w1 = 0, w2 = 0, w3 = 0;

    w1 += _get_group_pts(_TYPE_MELEE);
    w1 += 2 * _get_skill_pts(_TYPE_SKILLS, _HEALTH);

    w2 += _get_group_pts(_TYPE_SHOOT);
    w2 += _get_group_pts(_TYPE_PRAYER);
    w2 += _get_skill_pts(_TYPE_TECHNIQUE, REALM_MUSIC);
    w2 += _get_skill_pts(_TYPE_TECHNIQUE, REALM_BURGLARY);

    w3 += _get_group_pts(_TYPE_MAGIC);
    w3 += _get_skill_pts(_TYPE_TECHNIQUE, REALM_MUSIC);

    if (w1 + w2 + w3 == 0)
        return plr_prorata_level_aux(amt, 1, 1, 1);

    return plr_prorata_level_aux(amt, w1, w2, w3);
}

plr_class_ptr skillmaster_get_class(void)
{
    static plr_class_ptr me = NULL;
    int i;

    if (!me)
    {
        me = plr_class_alloc(CLASS_SKILLMASTER);
        me->name = "Skillmaster";
        me->desc = "The Skillmaster is not your ordinary class. Instead, you "
                  "may design your own class, on the fly, using a point based "
                  "skill system. Upon birth, you will get 5 points to spend "
                  "and you should use these wisely to set the basic direction "
                  "of your class. Upon gaining every fifth level, you will "
                  "receive an additional point to spend, for fifteen points "
                  "overall. You may use these points to learn melee or to "
                  "gain access to spell realms. You can improve your speed or "
                  "your stealth. Or you may gain increased magic device skills. "
                  "In addition, you may learn riding skills, dual wielding or "
                  "even martial arts. You will have a lot of flexibility in "
                  "how you choose to play this class.\n \n"
                  "Most skills allow the investment of multiple points for "
                  "increased proficiency, but some are abilities that you may "
                  "buy with a single point (e.g. Luck). This class is not recommended for "
                  "beginning or immediate players. You only have a limited "
                  "amount of points to spend, and your choices are irreversible.";

        me->exp = 130;

        me->hooks.birth = _birth;
        me->hooks.character_dump = _character_dump;
        me->hooks.caster_info = _caster_info;
        me->hooks.gain_level = _gain_level;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.get_powers = _get_powers;
        me->hooks.get_flags = _get_flags;
        me->hooks.load_player = _load_player;
        me->hooks.save_player = _save_player;
    }
    /* Reset Stats and Skills on Each Call. Having these things
     * calculated here, rather than in calc_bonuses, enhances the
     * flavor of "building your own class" */
    for (i = 0; i < MAX_STATS; i++)
        me->stats[i] = 0;

    skills_wipe(&me->skills);
    skills_wipe(&me->extra_skills);

    me->life = 100;
    me->base_hp = 10;
    me->pets = 40;
    me->flags = 0;

    /* Rebuild the class_t, using the current skill allocation */
    if (!spoiler_hack && !birth_hack)
    {
        _melee_init_class(me);
        _shoot_init_class(me);
        _magic_init_class(me);
        _prayer_init_class(me);
        _skills_init_class(me);
        _tech_init_class(me);
    }

    return me;
}

