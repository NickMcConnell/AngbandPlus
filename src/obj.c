#include "angband.h"

#include "int_map.h"
#include "str_map.h"
#include <assert.h>

/************************************************************************
 * Dice (XdY+Z)
 ************************************************************************/
static int _scale(int amt, int per_mil)
{
    if (!per_mil) return amt; /* as always, 0 => no scaling */
    return (amt * per_mil + 500)/1000;
}
dice_t dice_create(int dd, int ds, int base)
{
    dice_t dice = {0};
    dice.dd = dd;
    dice.ds = ds;
    dice.base = base;
    return dice;
}
int dice_roll(dice_t dice)
{
    int roll = dice.base;
    if (dice.dd && dice.ds)
        roll += damroll(dice.dd, dice.ds);
    roll = _scale(roll, dice.scale);
    assert(roll >= 0);
    return MAX(0, roll);
}
dice_t dice_load(savefile_ptr file)
{
    dice_t dice = {0};
    dice.dd = savefile_read_s16b(file);
    dice.ds = savefile_read_s16b(file);
    dice.base = savefile_read_s16b(file);
    return dice;
}
void dice_save(dice_t dice, savefile_ptr file)
{
    savefile_write_s16b(file, dice.dd);
    savefile_write_s16b(file, dice.ds);
    savefile_write_s16b(file, dice.base);
}
int dice_lo_roll(dice_t dice)
{
    int dam = dice.base;
    if (dice.dd && dice.ds)
        dam += dice.dd;
    dam = _scale(dam, dice.scale);
    return dam;
}
int dice_avg_roll(dice_t dice)
{
    int avg = dice.base;
    if (dice.dd && dice.ds)
        avg += dice.dd*(dice.ds + 1)/2;
    avg = _scale(avg, dice.scale);
    return avg;
}
int dice_hi_roll(dice_t dice)
{
    int dam = dice.base;
    if (dice.dd && dice.ds)
        dam += dice.dd * dice.ds;
    dam = _scale(dam, dice.scale);
    return dam;
}
cptr dice_format(dice_t dice)
{
    /* XXX assert(dice == dice_parse(dice_format(dice)))
     * We'll lose scale info, though, but that is always temporary. */
    return dice_info(dice, "");
}
cptr dice_info(dice_t dice, cptr heading)
{
    if (dice.dd && dice.ds && dice.base)
        return format("%s%dd%d+%d", heading, dice.dd, dice.ds, dice.base);
    else if (dice.dd && dice.ds)
        return format("%s%dd%d", heading, dice.dd, dice.ds);
    else if (dice.base)
        return format("%s%d", heading, dice.base);
    return ""; /* XXX or "0"? */
}
errr dice_parse(dice_ptr dice, char *token)
{
    char arg[100], sentinel = '~', check;
    int dd, ds, base;

    sprintf(arg, "%s%c", token, sentinel);
    if (4 == sscanf(arg, "%dd%d+%d%c", &dd, &ds, &base, &check) && check == sentinel)
    {
        if (dd <= 0 || ds <= 0 || base <= 0)
        {
            msg_print("Error: Specifiy die rolls with XdY+Z for positive X, Y and Z.");
            return PARSE_ERROR_GENERIC;
        }
        dice->dd = dd;
        dice->ds = ds;
        dice->base = base;
    }
    else if (3 == sscanf(arg, "%dd%d%c", &dd, &ds, &check) && check == sentinel)
    {
        if (dd <= 0 || ds <= 0)
        {
            msg_print("Error: Specifiy die rolls with XdY for positive X, and Y.");
            return PARSE_ERROR_GENERIC;
        }
        dice->dd = dd;
        dice->ds = ds;
        dice->base = 0;
    }
    else if (2 == sscanf(arg, "%d%c", &base, &check) && check == sentinel)
    {
        if (base < 0)
        {
            msg_print("Error: Negative values are not supported.");
            return PARSE_ERROR_GENERIC;
        }
        dice->dd = 0;
        dice->ds = 0;
        dice->base = base;
    }
    else
    {
        msg_format("Error: Invalid dice token: %s. Expected XdY+Z or XdY or Z", token);
        return PARSE_ERROR_GENERIC;
    }
    return 0;
}
void dice_doc(dice_t dice, doc_ptr doc)
{
    if (dice.dd && dice.ds)
    {
        doc_printf(doc, "%dd%d", dice.dd, dice.ds);
        if (dice.base)
            doc_printf(doc, "+%d", dice.base);
    }
    else if (dice.base)
        doc_printf(doc, "%d", dice.base);
}
cptr dice_info_dam(dice_t dice)
{
    int dam = dice_avg_roll(dice);
    if (dice.dd && dice.ds)
        return format("dam ~%d", dam); /* approximate */
    return format("dam %d", dam); /* exact */
}
cptr dice_info_dam_each(dice_t dice)
{
    int dam = dice_avg_roll(dice);
    if (dice.dd && dice.ds)
        return format("dam ~%d each", dam); /* approximate */
    return format("dam %d each", dam); /* exact */
}
cptr dice_info_heal(dice_t dice)
{
    int amt = dice_avg_roll(dice);
    if (dice.dd && dice.ds)
        return format("heal ~%d", amt); /* approximate */
    return format("heal %d", amt); /* exact */
}
cptr dice_info_range(dice_t dice)
{
    return dice_info(dice, "range ");
}
cptr dice_info_dur(dice_t dice)
{
    return dice_info(dice, "dur ");
}
cptr dice_info_power(dice_t dice)
{
    int amt = dice_avg_roll(dice);
    if (dice.dd && dice.ds)
        return format("power ~%d", amt); /* approximate */
    return format("power %d", amt); /* exact */
}
/************************************************************************
 * Object Types
 ************************************************************************/
#define _SPELLBOOK (TVF_SPELLBOOK | TVF_STACKABLE | TVF_HATES_FIRE)
static tv_info_t _tv_tbl[] = {
    { TV_NONE, "None", TERM_WHITE },
    { TV_SKELETON, "Skeleton", TERM_WHITE, {"SKELETON"}, TVF_JUNK | TVF_HATES_ACID },  /* cf K&R A.8.7 */
    { TV_BOTTLE, "Bottle", TERM_WHITE, {"BOTTLE"},
        TVF_JUNK | TVF_STACKABLE | TVF_HATES_ACID | TVF_HATES_COLD, 30 },
    { TV_JUNK, "Junk", TERM_WHITE, {"JUNK"}, TVF_JUNK | TVF_HATES_ACID },
    { TV_WHISTLE, "Whistle", TERM_ORANGE, {"WHISTLE"}, TVF_MISC | TVF_STACKABLE, 10 },
    { TV_SPIKE, "Spike", TERM_SLATE, {"SPIKE"}, TVF_MISC | TVF_STACKABLE, 99 },
    { TV_CHEST, "Chest", TERM_SLATE, {"CHEST", "&"}, TVF_MISC | TVF_HATES_ACID | TVF_HATES_FIRE },
    { TV_FIGURINE, "Figurine", TERM_SLATE, {"FIGURINE"}, TVF_MISC },
    { TV_STATUE, "Statue", TERM_SLATE, {"STATUE"}, TVF_MISC },
    { TV_CORPSE, "Corpse", TERM_SLATE, {"CORPSE"}, TVF_STACKABLE, 10 },
    { TV_CAPTURE, "Capture Ball", TERM_L_BLUE, {"CAPTURE", "CAPTURE_BALL"}, TVF_MISC},
    { TV_SHOT, "Shot", TERM_L_UMBER, {"SHOT"}, TVF_AMMO | TVF_STACKABLE, 99 },
    { TV_ARROW, "Arrow", TERM_L_UMBER, {"ARROW"},
        TVF_AMMO | TVF_STACKABLE | TVF_HATES_ACID | TVF_HATES_FIRE, 99 },
    { TV_BOLT, "Bolt", TERM_L_UMBER, {"BOLT"}, TVF_AMMO | TVF_STACKABLE | TVF_HATES_ACID, 99 },
    { TV_BOW, "Bow", TERM_UMBER, {"BOW"},
        TVF_BOW | TVF_STACKABLE | TVF_HATES_ACID | TVF_HATES_FIRE, 5 },
    { TV_DIGGING, "Digger", TERM_SLATE, {"DIGGING", "DIGGER"}, TVF_WEAPON | TVF_STACKABLE, 5 },
    { TV_HAFTED, "Hafted Weapon", TERM_WHITE, {"HAFTED", "\\"},
        TVF_WEAPON | TVF_STACKABLE | TVF_HATES_ACID | TVF_HATES_FIRE, 5 },
    { TV_POLEARM, "Polearm", TERM_WHITE, {"POLEARM", "/"},
        TVF_WEAPON | TVF_STACKABLE | TVF_HATES_ACID | TVF_HATES_FIRE, 5 },
    { TV_SWORD, "Sword", TERM_WHITE, {"SWORD", "|"},
        TVF_WEAPON | TVF_STACKABLE | TVF_HATES_ACID, 5 },
    { TV_BOOTS, "Boots", TERM_L_UMBER, {"BOOTS"},
        TVF_BOOTS | TVF_STACKABLE | TVF_HATES_ACID | TVF_HATES_FIRE, 5 },
    { TV_GLOVES, "Gloves", TERM_L_UMBER, {"GLOVES"},
        TVF_GLOVES | TVF_STACKABLE | TVF_HATES_ACID | TVF_HATES_FIRE, 5 },
    { TV_HELM, "Helmet", TERM_L_UMBER, {"HELM", "HELMET"},
        TVF_HELMET | TVF_STACKABLE | TVF_HATES_ACID, 5 },
    { TV_CROWN, "Crown", TERM_L_UMBER, {"CROWN"},
        TVF_HELMET | TVF_STACKABLE | TVF_HATES_ACID, 5 },
    { TV_SHIELD, "Shield", TERM_L_UMBER, {"SHIELD"},
        TVF_SHIELD | TVF_STACKABLE | TVF_HATES_ACID, 5 },
    { TV_CLOAK, "Cloak", TERM_L_UMBER, {"CLOAK"},
        TVF_CLOAK | TVF_STACKABLE | TVF_HATES_ACID | TVF_HATES_FIRE, 5 },
    { TV_SOFT_ARMOR, "Soft Armor", TERM_SLATE, {"SOFT_ARMOR"},
        TVF_BODY_ARMOR | TVF_STACKABLE | TVF_HATES_ACID | TVF_HATES_FIRE, 5 },
    { TV_HARD_ARMOR, "Hard Armor", TERM_SLATE, {"HARD_ARMOR"},
        TVF_BODY_ARMOR | TVF_HATES_ACID },
    { TV_DRAG_ARMOR, "Dragon Armor", TERM_SLATE, {"DRAG_ARMOR", "DSM"},
        TVF_BODY_ARMOR | TVF_HATES_ACID },
    { TV_LIGHT, "Light", TERM_YELLOW, {"LIGHT", "~"},
        TVF_LIGHT | TVF_STACKABLE | TVF_HATES_FIRE, 5 },
    { TV_AMULET, "Amulet", TERM_ORANGE, {"AMULET", "\""},
        TVF_AMULET | TVF_STACKABLE, 10 },
    { TV_RING, "Ring", TERM_RED, {"RING", "="},
        TVF_RING | TVF_STACKABLE | TVF_HATES_ELEC, 10 },
    { TV_QUIVER, "Quiver", TERM_L_UMBER, {"QUIVER"}, TVF_QUIVER },
    { TV_CARD, "Card", TERM_L_GREEN, {"CARD"}, TVF_SHIELD },
    { TV_STAFF, "Staff", TERM_L_UMBER, {"STAFF", "_"},
        TVF_DEVICE | TVF_HATES_ACID | TVF_HATES_FIRE },
    { TV_WAND, "Wand", TERM_GREEN, {"WAND", "-"}, TVF_DEVICE | TVF_HATES_ELEC },
    { TV_ROD, "Rod", TERM_VIOLET, {"ROD"}, TVF_DEVICE },
    { TV_PARCHMENT, "Parchment", TERM_ORANGE, {"PARCHMENT"} },
    { TV_SCROLL, "Scroll", TERM_WHITE, {"SCROLL", "?"},
        TVF_SCROLL | TVF_STACKABLE | TVF_HATES_ACID | TVF_HATES_FIRE, 20 },
    { TV_POTION, "Potion", TERM_L_BLUE, {"POTION", "!"},
        TVF_POTION | TVF_STACKABLE | TVF_HATES_COLD, 25 },
    { TV_FLASK, "Flask", TERM_YELLOW, {"FLASK"},
        TVF_MISC | TVF_STACKABLE | TVF_HATES_COLD, 25 },
    { TV_FOOD, "Food", TERM_L_UMBER, {"FOOD", ","},
        TVF_FOOD | TVF_STACKABLE, 50 },
    { TV_RUNE, "Rune", TERM_L_BLUE, {"RUNE"} },
    { TV_LIFE_BOOK, "Life Book", TERM_L_WHITE, {"LIFE_BOOK"}, _SPELLBOOK, 5 },
    { TV_SORCERY_BOOK, "Sorcery Book", TERM_L_BLUE, {"SORCERY_BOOK"}, _SPELLBOOK, 5 },
    { TV_NATURE_BOOK, "Nature Book", TERM_L_GREEN, {"NATURE_BOOK"}, _SPELLBOOK, 5 },
    { TV_CHAOS_BOOK, "Chaos Book", TERM_L_RED, {"CHAOS_BOOK"}, _SPELLBOOK, 5 },
    { TV_DEATH_BOOK, "Death Book", TERM_L_DARK, {"DEATH_BOOK"}, _SPELLBOOK, 5 },
    { TV_TRUMP_BOOK, "Trump Book", TERM_ORANGE, {"TRUMP_BOOK"}, _SPELLBOOK, 5 },
    { TV_ARCANE_BOOK, "Arcane Book", TERM_SLATE, {"ARCANE_BOOK"}, _SPELLBOOK, 5 },
    { TV_CRAFT_BOOK, "Craft Book", TERM_YELLOW, {"CRAFT_BOOK"}, _SPELLBOOK, 5 },
    { TV_DAEMON_BOOK, "Daemon Book", TERM_RED, {"DAEMON_BOOK", "DEMON_BOOK"}, _SPELLBOOK, 5 },
    { TV_CRUSADE_BOOK, "Crusade Book", TERM_WHITE, {"CRUSADE_BOOK"}, _SPELLBOOK, 5 },
    { TV_NECROMANCY_BOOK, "Necromancy Book", TERM_L_DARK, {"NECROMANCY_BOOK"}, _SPELLBOOK, 5 },
    { TV_ARMAGEDDON_BOOK, "Armageddon Book", TERM_L_RED, {"ARMAGEDDON_BOOK"}, _SPELLBOOK, 5 },
    { TV_ILLUSION_BOOK, "Book of Illusions", TERM_L_BLUE,{"ILLUSION_BOOK"}, _SPELLBOOK, 5 },
    { TV_MUSIC_BOOK, "Music Book", TERM_GREEN, {"MUSIC_BOOK"}, _SPELLBOOK, 5 },
    { TV_HISSATSU_BOOK, "Kendo Book", TERM_UMBER, {"HISSATSU_BOOK", "KENDO_BOOK"}, _SPELLBOOK, 5 },
    { TV_HEX_BOOK, "Malediction Book", TERM_VIOLET, {"HEX_BOOK"}, _SPELLBOOK, 5 },
    { TV_RAGE_BOOK, "Rage Book", TERM_L_BLUE, {"RAGE_BOOK"}, _SPELLBOOK, 5 },
    { TV_BURGLARY_BOOK, "Burglary Book", TERM_UMBER, {"BURGLARY_BOOK"}, _SPELLBOOK, 5 },
    { TV_BLESS_BOOK, "Benediction Book", TERM_WHITE, {"BLESS_BOOK"}, _SPELLBOOK, 5 },
    { TV_GOLD, "Gold", TERM_YELLOW, {"GOLD", "$"} },
    { 0 }
};
tv_info_ptr tv_parse_name(cptr token)
{
    static str_map_ptr _map = NULL;
    if (!_map)
    {
        int i, j;
        _map = str_map_alloc(NULL);
        for (i = 0; ; i++)
        {
            tv_info_ptr info = &_tv_tbl[i];
            if (!info->name) break;
            for (j = 0; j < 3; j++)
            {
                cptr parse = info->parse[j];
                if (!parse) break;
                str_map_add(_map, parse, info);
            }
        }
    }
    return str_map_find(_map, token);
}

tv_info_ptr tv_lookup(int id)
{
    static int_map_ptr _map = NULL;
    tv_info_ptr info = NULL;
    if (!_map)
    {
        int i;
        _map = int_map_alloc(NULL);
        for (i = 0; ; i++)
        {
            tv_info_ptr info = &_tv_tbl[i];
            if (!info->name) break;
            info->sort = i; /* XXX still not being used ... cf obj_cmp */
            int_map_add(_map, info->id, info);
        }
    }
    info = int_map_find(_map, id);
    /*if (!info) info = int_map_find(_map, TV_NONE); XXX probably better to just blow up! */
    return info;
}

byte tv_color(int id)
{
    byte color = TERM_WHITE;
    tv_info_ptr info = tv_lookup(id);
    if (info) color = info->color;
    return color;
}

vec_ptr tv_lookup_(int flag)
{
    vec_ptr v = vec_alloc(NULL);
    int     i;

    for (i = 0; ; i++)
    {
        tv_info_ptr info = &_tv_tbl[i];
        if (!info->name) break;
        if (info->flags & flag)
            vec_add(v, info);
    }
    return v;
}

bool tv_is_weapon(int id)     { return BOOL(tv_lookup(id)->flags & TVF_WEAPON); }
bool tv_is_shield(int id)     { return BOOL(tv_lookup(id)->flags & TVF_SHIELD); }
bool tv_is_bow(int id)        { return BOOL(tv_lookup(id)->flags & TVF_BOW); }
bool tv_is_quiver(int id)     { return BOOL(tv_lookup(id)->flags & TVF_QUIVER); }
bool tv_is_ammo(int id)       { return BOOL(tv_lookup(id)->flags & TVF_AMMO); }
bool tv_is_ring(int id)       { return BOOL(tv_lookup(id)->flags & TVF_RING); }
bool tv_is_amulet(int id)     { return BOOL(tv_lookup(id)->flags & TVF_AMULET); }
bool tv_is_light(int id)       { return BOOL(tv_lookup(id)->flags & TVF_LIGHT); }
bool tv_is_body_armor(int id) { return BOOL(tv_lookup(id)->flags & TVF_BODY_ARMOR); }
bool tv_is_cloak(int id)      { return BOOL(tv_lookup(id)->flags & TVF_CLOAK); }
bool tv_is_helmet(int id)     { return BOOL(tv_lookup(id)->flags & TVF_HELMET); }
bool tv_is_gloves(int id)     { return BOOL(tv_lookup(id)->flags & TVF_GLOVES); }
bool tv_is_boots(int id)      { return BOOL(tv_lookup(id)->flags & TVF_BOOTS); }
bool tv_is_armor(int id)      { return BOOL(tv_lookup(id)->flags & TVF_ARMOR); }
bool tv_is_jewelry(int id)    { return BOOL(tv_lookup(id)->flags & TVF_JEWELRY); }
bool tv_is_wearable(int id)   { return BOOL(tv_lookup(id)->flags & TVF_WEARABLE); }
bool tv_is_enchantable(int id){ return BOOL(tv_lookup(id)->flags & TVF_ENCHANTABLE); }
bool tv_is_equipment(int id)  { return BOOL(tv_lookup(id)->flags & TVF_EQUIPMENT); }
bool tv_is_device(int id)     { return BOOL(tv_lookup(id)->flags & TVF_DEVICE); }
bool tv_is_spellbook(int id)  { return BOOL(tv_lookup(id)->flags & TVF_SPELLBOOK); }
bool tv_is_potion(int id)     { return BOOL(tv_lookup(id)->flags & TVF_POTION); }
bool tv_is_scroll(int id)     { return BOOL(tv_lookup(id)->flags & TVF_SCROLL); }
bool tv_is_junk(int id)       { return BOOL(tv_lookup(id)->flags & TVF_JUNK); }
bool tv_is_misc(int id)       { return BOOL(tv_lookup(id)->flags & TVF_MISC); }
bool tv_is_food(int id)       { return BOOL(tv_lookup(id)->flags & TVF_FOOD); }
bool tv_is_stackable(int id)  { return BOOL(tv_lookup(id)->flags & TVF_STACKABLE); }
bool tv_is_weapon_ammo(int id){ return BOOL(tv_lookup(id)->flags & (TVF_WEAPON | TVF_AMMO)); }
bool tv_is_bow_weapon_ammo(int id){ return BOOL(tv_lookup(id)->flags & (TVF_BOW | TVF_WEAPON | TVF_AMMO)); }

/************************************************************************
 * Object Kind
 ************************************************************************/
bool obj_kind_is_weapon(int k_idx)     { return tv_is_weapon(k_info[k_idx].tval); }
bool obj_kind_is_shield(int k_idx)     { return tv_is_shield(k_info[k_idx].tval); }
bool obj_kind_is_bow(int k_idx)        { return tv_is_bow(k_info[k_idx].tval); }
bool obj_kind_is_quiver(int k_idx)     { return tv_is_quiver(k_info[k_idx].tval); }
bool obj_kind_is_ammo(int k_idx)       { return tv_is_ammo(k_info[k_idx].tval); }
bool obj_kind_is_ring(int k_idx)       { return tv_is_ring(k_info[k_idx].tval); }
bool obj_kind_is_amulet(int k_idx)     { return tv_is_amulet(k_info[k_idx].tval); }
bool obj_kind_is_light(int k_idx)       { return tv_is_light(k_info[k_idx].tval); }
bool obj_kind_is_body_armor(int k_idx) { return tv_is_body_armor(k_info[k_idx].tval); }
bool obj_kind_is_cloak(int k_idx)      { return tv_is_cloak(k_info[k_idx].tval); }
bool obj_kind_is_helmet(int k_idx)     { return tv_is_helmet(k_info[k_idx].tval); }
bool obj_kind_is_gloves(int k_idx)     { return tv_is_gloves(k_info[k_idx].tval); }
bool obj_kind_is_boots(int k_idx)      { return tv_is_boots(k_info[k_idx].tval); }
bool obj_kind_is_armor(int k_idx)      { return tv_is_armor(k_info[k_idx].tval); }
bool obj_kind_is_jewelry(int k_idx)    { return tv_is_jewelry(k_info[k_idx].tval); }
bool obj_kind_is_wearable(int k_idx)   { return tv_is_wearable(k_info[k_idx].tval); }
bool obj_kind_is_enchantable(int k_idx){ return tv_is_enchantable(k_info[k_idx].tval); }
bool obj_kind_is_equipment(int k_idx)  { return tv_is_equipment(k_info[k_idx].tval); }
bool obj_kind_is_device(int k_idx)     { return tv_is_device(k_info[k_idx].tval); }
bool obj_kind_is_spellbook(int k_idx)  { return tv_is_spellbook(k_info[k_idx].tval); }
bool obj_kind_is_potion(int k_idx)     { return tv_is_potion(k_info[k_idx].tval); }
bool obj_kind_is_scroll(int k_idx)     { return tv_is_scroll(k_info[k_idx].tval); }
bool obj_kind_is_junk(int k_idx)       { return tv_is_junk(k_info[k_idx].tval); }
bool obj_kind_is_misc(int k_idx)       { return tv_is_misc(k_info[k_idx].tval); }
bool obj_kind_is_food(int k_idx)       { return tv_is_food(k_info[k_idx].tval); }
bool obj_kind_is_weapon_ammo(int k_idx){ return tv_is_weapon_ammo(k_info[k_idx].tval); }
bool obj_kind_is_bow_weapon_ammo(int k_idx){ return tv_is_bow_weapon_ammo(k_info[k_idx].tval); }

bool obj_kind_is_(int k_idx, int tv, int sv)
{
    obj_kind_ptr kind = &k_info[k_idx];
    if (kind->tval != tv) return FALSE;
    if (sv == SV_ANY) return TRUE;
    return kind->sval == sv;
}


/************************************************************************
 * Object
 ************************************************************************/
obj_ptr obj_alloc(void)
{
    obj_ptr obj = malloc(sizeof(obj_t));
    memset(obj, 0, sizeof(obj_t));
    return obj;
}

obj_ptr obj_copy(obj_ptr obj)
{
    obj_ptr copy = malloc(sizeof(obj_t));
    assert(obj);
    *copy = *obj;
    return copy;
}

obj_ptr obj_split(obj_ptr obj, int amt)
{
    obj_loc_t loc = {INV_TEMP};
    obj_ptr copy;

    assert(obj);
    assert(0 < amt && amt < obj->number);

    copy = obj_copy(obj);
    copy->loc = loc;
    copy->number = amt;
    obj->number -= amt;

    return copy;
}

void obj_clear_dun_info(obj_ptr obj)
{
    obj->marked &= (OM_WORN | OM_COUNTED | OM_EFFECT_COUNTED | OM_EGO_COUNTED | OM_ART_COUNTED);
    obj->next = NULL;
}

void obj_free(obj_ptr obj)
{
    if (obj)
    {
        object_wipe(obj);
        free(obj);
    }
}

term_char_t obj_display_char(obj_ptr obj)
{
    term_char_t tc;
    tc.c = object_char(obj);
    tc.a = object_attr(obj);
    return tc;
}

void obj_make_pile(obj_ptr obj)
{
    int          size = 1;
    object_kind *k_ptr = &k_info[obj->k_idx];

    if (obj_is_art(obj)) return;
    if (obj_is_ego(obj) && !obj_is_ammo(obj)) return;
    if (!k_ptr->stack_chance) return;
    if (randint1(100) > k_ptr->stack_chance) return;

    assert(k_ptr->stack_dice);
    assert(k_ptr->stack_sides);
    size = damroll(k_ptr->stack_dice, k_ptr->stack_sides);

    if (size <= 1) return;

    obj->number = size;
    if (!store_hack)
    {
        k_ptr->counts.generated += size - 1;
        if (obj->name2)
            e_info[obj->name2].counts.generated += size - 1;
    }
    else if (obj->discount)
    {
        obj->number -= (size * obj->discount / 100);
    }
}

static void _destroy(obj_ptr obj);

void obj_release(obj_ptr obj, int options)
{
    char name[MAX_NLEN];
    bool quiet = BOOL(options & OBJ_RELEASE_QUIET);
    bool delayed = BOOL(options & OBJ_RELEASE_DELAYED_MSG);

    if (!obj) return;
    if (!quiet)
        object_desc(name, obj, OD_COLOR_CODED);

    if ((obj->marked & OM_AUTODESTROY) && obj->number)
    {
        _destroy(obj);
        obj->number = 0;
    }

    if (options & OBJ_RELEASE_ENCHANT)
        gear_notice_enchant(obj);
    if (options & OBJ_RELEASE_ID)
        gear_notice_id(obj);

    switch (obj->loc.where)
    {
    case INV_FLOOR:
        if (!quiet)
            msg_format("You see %s.", name);
        if (obj->number <= 0)
        {
            assert(obj->loc.v.floor.dun_id == cave->id); /* XXX */
            delete_object_idx(obj->loc.v.floor.obj_id);
        }
        break;
    case INV_EQUIP:
        if (obj->number <= 0)
        {
            if (!quiet)
                msg_format("You are no longer wearing %s.", name);
            equip_remove(obj->loc.v.slot);
        }
        else if (!quiet)
            msg_format("You are wearing %s.", name);
        plr->window |= PW_EQUIP;
        break;
    case INV_PACK:
        if (!quiet && !delayed)
            msg_format("You have %s in your pack.", name);
        if (obj->number <= 0)
            pack_remove(obj->loc.v.slot);
        else if (delayed)
        {
            obj->marked |= OM_DELAYED_MSG;
            plr->notice |= PN_CARRY;
        }
        plr->window |= PW_INVEN;
        break;
    case INV_QUIVER:
        if (!quiet && !delayed)
            msg_format("You have %s in your quiver.", name);
        if (obj->number <= 0)
            quiver_remove(obj->loc.v.slot);
        else if (delayed)
        {
            obj->marked |= OM_DELAYED_MSG;
            plr->notice |= PN_CARRY;
        }
        plr->window |= PW_EQUIP; /* a Quiver [32 of 110] */
        break;
    case INV_TEMP:
        obj_free(obj);
        break;
    }
}

void gear_notice_id(obj_ptr obj)
{
    switch (obj->loc.where)
    {
    case INV_EQUIP:
        plr->update |= PU_BONUS; /* dis_to_h, dis_to_d, dis_to_ac, etc. */
        plr->window |= PW_EQUIP;
        break;
    case INV_PACK:
        plr->notice |= PN_OPTIMIZE_PACK;
        plr->window |= PW_INVEN;
        break;
    case INV_QUIVER:
        plr->notice |= PN_OPTIMIZE_QUIVER;
        plr->window |= PW_EQUIP; /* a Quiver [32 of 110] */
        break;
    }
}

void gear_notice_enchant(obj_ptr obj)
{
    switch (obj->loc.where)
    {
    case INV_EQUIP:
        plr->update |= PU_BONUS;
        plr->window |= PW_EQUIP;
        android_calc_exp();
        break;
    case INV_PACK:
        plr->notice |= PN_OPTIMIZE_PACK;
        plr->window |= PW_INVEN;
        break;
    case INV_QUIVER:
        plr->notice |= PN_OPTIMIZE_QUIVER;
        plr->window |= PW_EQUIP; /* a Quiver [32 of 110] */
        break;
    }
}

/************************************************************************
 * Predicates
 ***********************************************************************/
bool obj_can_sense1(obj_ptr obj)
{
    switch (obj->tval)
    {
    case TV_SHOT:
    case TV_ARROW:
    case TV_BOLT:
    case TV_BOW:
    case TV_QUIVER:
    case TV_DIGGING:
    case TV_HAFTED:
    case TV_POLEARM:
    case TV_SWORD:
    case TV_BOOTS:
    case TV_GLOVES:
    case TV_HELM:
    case TV_CROWN:
    case TV_SHIELD:
    case TV_CLOAK:
    case TV_SOFT_ARMOR:
    case TV_HARD_ARMOR:
    case TV_DRAG_ARMOR:
    case TV_CARD:
        return TRUE;
    }
    return FALSE;
}

bool obj_can_sense2(obj_ptr obj)
{
    switch (obj->tval)
    {
    case TV_RING:
    case TV_AMULET:
    case TV_LIGHT:
    case TV_FIGURINE:
    case TV_WAND:
    case TV_STAFF:
    case TV_ROD:
        return TRUE;
    }
    return FALSE;
}

bool obj_can_shoot(obj_ptr obj)
{
    if (!obj_is_ammo(obj)) return FALSE;
    if (!equip_find_obj(TV_BOW, SV_ANY)) return FALSE;
    return obj->tval == plr->shooter_info.tval_ammo;
}

bool obj_is_blessed(obj_ptr obj) { return obj_has_flag(obj, OF_BLESSED); }

bool obj_is_known(obj_ptr obj)
{
    obj_kind_ptr k;
    if (obj->ident & (IDENT_KNOWN | IDENT_STORE)) return TRUE;
    k = &k_info[obj->k_idx];
    if (k->easy_know && k->aware) return TRUE;
    return FALSE;
}

bool obj_is_readable_book(obj_ptr obj)
{
    if (!obj_is_spellbook(obj)) return FALSE;
    if (plr->pclass == CLASS_SORCERER)
    {
        return is_magic(tval2realm(obj->tval));
    }
    else if (plr->pclass == CLASS_RED_MAGE)
    {
        if (is_magic(tval2realm(obj->tval)))
            return ((obj->tval == TV_ARCANE_BOOK) || (obj->sval < 2));
    }
    else if (plr->pclass == CLASS_GRAY_MAGE)
    {
        return gray_mage_is_allowed_book(obj->tval, obj->sval);
    }
    else if (plr->pclass == CLASS_SKILLMASTER)
    {
        return skillmaster_is_allowed_book(obj->tval, obj->sval);
    }
    return (REALM1_BOOK == obj->tval || REALM2_BOOK == obj->tval);
}

bool obj_has_flag(obj_ptr obj, int which)
{
    u32b flags[OF_ARRAY_SIZE];
    obj_flags(obj, flags);
    return have_flag(flags, which);
}

bool obj_has_known_flag(obj_ptr obj, int which)
{
    u32b flags[OF_ARRAY_SIZE];
    obj_flags_known(obj, flags);
    return have_flag(flags, which);
}


bool obj_is_deleted(obj_ptr obj)    { return obj->loc.where == INV_JUNK; }
bool obj_exists(obj_ptr obj)        { return BOOL(obj); }
bool obj_is_weapon(obj_ptr obj)     { return tv_is_weapon(obj->tval); }
bool obj_is_shield(obj_ptr obj)     { return tv_is_shield(obj->tval); }
bool obj_is_bow(obj_ptr obj)        { return tv_is_bow(obj->tval); }
bool obj_is_quiver(obj_ptr obj)     { return tv_is_quiver(obj->tval); }
bool obj_is_ammo(obj_ptr obj)       { return tv_is_ammo(obj->tval); }
bool obj_is_ring(obj_ptr obj)       { return tv_is_ring(obj->tval); }
bool obj_is_amulet(obj_ptr obj)     { return tv_is_amulet(obj->tval); }
bool obj_is_light(obj_ptr obj)       { return tv_is_light(obj->tval); }
bool obj_is_body_armor(obj_ptr obj) { return tv_is_body_armor(obj->tval); }
bool obj_is_centaur_armor(obj_ptr obj) { return obj_is_body_armor(obj) && obj->ac > 8; }
bool obj_is_cloak(obj_ptr obj)      { return tv_is_cloak(obj->tval); }
bool obj_is_helmet(obj_ptr obj)     { return tv_is_helmet(obj->tval); }
bool obj_is_gloves(obj_ptr obj)     { return tv_is_gloves(obj->tval); }
bool obj_is_boots(obj_ptr obj)      { return tv_is_boots(obj->tval); }
bool obj_is_armor(obj_ptr obj)      { return tv_is_armor(obj->tval); }
bool obj_is_jewelry(obj_ptr obj)    { return tv_is_jewelry(obj->tval); }
bool obj_is_wearable(obj_ptr obj)   { return tv_is_wearable(obj->tval); }
bool obj_is_enchantable(obj_ptr obj){ return tv_is_enchantable(obj->tval) && !obj_has_flag(obj, OF_NO_ENCHANT); }
bool obj_is_equipment(obj_ptr obj)  { return tv_is_equipment(obj->tval); }
bool obj_is_device(obj_ptr obj)     { return tv_is_device(obj->tval); }
bool obj_is_spellbook(obj_ptr obj)  { return tv_is_spellbook(obj->tval); }
bool obj_is_potion(obj_ptr obj)     { return tv_is_potion(obj->tval); }
bool obj_is_scroll(obj_ptr obj)     { return tv_is_scroll(obj->tval); }
bool obj_is_junk(obj_ptr obj)       { return tv_is_junk(obj->tval); }
bool obj_is_misc(obj_ptr obj)       { return tv_is_misc(obj->tval); }
bool obj_is_food(obj_ptr obj)       { return tv_is_food(obj->tval); }
bool obj_is_weapon_ammo(obj_ptr obj){ return tv_is_weapon_ammo(obj->tval); }
bool obj_is_bow_weapon_ammo(obj_ptr obj){ return tv_is_bow_weapon_ammo(obj->tval); }

bool obj_is_gold(obj_ptr obj)    { return obj->tval == TV_GOLD; }
bool obj_is_not_gold(obj_ptr obj){ return obj->tval != TV_GOLD; }
bool obj_is_wand(obj_ptr obj)    { return obj->tval == TV_WAND; }
bool obj_is_rod(obj_ptr obj)     { return obj->tval == TV_ROD; }
bool obj_is_staff(obj_ptr obj)   { return obj->tval == TV_STAFF; }

bool obj_is_art(obj_ptr obj)     { return obj->art_id || obj->art_name; }
bool obj_is_std_art(obj_ptr obj) { return BOOL(obj->art_id); }
bool obj_is_specified_art(obj_ptr obj, cptr which)
{ 
    assert(arts_parse(which)); /* paranoia wrt my fat fingers ... */
    return sym_equals(obj->art_id, which);
}
bool obj_is_rand_art(obj_ptr obj){ return BOOL(obj->art_name); }
bool obj_is_ego(obj_ptr obj)     { return BOOL(obj->name2); }
bool obj_is_cursed(obj_ptr obj)  { return BOOL(obj->curse_flags); }
bool obj_is_broken(obj_ptr obj)  { return BOOL(obj->ident & IDENT_BROKEN); }

bool obj_is_found(obj_ptr obj)   { return BOOL(obj->marked & OM_FOUND); }
bool obj_is_inscribed(obj_ptr obj) { return BOOL(obj->inscription); }

bool obj_is_unknown(obj_ptr obj) { return !obj_is_known(obj); }
bool obj_is_(obj_ptr obj, int tv, int sv) { return obj && obj->tval == tv && obj->sval == sv; }

bool obj_is_dragon_armor(obj_ptr obj)
{
    if (obj->tval == TV_DRAG_ARMOR) return TRUE;
    return obj_is_(obj, TV_HELM, SV_DRAGON_HELM)
        || obj_is_(obj, TV_CLOAK, SV_DRAGON_CLOAK)
        || obj_is_(obj, TV_SHIELD, SV_DRAGON_SHIELD)
        || obj_is_(obj, TV_GLOVES, SV_SET_OF_DRAGON_GLOVES)
        || obj_is_(obj, TV_BOOTS, SV_PAIR_OF_DRAGON_GREAVE);
}

/************************************************************************
 * Corpses
 ***********************************************************************/
mon_race_ptr corpse_race(obj_ptr corpse)
{
    assert(corpse->tval == TV_CORPSE);
    assert(corpse->race_id);
    return mon_race_lookup(corpse->race_id);
}
bool corpse_race_is_char(obj_ptr corpse, char c)
{
    mon_race_ptr r = corpse_race(corpse);
    return mon_race_is_char(r, c);
}
bool corpse_race_is_char_ex(obj_ptr corpse, cptr s)
{
    mon_race_ptr r = corpse_race(corpse);
    return mon_race_is_char_ex(r, s);
}

/************************************************************************
 * Chests
 ***********************************************************************/
static bool _chest_is_can_of_toys(obj_ptr chest)
{
    assert(chest->tval == TV_CHEST);
    return chest->sval == SV_CHEST_KANDUME;
}
static vec_ptr _chest_contents(obj_ptr chest)
{
    vec_ptr v = vec_alloc((vec_free_f)obj_free);
    if (!chest_is_empty(chest))
    {
        int  ct_objects = 0, ct_gold = 0;
        int  i;
        int  lvl = cave->difficulty;
        u32b mode = AM_GOOD;

        /* figure out how much to drop */
        if (_chest_is_can_of_toys(chest))
        {
            ct_objects = 5;
            ct_gold = 0;
            mode |= AM_GREAT;
            lvl = chest->xtra3; /* no lugging to deeper levels for better loot */
        }
        else
        {      /* v~~~~~~~~~ You'll need to look a k_info.txt to understand this ... */
            int Y = chest->sval % SV_CHEST_MIN_LARGE; /* dice are XdY */

            /* Old: lvl = ABS(chest->pval) + 10;
             * i.e., 1dL+10. Finding an OL99 chest and getting L22 objects is just
             * plain insulting. Chests should be like ?Acquirement, only AM_GOOD rather
             * than AM_GREAT. */
            lvl = chest->xtra3;
            if (chest->sval < SV_CHEST_MIN_LARGE)
            {
                ct_gold = _2d(Y);
                ct_objects = _1d(Y);
            }
            else
            {
                ct_gold = _3d(Y);
                ct_objects = _2d(Y);
            }
        }

        opening_chest = TRUE; /* <==== This hack prevents getting chests from inside chests! */
        for (i = 0; i < ct_objects; i++)
        {
            obj_t forge = {0};
            if (!make_object(&forge, lvl, mode)) continue;
            vec_add(v, obj_copy(&forge));
        }
        for (i = 0; i < ct_gold; i++)
        {
            obj_t forge = {0};
            if (!make_gold(&forge, lvl)) continue;
            vec_add(v, obj_copy(&forge));
        }
        opening_chest = FALSE;

        chest->pval = 0; /* chest is empty now */
        obj_identify(chest);
    }
    return v;
}
static bool _obj_scatter(obj_ptr obj)
{
    rect_t r = rect_interior(cave->rect);
    int i;
    for (i = 0; i < 200; i++)
    {
        point_t p = rect_random_point(r);
        if (!dun_allow_drop_at(cave, p)) continue;
        dun_drop_near(cave, obj, p);
        return TRUE;
    }
    return FALSE;
}
bool obj_is_chest(obj_ptr obj) { return obj->tval == TV_CHEST; }
bool chest_has_trap(obj_ptr chest) { return chest->pval > 0 && chest_traps[chest->pval]; }
bool chest_has_known_trap(obj_ptr chest) { return obj_is_known(chest) && chest_has_trap(chest); }
bool chest_is_empty(obj_ptr chest) { return chest->pval == 0; }
bool chest_is_not_empty(obj_ptr chest) { return !chest_is_empty(chest); }
bool chest_is_locked(obj_ptr chest) { return chest->pval > 0; }
static void _chest_scatter(obj_ptr chest)
{
    vec_ptr v = _chest_contents(chest);
    int i;

    msg_print("The contents of the chest scatter all over the dungeon!");
    for (i = 0; i < vec_length(v); i++)
    {
        obj_ptr obj = vec_get(v, i);
        _obj_scatter(obj);
    }
    vec_free(v);
}
static point_t _chest_pos(obj_ptr chest)
{
    assert(chest->loc.where == INV_FLOOR); /* XXX you can't open it from your pack! */
    return point_create(chest->loc.v.floor.x, chest->loc.v.floor.y);
}
static void _chest_drop(obj_ptr chest)
{
    vec_ptr v = _chest_contents(chest);
    int i;
    point_t pos = _chest_pos(chest);

    for (i = 0; i < vec_length(v); i++)
    {
        obj_ptr obj = vec_get(v, i);
        dun_drop_near(cave, obj, pos);
    }
    vec_free(v);
}
static void _chest_summon(obj_ptr chest, int type)
{
    int lvl = chest->xtra3;
    point_t pos = _chest_pos(chest);
    u32b mode = PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET;

    summon_specific(who_create_null(), pos, lvl, type, mode);
}
static void _chest_traps(obj_ptr chest)
{
    int     i, trap;
    point_t pos = _chest_pos(chest);

    /* Ignore disarmed chests */
    if (chest->pval <= 0) return;

    /* Obtain the traps */
    trap = chest_traps[chest->pval];

    if (trap & CHEST_LOSE_STR)
    {
        msg_print("A small needle has pricked you!");
        take_hit(DAMAGE_NOESCAPE, _1d(4), "a poison needle");
        do_dec_stat(A_STR);
    }
    if (trap & CHEST_LOSE_CON)
    {
        msg_print("A small needle has pricked you!");
        take_hit(DAMAGE_NOESCAPE, _1d(4), "a poison needle");
        do_dec_stat(A_CON);
    }
    if (trap & CHEST_POISON)
    {
        msg_print("A puff of green gas surrounds you!");
        if (!res_save_default(GF_POIS))
            plr_tim_add(T_POISON, 10 + _1d(20));
    }
    if (trap & CHEST_PARALYZE)
    {
        msg_print("A puff of yellow gas surrounds you!");
        if (!free_act_save_p(0))
            plr_tim_add(T_PARALYZED, _1d(4));
    }
    if (trap & CHEST_SUMMON)
    {
        int num = 2 + _1d(3);
        msg_print("You are enveloped in a cloud of smoke!");
        for (i = 0; i < num; i++)
        {
            if (_1d(100) < cave->difficulty)
                activate_hi_summon(plr->pos, FALSE);
            else
                _chest_summon(chest, SUMMON_MONSTER);
        }
    }
    if (trap & CHEST_E_SUMMON)
    {
        msg_print("Elemental beings appear to protect their treasures!");
        for (i = 0; i < _1d(3) + 5; i++)
            _chest_summon(chest, SUMMON_ELEMENTAL);
    }
    if (trap & CHEST_BIRD_STORM)
    {
        msg_print("A storm of birds swirls around you!");
        for (i = 0; i < _1d(3) + 3; i++)
            dun_burst(cave, who_create_trap(pos), 7, pos, GF_FORCE, chest->pval/5);

        for (i = 0; i < _1d(5) + chest->pval/5; i++)
            _chest_summon(chest, SUMMON_BIRD);
    }
    if (trap & CHEST_H_SUMMON)
    {
        if (one_in_(4))
        {
            msg_print("Demons materialize in clouds of fire and brimstone!");
            for (i = 0; i < _1d(3) + 2; i++)
            {
                dun_burst(cave, who_create_trap(pos), 5, pos, GF_FIRE, 10);
                _chest_summon(chest, SUMMON_DEMON);
            }
        }
        else if (one_in_(3))
        {
            msg_print("Draconic forms loom out of the darkness!");
            for (i = 0; i < _1d(3) + 2; i++)
                _chest_summon(chest, SUMMON_DRAGON);
        }
        else if (one_in_(2))
        {
            msg_print("Creatures strange and twisted assault you!");
            for (i = 0; i < _1d(5) + 3; i++)
                _chest_summon(chest, SUMMON_HYBRID);
        }
        else
        {
            msg_print("Vortices coalesce and wreak destruction!");
            for (i = 0; i < _1d(3) + 2; i++)
                _chest_summon(chest, SUMMON_VORTEX);
        }
    }
    if (trap & CHEST_RUNES_OF_EVIL)
    {
        int nasty_tricks_count = 3 + _1d(3);
        msg_print("Hideous voices bid: 'Let the darkness have thee!'");

        /* This is gonna hurt... */
        for (; nasty_tricks_count > 0; nasty_tricks_count--)
        {
            /* ...but a high saving throw does help a little. */
            if (_1d(100 + 2*chest->pval) > plr->skills.sav)
            {
                if (one_in_(6)) take_hit(DAMAGE_NOESCAPE, _5d(20), "a trapped chest");
                else if (one_in_(5))
                {
                    if (!plr->no_cut)
                        plr_tim_add(T_CUT, 200);
                }
                else if (one_in_(4))
                {
                    if (!free_act_save_p(0))
                        plr_tim_add(T_PARALYZED, _1d(4));
                    else if (_1d(100) > res_pct(GF_STUN))
                        plr_tim_add(T_STUN, 10 + _1d(100));
                }
                else if (one_in_(3)) apply_disenchant(0);
                else if (one_in_(2))
                {
                    do_dec_stat(A_STR);
                    do_dec_stat(A_DEX);
                    do_dec_stat(A_CON);
                    do_dec_stat(A_INT);
                    do_dec_stat(A_WIS);
                    do_dec_stat(A_CHR);
                }
                else dun_burst(cave, who_create_trap(pos), 1, pos, GF_NETHER, 150);
            }
        }
    }
    if (trap & CHEST_ALARM)
    {
        msg_print("An alarm sounds!");
        aggravate_monsters(who_create_null());
    }
    if (trap & CHEST_EXPLODE)
    {
        msg_print("There is a sudden explosion!");
        msg_print("Everything inside the chest is destroyed!");

        chest->pval = 0;
        obj_identify(chest);
        take_hit(DAMAGE_ATTACK, _5d(8), "an exploding chest");

    }
    if (trap & CHEST_SCATTER)
        _chest_scatter(chest);
}
int chest_open(obj_ptr chest)
{
    int rc = ACTION_SUCCESS;
    if (chest_is_locked(chest))
        rc = chest_unlock(chest);
    if (rc == ACTION_SUCCESS)
    {
        if (chest_has_trap(chest))
            _chest_traps(chest);
        if (!chest_is_empty(chest)) /* CHEST_SCATTER */ 
            _chest_drop(chest);
    }
    return rc;
}
int chest_disarm(obj_ptr chest)
{
    int skill, chance;
    if (!obj_is_known(chest))
    {
        msg_print("I don't see any traps.");
        return ACTION_ABORT;
    }
    if (!chest_has_trap(chest))
    {
        msg_print("The chest is not trapped.");
        return ACTION_ABORT;
    }
    skill = plr_skill(plr->skills.dis);
    chance = skill - chest->pval;
    #if DEVELOPER
    if (0)
        msg_format("<color:D>Disarm Chest: %d%%</color>", chance);
    #endif
    if (_1d(100) <= chance)
    {
        msg_print("You have disarmed the chest.");
        gain_exp(chest->pval);
        chest->pval = -chest->pval;
        return ACTION_SUCCESS;
    }
    else if (chance > 5 && _1d(chance) > 5)
    {
        if (flush_failure) flush();
        msg_print("You failed to disarm the chest.");
        return ACTION_CONTINUE;
    }
    msg_print("You set off a trap!");
    _chest_traps(chest);
    return ACTION_FAIL;
}
int chest_unlock(obj_ptr chest)
{
    int rc = ACTION_SUCCESS;
    if (chest_is_locked(chest))
    {
        int skill = plr_skill(plr->skills.dis);
        int chance = MAX(2, skill - chest->pval);
        if (_1d(100) <= chance)
        {
            msg_print("You have picked the lock.");
            gain_exp(1);
        }
        else
        {
            msg_print("You failed to pick the lock.");
            rc = ACTION_CONTINUE;
        }
    }
    return rc;
}
/************************************************************************
 * Sorting
 ***********************************************************************/
void obj_clear_scratch(obj_ptr obj)
{
    if (obj) obj->scratch = 0;
}

static int _obj_cmp_type(obj_ptr obj)
{
    if (!obj_is_device(obj))
    {
        if (obj_is_std_art(obj)) return 3;
        else if (obj->art_name) return 2;
        else if (obj_is_ego(obj)) return 1;
    }
    return 0;
}

int obj_cmp(obj_ptr left, obj_ptr right)
{
    int left_type, right_type;
    /* Modified from object_sort_comp but the comparison is tri-valued
     * as is standard practice for compare functions. We also memoize
     * computation of obj_value for efficiency. */

    /* Empty slots sort to the end */
    if (!left && !right) return 0;
    if (!left && right) return 1;
    if (left && !right) return -1;
    if (left == right) return 0;

    assert(left && right);

    /* Hack -- readable books always come first (This fails for the Skillmaster) */
    if (left->tval == REALM1_BOOK && right->tval != REALM1_BOOK) return -1;
    if (left->tval != REALM1_BOOK && right->tval == REALM1_BOOK) return 1;

    if (left->tval == REALM2_BOOK && right->tval != REALM2_BOOK) return -1;
    if (left->tval != REALM2_BOOK && right->tval == REALM2_BOOK) return 1;

    /* Objects sort by decreasing type */
    if (left->tval < right->tval) return 1;
    if (left->tval > right->tval) return -1;

    /* Non-aware (flavored) items always come last (buggy test in shops) */
    if (left->loc.where != INV_SHOP && right->loc.where != INV_SHOP)
    {
        if (!object_is_aware(left) && object_is_aware(right)) return 1;
        if (object_is_aware(left) && !object_is_aware(right)) return -1;
    }

    /* Objects sort by increasing sval */
    if (left->sval < right->sval) return -1;
    if (left->sval > right->sval) return 1;

    /* Unidentified objects always come last */
    if (!obj_is_known(left) && obj_is_known(right)) return 1;
    if (obj_is_known(left) && !obj_is_known(right)) return -1;

    /* Fixed artifacts, random artifacts and ego items */
    left_type = _obj_cmp_type(left);
    right_type = _obj_cmp_type(right);
    if (left_type < right_type) return -1;
    if (left_type > right_type) return 1;

    switch (left->tval)
    {
    case TV_FIGURINE:
    case TV_STATUE:
    case TV_CORPSE:
    case TV_CAPTURE: {
        mon_race_ptr lrace, rrace;
        /* handle empty capture balls */
        if (!left->race_id && right->race_id) return -1;
        if (left->race_id && !right->race_id) return 1;
        if (!left->race_id && !right->race_id) return 0;

        assert(left->race_id && right->race_id);
        lrace = mon_race_lookup(left->race_id);
        rrace = mon_race_lookup(right->race_id);
        if (lrace->alloc.lvl < rrace->alloc.lvl) return -1;
        if (lrace->alloc.lvl > rrace->alloc.lvl) return 1;
        return strcmp(lrace->name, rrace->name); }
    case TV_SHOT:
    case TV_ARROW:
    case TV_BOLT:
        if (left->dd < right->dd) return -1;
        if (left->dd > right->dd) return 1;
        if (left->to_d < right->to_d) return -1;
        if (left->to_d > right->to_d) return 1;
        if (left->to_h < right->to_h) return -1;
        if (left->to_h > right->to_h) return 1;
        break;

    case TV_ROD:
    case TV_WAND:
    case TV_STAFF:
        if (left->activation.type < right->activation.type) return -1;
        if (left->activation.type > right->activation.type) return 1;
        if (device_level(left) < device_level(right)) return -1;
        if (device_level(left) > device_level(right)) return 1;
        break;

    case TV_LIGHT:
        if (left->xtra4 < right->xtra4) return 1;
        if (left->xtra4 > right->xtra4) return -1;
        break;
    }

    if (!left->scratch) left->scratch = obj_value(left);
    if (!right->scratch) right->scratch = obj_value(right);

    if (left->scratch < right->scratch) return 1;
    if (left->scratch > right->scratch) return -1;

    if (left->number < right->number) return 1;
    if (left->number > right->number) return -1;
    return 0;
}

/************************************************************************
 * Menus
 ***********************************************************************/
char obj_label(obj_ptr obj)
{
    cptr insc;
    if (!obj->inscription) return '\0';
    insc = quark_str(obj->inscription);

    for (insc = strchr(insc, '@'); insc; insc = strchr(insc, '@'))
    {
        insc++;
        /* @mc uses 'c' as a label only for the 'm' command */
        if (command_cmd && *insc == command_cmd)
        {
            insc++;
            if ( ('a' <= *insc && *insc <= 'z')
              || ('A' <= *insc && *insc <= 'Z')
              || ('0' <= *insc && *insc <= '9') )
            {
                return *insc;
            }
        }
        /* @3 uses '3' as a label for *any* command */
        else if ('0' <= *insc && *insc <= '9')
            return *insc;
    }
    return '\0';
}

bool obj_confirm_choice(obj_ptr obj)
{
    char name[MAX_NLEN], prompt[MAX_NLEN + 20];
    cptr insc, pos;
    int  ct = 0;

    if (!obj->inscription) return TRUE;
    if (!command_cmd) return TRUE;

    insc = quark_str(obj->inscription);
    /* !sdk = !s!d!k */
    for (pos = strchr(insc, '!');
            pos && *pos;
            pos = strchr(pos + 1, '!'))
    {
        for (;;)
        {
            pos++;
            if (!*pos) return TRUE;
            else if (*pos == command_cmd || *pos == '*')
            {
                if (!ct++)
                {
                    object_desc(name, obj, OD_COLOR_CODED);
                    sprintf(prompt, "Really choose %s? ", name);
                }
                if (!get_check(prompt)) return FALSE;
            }
            else if (!isalpha(*pos))
            {
                if (*pos == '!') pos--; /* !k!q */
                break;
            }
        }
    }
    return TRUE;
}

/************************************************************************
 * Stacking
 ***********************************************************************/
int obj_stack_max(obj_ptr obj)
{
    obj_kind_ptr kind = &k_info[obj->k_idx];
    if (kind->stack) return kind->stack;
    return MAX(1, tv_lookup(obj->tval)->stack);
}
bool obj_can_combine(obj_ptr dest, obj_ptr obj, int loc)
{
    int  i;

    if (dest == obj) return FALSE;
    if (dest->k_idx != obj->k_idx) return FALSE; /* i.e. same tval/sval */
    if (obj_is_art(dest) || obj_is_art(obj)) return FALSE;
    if (obj_stack_max(dest) <= 1) return FALSE;
    if (obj->marked & OM_RESERVED) return FALSE;
    if (dest->marked & OM_RESERVED) return FALSE;

    switch (dest->tval)
    {
    case TV_FIGURINE:
    case TV_CORPSE:
        if (dest->race_id != obj->race_id) return FALSE;
        break;

    case TV_FOOD:
    case TV_POTION:
    case TV_SCROLL:
        break;

    /* Equipment */
    case TV_BOW:
    case TV_DIGGING:
    case TV_HAFTED:
    case TV_POLEARM:
    case TV_SWORD:
    case TV_BOOTS:
    case TV_GLOVES:
    case TV_HELM:
    case TV_CROWN:
    case TV_SHIELD:
    case TV_CLOAK:
    case TV_SOFT_ARMOR:
    case TV_HARD_ARMOR:
    case TV_DRAG_ARMOR:
    case TV_RING:
    case TV_AMULET:
    case TV_LIGHT:
    case TV_WHISTLE:
        /* Require full knowledge of both items. Ammo skips this check
         * so that you can shoot unidentifed stacks of arrows and have
         * them recombine later. */
        if (loc != INV_SHOP)
        {
            if (!obj_is_known(dest) || !obj_is_known(obj)) return FALSE;
        }
        /* Fall through */
    case TV_BOLT:
    case TV_ARROW:
    case TV_SHOT:
        if (loc != INV_SHOP)
        {
            if (obj_is_known(dest) != obj_is_known(obj)) return FALSE;
            if (dest->feeling != obj->feeling) return FALSE;
        }

        /* Require identical bonuses */
        if (dest->to_h != obj->to_h) return FALSE;
        if (dest->to_d != obj->to_d) return FALSE;
        if (dest->to_a != obj->to_a) return FALSE;
        if (dest->pval != obj->pval) return FALSE;

        /* Require identical ego types (Artifacts were precluded above) */
        if (dest->name2 != obj->name2) return FALSE;

        /* Require identical added essence  */
        if (dest->xtra3 != obj->xtra3) return FALSE;
        if (dest->xtra4 != obj->xtra4) return FALSE;

        /* Hack -- Never stack "powerful" items (Q: What does this mean?) */
        if (dest->xtra1 || obj->xtra1) return FALSE;

        /* Hack -- Never stack recharging items */
        if (dest->timeout || obj->timeout) return FALSE;

        /* Require identical "values" */
        if (dest->ac != obj->ac) return FALSE;
        if (dest->dd != obj->dd) return FALSE;
        if (dest->ds != obj->ds) return FALSE;

        break;

    default:
        /* Require knowledge */
        if (!obj_is_known(dest) || !obj_is_known(obj)) return FALSE;
    }

    /* Hack -- Identical art_flags! */
    for (i = 0; i < OF_ARRAY_SIZE; i++)
        if (dest->flags[i] != obj->flags[i]) return FALSE;

    /* Hack -- Require identical "cursed" status */
    if (dest->curse_flags != obj->curse_flags) return FALSE;

    /* Require identical activations */
    if ( dest->activation.type != obj->activation.type
      || dest->activation.cost != obj->activation.cost
      || dest->activation.power != obj->activation.power
      || dest->activation.difficulty != obj->activation.difficulty
      || dest->activation.extra != obj->activation.extra )
    {
        return FALSE;
    }

    /* Hack -- Require identical "broken" status */
    if ((dest->ident & IDENT_BROKEN) != (obj->ident & IDENT_BROKEN)) return FALSE;

    /* Shops always merge inscriptions, but never discounts. For the
     * player, merging of inscriptions and discounts is controlled
     * by options (stack_force_*) */
    if (loc == INV_SHOP)
    {
        if (dest->discount != obj->discount) return FALSE;
    }
    else
    {
        /* If both objects are inscribed, then inscriptions must match. */
        if (dest->inscription && obj->inscription && dest->inscription != obj->inscription)
            return FALSE;

        if (!stack_force_notes && dest->inscription != obj->inscription) return FALSE;
        if (!stack_force_costs && dest->discount != obj->discount) return FALSE;
    }

    return TRUE;
}

/* combine obj into dest up to the max stack size.
 * decrease obj->number by the amount combined and 
 * return the amount combined. */
int obj_combine(obj_ptr dest, obj_ptr obj, int loc)
{
    int amt;
    int stack_max;

    assert(dest && obj);
    stack_max = obj_stack_max(obj);

    if (dest->number >= stack_max) return 0; /* don't break savefiles when tweaking stack maximums */
    if (!obj_can_combine(dest, obj, loc)) return 0;

    if (dest->number + obj->number > stack_max)
        amt = stack_max - dest->number;
    else
        amt = obj->number;

    dest->number += amt;
    obj->number -= amt;

    if (loc != INV_SHOP)
    {
        if (obj_is_known(obj)) obj_identify(dest);

        /* Hack -- clear "storebought" if only one has it */
        if ( ((dest->ident & IDENT_STORE) || (obj->ident & IDENT_STORE))
          && !((dest->ident & IDENT_STORE) && (obj->ident & IDENT_STORE)) )
        {
            if (obj->ident & IDENT_STORE) obj->ident &= ~IDENT_STORE;
            if (dest->ident & IDENT_STORE) dest->ident &= ~IDENT_STORE;
        }

        /* Hack -- blend "inscriptions" */
        if (obj->inscription && !dest->inscription) dest->inscription = obj->inscription;

        /* Hack -- blend "feelings" */
        if (obj->feeling) dest->feeling = obj->feeling;
        if (obj->marked & OM_DELAYED_MSG) dest->marked |= OM_DELAYED_MSG;

        /* Hack -- could average discounts XXX XXX XXX */
        /* Hack -- save largest discount XXX XXX XXX */
        if (dest->discount < obj->discount) dest->discount = obj->discount;
    }
    return amt;
}

void obj_delayed_describe(obj_ptr obj)
{
    if (obj->marked & OM_DELAYED_MSG)
    {
        str_ptr msg = str_alloc();
        char    name[MAX_NLEN];
        bool    show_slot = FALSE;

        object_desc(name, obj, OD_COLOR_CODED);
        if (obj->loc.where == INV_EQUIP) /* paranoia */
            str_append_s(msg, "You are wearing");
        else
            str_append_s(msg, "You have");
        str_printf(msg, " %s", name);
        if (obj->loc.where == INV_QUIVER)
            str_append_s(msg, " in your quiver");

        switch (obj->loc.where)
        {
        case INV_QUIVER:
        case INV_PACK:
            show_slot = use_pack_slots;
            break;
        case INV_EQUIP:
            show_slot = TRUE;
            break;
        }
        if (show_slot)
            str_printf(msg, " (%c)", slot_label(obj->loc.v.slot));
        str_append_c(msg, '.');
        msg_print(str_buffer(msg));
        str_free(msg);

        obj->marked &= ~OM_DELAYED_MSG;
    }
}

/************************************************************************
 * Commands:
 * For Inspect and Inscribe, it seems useful to keep the obj_prompt up
 * to allow multiple operations. There is no energy cost for these commands.
 ***********************************************************************/
static int _inspector(obj_prompt_context_ptr context, int cmd)
{
    obj_prompt_tab_ptr tab = vec_get(context->tabs, context->tab);
    slot_t             slot = inv_label_slot(tab->inv, cmd);
    if (slot)
    {
        obj_ptr obj = inv_obj(tab->inv, slot);
        if (!obj) return OP_CMD_SKIPPED; /* gear_ui(INV_EQUIP) */
        doc_clear(context->doc);
        if (object_is_flavor(obj) && !obj_is_known(obj))
        {
            char name[MAX_NLEN];
            object_desc(name, obj, OD_COLOR_CODED);
            doc_insert(context->doc, name);
            doc_insert(context->doc, "\n\nYou have no special knowledge about this item.\n");
        }
        else
            obj_display_doc(obj, context->doc);
        doc_insert(context->doc, "<color:B>[Press <color:y>Any Key</color> to Continue]</color>\n\n");
        Term_load();
        doc_sync_menu(context->doc);
        cmd = inkey();
        return OP_CMD_HANDLED;
    }
    return OP_CMD_SKIPPED;
}

void obj_inspect_ui(void)
{
    obj_prompt_t prompt = {0};

    prompt.prompt = "Examine which item <color:w>(<color:keypress>Esc</color> to exit)</color>?";
    prompt.error = "You have nothing to examine.";
    prompt.filter = obj_exists;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_QUIVER;
    prompt.where[3] = INV_FLOOR;
    prompt.cmd_handler = _inspector;

    obj_prompt(&prompt);

    /* The '-' key autoselects a single floor object */
    if (prompt.obj)
        obj_display(prompt.obj);
}

void gear_ui(int which)
{
    obj_prompt_t prompt = {0};
    int          wgt = plr_total_weight();
    int          pct = wgt * 100 / weight_limit();
    str_ptr      s;

    s = str_alloc_format(
        "<color:w>Carrying %d.%d pounds (<color:%c>%d%%</color> capacity).</color>\n\n"
        "Examine which item <color:w>(<color:keypress>Esc</color> to exit)</color>?",
         wgt / 10, wgt % 10, pct > 100 ? 'r' : 'G', pct);
    prompt.prompt = str_buffer(s);
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_QUIVER;
    prompt.top_loc = which;

    prompt.cmd_handler = _inspector;

    obj_prompt(&prompt);
    str_free(s);
}

static int _inscriber(obj_prompt_context_ptr context, int cmd)
{
    obj_prompt_tab_ptr tab = vec_get(context->tabs, context->tab);
    slot_t             slot = inv_label_slot(tab->inv, cmd);
    if (slot)
    {
        obj_ptr obj = inv_obj(tab->inv, slot);
        char    name[MAX_NLEN];
        char    insc[80];

        object_desc(name, obj, OD_OMIT_INSCRIPTION | OD_COLOR_CODED);
        if (obj->inscription)
            strcpy(insc, quark_str(obj->inscription));
        else
            strcpy(insc, "");

        doc_clear(context->doc);
        doc_printf(context->doc, "Inscribing %s.\n", name);
        doc_printf(context->doc, "Inscription: ");
        Term_load();
        doc_sync_menu(context->doc);
        if (askfor(insc, 80))
            obj->inscription = quark_add(insc);
        return OP_CMD_HANDLED;
    }
    return OP_CMD_SKIPPED;
}

void obj_inscribe_ui(void)
{
    obj_prompt_t prompt = {0};

    prompt.prompt = "Inscribe which item <color:w>(<color:keypress>Esc</color> to exit)</color>?";
    prompt.error = "You have nothing to inscribe.";
    prompt.filter = obj_exists;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_QUIVER;
    prompt.where[3] = INV_FLOOR;
    prompt.cmd_handler = _inscriber;

    obj_prompt(&prompt);

    plr->notice |= PN_OPTIMIZE_PACK | PN_OPTIMIZE_QUIVER;
    plr->window |= PW_INVEN | PW_EQUIP;
}

static int _uninscriber(obj_prompt_context_ptr context, int cmd)
{
    obj_prompt_tab_ptr tab = vec_get(context->tabs, context->tab);
    slot_t             slot = inv_label_slot(tab->inv, cmd);
    if (slot)
    {
        obj_ptr obj = inv_obj(tab->inv, slot);
        obj->inscription = 0;
        return OP_CMD_HANDLED;
    }
    return OP_CMD_SKIPPED;
}

void obj_uninscribe_ui(void)
{
    obj_prompt_t prompt = {0};

    prompt.prompt = "Remove inscription from which item <color:w>(<color:keypress>Esc</color> to exit)</color>?";
    prompt.error = "You have nothing to uninscribe.";
    prompt.filter = obj_is_inscribed;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_QUIVER;
    prompt.where[3] = INV_FLOOR;
    prompt.cmd_handler = _uninscriber;

    obj_prompt(&prompt);

    plr->notice |= PN_OPTIMIZE_PACK | PN_OPTIMIZE_QUIVER;
    plr->window |= PW_INVEN | PW_EQUIP;
}

static void _drop(obj_ptr obj)
{
    char name[MAX_NLEN];
    object_desc(name, obj, OD_COLOR_CODED);
    msg_format("You drop %s.", name);
    obj->marked |= OM_NO_MSG; /* suppress the "rolls beneath your feet" msg */
    drop_near(obj, plr->pos, -1);
    obj->marked &= ~OM_NO_MSG;
    plr->update |= PU_BONUS; /* Weight changed */
    if (obj->loc.where == INV_PACK)
        plr->window |= PW_INVEN;
}

void obj_drop(obj_ptr obj, int amt)
{
    assert(obj);
    assert(amt <= obj->number);

    if (!amt) return;

    if (amt < obj->number)
    {
        obj_t copy = *obj;
        copy.number = amt;
        obj->number -= amt;

        obj->marked |= OM_DELAYED_MSG;
        plr->notice |= PN_CARRY;
        if (obj->loc.where == INV_PACK)
            plr->notice |= PN_OPTIMIZE_PACK;
        else if (obj->loc.where == INV_QUIVER)
            plr->notice |= PN_OPTIMIZE_QUIVER;

        copy.marked &= ~OM_WORN;
        _drop(&copy);
    }
    else
    {
        obj->marked &= ~OM_WORN;
        _drop(obj);
        obj->number = 0;
        obj_release(obj, OBJ_RELEASE_QUIET);
    }
}

static void _drop_at(obj_ptr obj, int x, int y, int break_chance)
{
    drop_near(obj, point_create(x, y), break_chance);
}

void obj_drop_at(obj_ptr obj, int amt, int x, int y, int break_chance)
{
    assert(obj);
    assert(amt <= obj->number);

    if (!amt) return;

    if (amt < obj->number)
    {
        obj_t copy = *obj;
        copy.number = amt;
        obj->number -= amt;
        copy.marked &= ~OM_WORN;
        _drop_at(&copy, x, y, break_chance);
    }
    else
    {
        obj->marked &= ~OM_WORN;
        _drop_at(obj, x, y, break_chance);
        obj->number = 0;
        obj_release(obj, OBJ_RELEASE_QUIET);
    }
}

static bool _can_destroy(obj_ptr obj)
{
    if (obj->loc.where == INV_EQUIP && obj->rune != RUNE_SACRIFICE)
        return FALSE;
    return TRUE;
}

void obj_destroy_ui(void)
{
    obj_prompt_t prompt = {0};
    char         name[MAX_NLEN];
    int          pos = 0;
    bool         force = command_arg > 0; /* 033kx to destroy 33 in slot (x) */
    int          amt = 1;

    if (plr->special_defense & KATA_MUSOU)
        set_action(ACTION_NONE);

    /* Prompt for an object */
    prompt.prompt = "Destroy which item?";
    prompt.error = "You have nothing to destroy.";
    prompt.filter = _can_destroy;
    prompt.where[pos++] = INV_PACK;
    if (plr->pclass == CLASS_RUNE_KNIGHT)
        prompt.where[pos++] = INV_EQUIP;
    prompt.where[pos++] = INV_QUIVER;
    prompt.where[pos++] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return;

    /* Verify unless quantity given beforehand */
    if (!force && (confirm_destroy || (obj_value(prompt.obj) > 0)))
    {
        char ch;
        int  options = OD_COLOR_CODED;
        char msg[MAX_NLEN + 100];

        if (prompt.obj->number > 1)
            options |= OD_OMIT_PREFIX;
        object_desc(name, prompt.obj, options);
        sprintf(msg, "Really destroy %s? <color:y>[y/n/Auto]</color>", name);

        ch = msg_prompt(msg, "nyA", PROMPT_DEFAULT);
        if (ch == 'n') return;
        if (ch == 'A')
        {
            if (autopick_autoregister(prompt.obj))
            {
                autopick_alter_obj(prompt.obj, TRUE); /* destroyed! */
                obj_release(prompt.obj, OBJ_RELEASE_QUIET);
            }
            return;
        }
    }

    /* Get a quantity. Note: get_quantity will return command_arg if set. */
    if (prompt.obj->number > 1)
    {
        amt = get_quantity(NULL, prompt.obj->number);
        if (amt <= 0) return;
    }

    /* Artifacts cannot be destroyed */
    if (!can_player_destroy_object(prompt.obj)) /* side effect: obj->sense = FEEL_SPECIAL */
    {
        object_desc(name, prompt.obj, OD_COLOR_CODED);
        msg_format("You cannot destroy %s.", name);
        return;
    }
    obj_destroy(prompt.obj, amt);
}

static void _destroy(obj_ptr obj)
{
    stats_on_p_destroy(obj, obj->number);
    if (!plr_hook_destroy_object(obj))
    {
        if (obj->loc.where)
            msg_print("Destroyed.");
        else  /* Destroying part of a pile */
        {
            char name[MAX_NLEN];
            object_desc(name, obj, OD_COLOR_CODED);
            msg_format("You destroy %s.", name);
        }
    }

    sound(SOUND_DESTITEM);

    if (high_level_book(obj))
        spellbook_destroy(obj);
    if (obj->to_a || obj->to_h || obj->to_d)
        virtue_add(VIRTUE_ENCHANTMENT, -1);

    if (obj_value_real(obj) > 30000)
        virtue_add(VIRTUE_SACRIFICE, 2);

    else if (obj_value_real(obj) > 10000)
        virtue_add(VIRTUE_SACRIFICE, 1);

    if (obj->to_a != 0 || obj->to_d != 0 || obj->to_h != 0)
        virtue_add(VIRTUE_HARMONY, 1);
}

void obj_destroy(obj_ptr obj, int amt)
{
    assert(obj);
    assert(amt <= obj->number);

    if (!amt) return;

    energy_use = 100;
    if (amt < obj->number)
    {
        obj_t copy = *obj;
        copy.number = amt;
        obj->number -= amt;
        _destroy(&copy);
        if (obj->loc.where == INV_PACK)
            plr->window |= PW_INVEN;
    }
    else
    {
        _destroy(obj);
        obj->number = 0;
        obj_release(obj, OBJ_RELEASE_QUIET);
    }
}

void obj_describe_charges(obj_ptr obj)
{
    int charges;

    if (!obj_is_device(obj)) return;
    if (!obj_is_known(obj)) return;
    if (!obj->activation.cost) return; /* Just checking ... */

    charges = device_sp(obj) / obj->activation.cost;

    if (charges == 1)
    {
        msg_format("%s 1 charge remaining.",
            obj->loc.where == INV_FLOOR ? "There is" : "You have");
    }
    else
    {
        msg_format("%s %d charges remaining.",
            obj->loc.where == INV_FLOOR ? "There are" : "You have",
            charges);
    }
}

/************************************************************************
 * Savefiles
 ***********************************************************************/

enum object_save_fields_e {
    OBJ_SAVE_DONE = 0,
    OBJ_SAVE_PVAL,
    OBJ_SAVE_DISCOUNT,
    OBJ_SAVE_NUMBER,
    OBJ_SAVE_ART_ID,
    OBJ_SAVE_REPLACEMENT_ART_ID,
    OBJ_SAVE_RACE_ID,
    OBJ_SAVE_NAME2,
    OBJ_SAVE_ART_NAME,
    OBJ_SAVE_TIMEOUT,
    OBJ_SAVE_COMBAT,
    OBJ_SAVE_ARMOR,
    OBJ_SAVE_DAMAGE_DICE,
    OBJ_SAVE_IDENT,
    OBJ_SAVE_MARKED_BYTE,
    OBJ_SAVE_FEELING,
    OBJ_SAVE_INSCRIPTION,
    OBJ_SAVE_ART_FLAGS_0,
    OBJ_SAVE_ART_FLAGS_1,
    OBJ_SAVE_ART_FLAGS_2,
    OBJ_SAVE_ART_FLAGS_3,
    OBJ_SAVE_ART_FLAGS_4,
    OBJ_SAVE_ART_FLAGS_5,
    OBJ_SAVE_ART_FLAGS_6,
    OBJ_SAVE_ART_FLAGS_7,
    OBJ_SAVE_ART_FLAGS_8,
    OBJ_SAVE_ART_FLAGS_9,
    OBJ_SAVE_CURSE_FLAGS,
    OBJ_SAVE_RUNE_FLAGS,
    OBJ_SAVE_XTRA1,
    OBJ_SAVE_XTRA2,
    OBJ_SAVE_XTRA3,
    OBJ_SAVE_XTRA4,
    OBJ_SAVE_XTRA5_OLD,
    OBJ_SAVE_ACTIVATION,
    OBJ_SAVE_MULT,
    OBJ_SAVE_MARKED,
    OBJ_SAVE_XTRA5,
    OBJ_SAVE_KNOWN_FLAGS_0,
    OBJ_SAVE_KNOWN_FLAGS_1,
    OBJ_SAVE_KNOWN_FLAGS_2,
    OBJ_SAVE_KNOWN_FLAGS_3,
    OBJ_SAVE_KNOWN_FLAGS_4,
    OBJ_SAVE_KNOWN_FLAGS_5,
    OBJ_SAVE_KNOWN_FLAGS_6,
    OBJ_SAVE_KNOWN_FLAGS_7,
    OBJ_SAVE_KNOWN_FLAGS_8,
    OBJ_SAVE_KNOWN_FLAGS_9,
    OBJ_SAVE_KNOWN_CURSE_FLAGS,
    OBJ_SAVE_LEVEL,
    OBJ_SAVE_KNOWN_XTRA,
};

void obj_load(obj_ptr obj, savefile_ptr file)
{
    object_kind *k_ptr;
    char         buf[128];

    obj->k_idx = savefile_read_s16b(file);
    k_ptr = &k_info[obj->k_idx];
    obj->tval = k_ptr->tval;
    obj->sval = k_ptr->sval;

    obj->loc.where = savefile_read_byte(file);
    switch (obj->loc.where)
    {
    case INV_PACK:
    case INV_QUIVER:
    case INV_EQUIP:
    case INV_SHOP:
    case INV_HOME:
    case INV_MUSEUM:
        obj->loc.v.slot = savefile_read_s32b(file);
        break;
    case INV_FLOOR:
        obj->loc.v.floor.dun_id = savefile_read_u16b(file);
        obj->loc.v.floor.obj_id = savefile_read_u16b(file);
        obj->loc.v.floor.x = savefile_read_s16b(file);
        obj->loc.v.floor.y = savefile_read_s16b(file);
        break;
    case INV_MON_PACK:
        obj->loc.v.mon_pack.dun_id = savefile_read_u16b(file);
        obj->loc.v.mon_pack.obj_id = savefile_read_u16b(file);
        obj->loc.v.mon_pack.mon_id = savefile_read_u32b(file);
        break;
    }

    obj->weight = savefile_read_s16b(file);
    obj->number = 1;

    for (;;)
    {
        byte code = savefile_read_byte(file);
        if (code == OBJ_SAVE_DONE)
            break;

        switch (code)
        {
        case OBJ_SAVE_PVAL:
            obj->pval = savefile_read_s16b(file);
            break;
        case OBJ_SAVE_DISCOUNT:
            obj->discount = savefile_read_byte(file);
            break;
        case OBJ_SAVE_NUMBER:
            obj->number = savefile_read_byte(file);
            break;
        case OBJ_SAVE_ART_ID:
            obj->art_id = savefile_read_sym(file);
            break;
        case OBJ_SAVE_NAME2:
            obj->name2 = savefile_read_s16b(file);
            break;
        case OBJ_SAVE_REPLACEMENT_ART_ID:
            obj->replacement_art_id = savefile_read_sym(file);
            break;
        case OBJ_SAVE_RACE_ID:
            obj->race_id = savefile_read_sym(file);
            break;
        case OBJ_SAVE_TIMEOUT:
            obj->timeout = savefile_read_s16b(file);
            break;
        case OBJ_SAVE_COMBAT:
            obj->to_h = savefile_read_s16b(file);
            obj->to_d = savefile_read_s16b(file);
            break;
        case OBJ_SAVE_ARMOR:
            obj->to_a = savefile_read_s16b(file);
            obj->ac = savefile_read_s16b(file);
            break;
        case OBJ_SAVE_DAMAGE_DICE:
            obj->dd = savefile_read_byte(file);
            obj->ds = savefile_read_byte(file);
            break;
        case OBJ_SAVE_MULT:
            obj->mult = savefile_read_s16b(file);
            break;
        case OBJ_SAVE_IDENT:
            obj->ident = savefile_read_byte(file);
            break;
        case OBJ_SAVE_MARKED_BYTE:
            obj->marked = savefile_read_byte(file);
            break;
        case OBJ_SAVE_MARKED:
            obj->marked = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_ART_FLAGS_0:
            obj->flags[0] = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_ART_FLAGS_1:
            obj->flags[1] = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_ART_FLAGS_2:
            obj->flags[2] = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_ART_FLAGS_3:
            obj->flags[3] = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_ART_FLAGS_4:
            obj->flags[4] = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_ART_FLAGS_5:
            obj->flags[5] = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_ART_FLAGS_6:
            obj->flags[6] = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_ART_FLAGS_7:
            obj->flags[7] = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_CURSE_FLAGS:
            obj->curse_flags = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_KNOWN_FLAGS_0:
            obj->known_flags[0] = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_KNOWN_FLAGS_1:
            obj->known_flags[1] = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_KNOWN_FLAGS_2:
            obj->known_flags[2] = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_KNOWN_FLAGS_3:
            obj->known_flags[3] = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_KNOWN_FLAGS_4:
            obj->known_flags[4] = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_KNOWN_FLAGS_5:
            obj->known_flags[5] = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_KNOWN_FLAGS_6:
            obj->known_flags[6] = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_KNOWN_FLAGS_7:
            obj->known_flags[7] = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_KNOWN_CURSE_FLAGS:
            obj->known_curse_flags = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_KNOWN_XTRA:
            obj->known_xtra = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_RUNE_FLAGS:
            obj->rune = savefile_read_u32b(file);
            break;
        case OBJ_SAVE_XTRA1:
            obj->xtra1 = savefile_read_byte(file);
            break;
        case OBJ_SAVE_XTRA2:
            obj->xtra2 = savefile_read_byte(file);
            break;
        case OBJ_SAVE_XTRA3:
            obj->xtra3 = savefile_read_byte(file);
            break;
        case OBJ_SAVE_XTRA4:
            obj->xtra4 = savefile_read_s16b(file);
            break;
        case OBJ_SAVE_XTRA5_OLD:
            obj->xtra5 = savefile_read_s16b(file);
            break;
        case OBJ_SAVE_XTRA5:
            obj->xtra5 = savefile_read_s32b(file);
            break;
        case OBJ_SAVE_FEELING:
            obj->feeling = savefile_read_byte(file);
            break;
        case OBJ_SAVE_INSCRIPTION:
            savefile_read_cptr(file, buf, sizeof(buf));
            obj->inscription = quark_add(buf);
            break;
        case OBJ_SAVE_ART_NAME:
            savefile_read_cptr(file, buf, sizeof(buf));
            obj->art_name = quark_add(buf);
            break;
        case OBJ_SAVE_ACTIVATION:
            obj->activation.type = savefile_read_s16b(file);
            obj->activation.power = savefile_read_byte(file);
            obj->activation.difficulty = savefile_read_byte(file);
            obj->activation.cost = savefile_read_s16b(file);
            obj->activation.extra = savefile_read_s16b(file);
            break;
        case OBJ_SAVE_LEVEL:
            obj->level = savefile_read_s16b(file);
            break;
        /* default:
            TODO: Report an error back to the load routine!!*/
        }
    }
    if (obj_is_device(obj))
        add_flag(obj->flags, OF_ACTIVATE);
}

void obj_save(obj_ptr obj, savefile_ptr file)
{
    savefile_write_s16b(file, obj->k_idx);
    savefile_write_byte(file, obj->loc.where);
    switch (obj->loc.where)
    {
    case INV_PACK:
    case INV_QUIVER:
    case INV_EQUIP:
    case INV_SHOP:
    case INV_HOME:
    case INV_MUSEUM:
        savefile_write_s32b(file, obj->loc.v.slot);
        break;
    case INV_FLOOR:
        savefile_write_u16b(file, obj->loc.v.floor.dun_id);
        savefile_write_u16b(file, obj->loc.v.floor.obj_id);
        savefile_write_s16b(file, obj->loc.v.floor.x);
        savefile_write_s16b(file, obj->loc.v.floor.y);
        break;
    case INV_MON_PACK:
        savefile_write_u16b(file, obj->loc.v.mon_pack.dun_id);
        savefile_write_u16b(file, obj->loc.v.mon_pack.obj_id);
        savefile_write_u32b(file, obj->loc.v.mon_pack.mon_id);
        break;
    }
    savefile_write_s16b(file, obj->weight);
    if (obj->pval)
    {
        savefile_write_byte(file, OBJ_SAVE_PVAL);
        savefile_write_s16b(file, obj->pval);
    }
    if (obj->discount)
    {
        savefile_write_byte(file, OBJ_SAVE_DISCOUNT);
        savefile_write_byte(file, obj->discount);
    }
    if (obj->number != 1)
    {
        savefile_write_byte(file, OBJ_SAVE_NUMBER);
        savefile_write_byte(file, obj->number);
    }
    if (obj->art_id)
    {
        savefile_write_byte(file, OBJ_SAVE_ART_ID);
        savefile_write_sym(file, obj->art_id);
    }
    if (obj->replacement_art_id)
    {
        savefile_write_byte(file, OBJ_SAVE_REPLACEMENT_ART_ID);
        savefile_write_sym(file, obj->replacement_art_id);
    }
    if (obj->race_id)
    {
        savefile_write_byte(file, OBJ_SAVE_RACE_ID);
        savefile_write_sym(file, obj->race_id);
    }
    if (obj->name2)
    {
        savefile_write_byte(file, OBJ_SAVE_NAME2);
        savefile_write_s16b(file, obj->name2);
    }
    if (obj->timeout)
    {
        savefile_write_byte(file, OBJ_SAVE_TIMEOUT);
        savefile_write_s16b(file, obj->timeout);
    }
    if (obj->to_h || obj->to_d)
    {
        savefile_write_byte(file, OBJ_SAVE_COMBAT);
        savefile_write_s16b(file, obj->to_h);
        savefile_write_s16b(file, obj->to_d);
    }
    if (obj->to_a || obj->ac)
    {
        savefile_write_byte(file, OBJ_SAVE_ARMOR);
        savefile_write_s16b(file, obj->to_a);
        savefile_write_s16b(file, obj->ac);
    }
    if (obj->dd || obj->ds)
    {
        savefile_write_byte(file, OBJ_SAVE_DAMAGE_DICE);
        savefile_write_byte(file, obj->dd);
        savefile_write_byte(file, obj->ds);
    }
    if (obj->mult)
    {
        savefile_write_byte(file, OBJ_SAVE_MULT);
        savefile_write_s16b(file, obj->mult);
    }
    if (obj->ident)
    {
        savefile_write_byte(file, OBJ_SAVE_IDENT);
        savefile_write_byte(file, obj->ident);
    }
    if (obj->marked)
    {
        savefile_write_byte(file, OBJ_SAVE_MARKED);
        savefile_write_u32b(file, obj->marked);
    }
    if (obj->flags[0])
    {
        savefile_write_byte(file, OBJ_SAVE_ART_FLAGS_0);
        savefile_write_u32b(file, obj->flags[0]);
    }
    if (obj->flags[1])
    {
        savefile_write_byte(file, OBJ_SAVE_ART_FLAGS_1);
        savefile_write_u32b(file, obj->flags[1]);
    }
    if (obj->flags[2])
    {
        savefile_write_byte(file, OBJ_SAVE_ART_FLAGS_2);
        savefile_write_u32b(file, obj->flags[2]);
    }
    if (obj->flags[3])
    {
        savefile_write_byte(file, OBJ_SAVE_ART_FLAGS_3);
        savefile_write_u32b(file, obj->flags[3]);
    }
    if (obj->flags[4])
    {
        savefile_write_byte(file, OBJ_SAVE_ART_FLAGS_4);
        savefile_write_u32b(file, obj->flags[4]);
    }
    if (obj->flags[5])
    {
        savefile_write_byte(file, OBJ_SAVE_ART_FLAGS_5);
        savefile_write_u32b(file, obj->flags[5]);
    }
    if (obj->flags[6])
    {
        savefile_write_byte(file, OBJ_SAVE_ART_FLAGS_6);
        savefile_write_u32b(file, obj->flags[6]);
    }
    if (obj->flags[7])
    {
        savefile_write_byte(file, OBJ_SAVE_ART_FLAGS_7);
        savefile_write_u32b(file, obj->flags[7]);
    }
    if (obj->curse_flags)
    {
        savefile_write_byte(file, OBJ_SAVE_CURSE_FLAGS);
        savefile_write_u32b(file, obj->curse_flags);
    }
    if (obj->known_flags[0])
    {
        savefile_write_byte(file, OBJ_SAVE_KNOWN_FLAGS_0);
        savefile_write_u32b(file, obj->known_flags[0]);
    }
    if (obj->known_flags[1])
    {
        savefile_write_byte(file, OBJ_SAVE_KNOWN_FLAGS_1);
        savefile_write_u32b(file, obj->known_flags[1]);
    }
    if (obj->known_flags[2])
    {
        savefile_write_byte(file, OBJ_SAVE_KNOWN_FLAGS_2);
        savefile_write_u32b(file, obj->known_flags[2]);
    }
    if (obj->known_flags[3])
    {
        savefile_write_byte(file, OBJ_SAVE_KNOWN_FLAGS_3);
        savefile_write_u32b(file, obj->known_flags[3]);
    }
    if (obj->known_flags[4])
    {
        savefile_write_byte(file, OBJ_SAVE_KNOWN_FLAGS_4);
        savefile_write_u32b(file, obj->known_flags[4]);
    }
    if (obj->known_flags[5])
    {
        savefile_write_byte(file, OBJ_SAVE_KNOWN_FLAGS_5);
        savefile_write_u32b(file, obj->known_flags[5]);
    }
    if (obj->known_flags[6])
    {
        savefile_write_byte(file, OBJ_SAVE_KNOWN_FLAGS_6);
        savefile_write_u32b(file, obj->known_flags[6]);
    }
    if (obj->known_flags[7])
    {
        savefile_write_byte(file, OBJ_SAVE_KNOWN_FLAGS_7);
        savefile_write_u32b(file, obj->known_flags[7]);
    }
    if (obj->known_curse_flags)
    {
        savefile_write_byte(file, OBJ_SAVE_KNOWN_CURSE_FLAGS);
        savefile_write_u32b(file, obj->known_curse_flags);
    }
    if (obj->known_xtra)
    {
        savefile_write_byte(file, OBJ_SAVE_KNOWN_XTRA);
        savefile_write_u32b(file, obj->known_xtra);
    }
    if (obj->rune)
    {
        savefile_write_byte(file, OBJ_SAVE_RUNE_FLAGS);
        savefile_write_u32b(file, obj->rune);
    }
    if (obj->xtra1)
    {
        savefile_write_byte(file, OBJ_SAVE_XTRA1);
        savefile_write_byte(file, obj->xtra1);
    }
    if (obj->xtra2)
    {
        savefile_write_byte(file, OBJ_SAVE_XTRA2);
        savefile_write_byte(file, obj->xtra2);
    }
    if (obj->xtra3)
    {
        savefile_write_byte(file, OBJ_SAVE_XTRA3);
        savefile_write_byte(file, obj->xtra3);
    }
    if (obj->xtra4)
    {
        savefile_write_byte(file, OBJ_SAVE_XTRA4);
        savefile_write_s16b(file, obj->xtra4);
    }
    if (obj->xtra5)
    {
        savefile_write_byte(file, OBJ_SAVE_XTRA5);
        savefile_write_s32b(file, obj->xtra5);
    }
    if (obj->feeling)
    {
        savefile_write_byte(file, OBJ_SAVE_FEELING);
        savefile_write_byte(file, obj->feeling);
    }
    if (obj->inscription)
    {
        savefile_write_byte(file, OBJ_SAVE_INSCRIPTION);
        savefile_write_cptr(file, quark_str(obj->inscription));
    }
    if (obj->art_name)
    {
        savefile_write_byte(file, OBJ_SAVE_ART_NAME);
        savefile_write_cptr(file, quark_str(obj->art_name));
    }
    if (obj->activation.type)
    {
        savefile_write_byte(file, OBJ_SAVE_ACTIVATION);
        savefile_write_s16b(file, obj->activation.type);
        savefile_write_byte(file, obj->activation.power);
        savefile_write_byte(file, obj->activation.difficulty);
        savefile_write_s16b(file, obj->activation.cost);
        savefile_write_s16b(file, obj->activation.extra);
    }
    if (obj->level)
    {
        savefile_write_byte(file, OBJ_SAVE_LEVEL);
        savefile_write_s16b(file, obj->level);
    }

    savefile_write_byte(file, OBJ_SAVE_DONE);
}

/************************************************************************
 * Object Flags
 ************************************************************************/
static of_info_t _of_tbl[] = {
    /* Flavor/Description */
    { OF_HIDE_TYPE,         "Hide Type",       TERM_WHITE,   "HIDE_TYPE",         OFT_OTHER },
    { OF_SHOW_MODS,         "Show Mods",       TERM_WHITE,   "SHOW_MODS",         OFT_OTHER },
    { OF_FULL_NAME,         "Full Name",       TERM_WHITE,   "FULL_NAME",         OFT_OTHER },
    { OF_FIXED_FLAVOR,      "Fixed Flavor",    TERM_WHITE,   "FIXED_FLAVOR",      OFT_OTHER },
    { OF_FAKE,              "Fake Object",     TERM_WHITE,   "",                  0 },

    /* Stats */
    { OF_STR,               "Str",             TERM_L_GREEN, "STR",               OFT_STAT | OFT_PVAL | OFT_LORE },
    { OF_INT,               "Int",             TERM_L_GREEN, "INT",               OFT_STAT | OFT_PVAL | OFT_LORE },
    { OF_WIS,               "Wis",             TERM_L_GREEN, "WIS",               OFT_STAT | OFT_PVAL | OFT_LORE },
    { OF_DEX,               "Dex",             TERM_L_GREEN, "DEX",               OFT_STAT | OFT_PVAL | OFT_LORE },
    { OF_CON,               "Con",             TERM_L_GREEN, "CON",               OFT_STAT | OFT_PVAL | OFT_LORE },
    { OF_CHR,               "Chr",             TERM_L_GREEN, "CHR",               OFT_STAT | OFT_PVAL | OFT_LORE },
    { OF_DEC_STR,           "Dec Str",         TERM_RED,     "DEC_STR",           OFT_STAT | OFT_PVAL | OFT_LORE | OFT_DEC },
    { OF_DEC_INT,           "Dec Int",         TERM_RED,     "DEC_INT",           OFT_STAT | OFT_PVAL | OFT_LORE | OFT_DEC },
    { OF_DEC_WIS,           "Dec Wis",         TERM_RED,     "DEC_WIS",           OFT_STAT | OFT_PVAL | OFT_LORE | OFT_DEC },
    { OF_DEC_DEX,           "Dec Dex",         TERM_RED,     "DEC_DEX",           OFT_STAT | OFT_PVAL | OFT_LORE | OFT_DEC },
    { OF_DEC_CON,           "Dec Con",         TERM_RED,     "DEC_CON",           OFT_STAT | OFT_PVAL | OFT_LORE | OFT_DEC },
    { OF_DEC_CHR,           "Dec Chr",         TERM_RED,     "DEC_CHR",           OFT_STAT | OFT_PVAL | OFT_LORE | OFT_DEC },
    { OF_SUST_STR,          "Sust Str",        TERM_L_BLUE,  "SUST_STR",          OFT_STAT },
    { OF_SUST_INT,          "Sust Int",        TERM_L_BLUE,  "SUST_INT",          OFT_STAT },
    { OF_SUST_WIS,          "Sust Wis",        TERM_L_BLUE,  "SUST_WIS",          OFT_STAT },
    { OF_SUST_DEX,          "Sust Dex",        TERM_L_BLUE,  "SUST_DEX",          OFT_STAT },
    { OF_SUST_CON,          "Sust Con",        TERM_L_BLUE,  "SUST_CON",          OFT_STAT },
    { OF_SUST_CHR,          "Sust Chr",        TERM_L_BLUE,  "SUST_CHR",          OFT_STAT },

    /* Skills/Bonuses */
    { OF_SPEED,             "Speed",           TERM_L_GREEN, "SPEED",             OFT_BONUS | OFT_PVAL | OFT_LORE },
    { OF_STEALTH,           "Stealth",         TERM_L_GREEN, "STEALTH",           OFT_BONUS | OFT_PVAL | OFT_LORE },
    { OF_SEARCH,            "Search",          TERM_L_GREEN, "SEARCH",            OFT_BONUS | OFT_PVAL | OFT_LORE },
    { OF_INFRA,             "Infravision",     TERM_L_GREEN, "INFRA",             OFT_BONUS | OFT_PVAL | OFT_LORE },
    { OF_TUNNEL,            "Tunnel",          TERM_L_GREEN, "TUNNEL",            OFT_BONUS | OFT_PVAL | OFT_LORE },
    { OF_MAGIC_MASTERY,     "Magic Skill",     TERM_L_GREEN, "MAGIC_MASTERY",     OFT_BONUS | OFT_PVAL | OFT_LORE },
    { OF_MAGIC_RESISTANCE,  "Magic Res",       TERM_L_GREEN, "MAGIC_RESISTANCE",  OFT_BONUS | OFT_PVAL | OFT_LORE },
    { OF_SPELL_POWER,       "Spell Power",     TERM_L_GREEN, "SPELL_POWER",       OFT_BONUS | OFT_PVAL | OFT_LORE },
    { OF_SPELL_CAP,         "Spell Cap",       TERM_L_GREEN, "SPELL_CAP",         OFT_BONUS | OFT_PVAL | OFT_LORE },
    { OF_DEVICE_POWER,      "Device Power",    TERM_L_GREEN, "DEVICE_POWER",      OFT_BONUS | OFT_PVAL | OFT_LORE },
    { OF_LIFE,              "Life",            TERM_L_GREEN, "LIFE",              OFT_BONUS | OFT_PVAL | OFT_LORE },
    { OF_DEC_SPEED,         "Dec Speed",       TERM_RED,     "DEC_SPEED",         OFT_BONUS | OFT_PVAL | OFT_LORE | OFT_DEC },
    { OF_DEC_STEALTH,       "Dec Stealth",     TERM_RED,     "DEC_STEALTH",       OFT_BONUS | OFT_PVAL | OFT_LORE | OFT_DEC },
    { OF_DEC_MAGIC_MASTERY, "Dec Magic Skill", TERM_RED,     "DEC_MAGIC_MASTERY", OFT_BONUS | OFT_PVAL | OFT_LORE | OFT_DEC },
    { OF_DEC_SPELL_POWER,   "Dec Spell Power", TERM_RED,     "DEC_SPELL_POWER",   OFT_BONUS | OFT_PVAL | OFT_LORE | OFT_DEC },
    { OF_DEC_SPELL_CAP,     "Dec Spell Cap",   TERM_RED,     "DEC_SPELL_CAP",     OFT_BONUS | OFT_PVAL | OFT_LORE | OFT_DEC },
    { OF_DEC_LIFE,          "Dec Life",        TERM_RED,     "DEC_LIFE",          OFT_BONUS | OFT_PVAL | OFT_LORE | OFT_DEC },

    /* Resists */
    { OF_RES_(GF_ACID),          "Res Acid",        TERM_GREEN,   "RES_ACID",          OFT_RESIST },
    { OF_RES_(GF_ELEC),          "Res Elec",        TERM_BLUE,    "RES_ELEC",          OFT_RESIST },
    { OF_RES_(GF_FIRE),          "Res Fire",        TERM_RED,     "RES_FIRE",          OFT_RESIST },
    { OF_RES_(GF_COLD),          "Res Cold",        TERM_L_WHITE, "RES_COLD",          OFT_RESIST },
    { OF_RES_(GF_POIS),          "Res Poison",      TERM_L_GREEN, "RES_POIS",          OFT_RESIST },
    { OF_RES_(GF_LIGHT),         "Res Light",       TERM_YELLOW,  "RES_LIGHT",         OFT_RESIST },
    { OF_RES_(GF_DARK),          "Res Dark",        TERM_L_DARK,  "RES_DARK",          OFT_RESIST },
    { OF_RES_(GF_CONF),          "Res Confusion",   TERM_L_RED,   "RES_CONF",          OFT_RESIST },
    { OF_RES_(GF_NETHER),        "Res Nether",      TERM_L_DARK,  "RES_NETHER",        OFT_RESIST },
    { OF_RES_(GF_NEXUS),         "Res Nexus",       TERM_VIOLET,  "RES_NEXUS",         OFT_RESIST },
    { OF_RES_(GF_SOUND),         "Res Sound",       TERM_ORANGE,  "RES_SOUND",         OFT_RESIST },
    { OF_RES_(GF_SHARDS),        "Res Shards",      TERM_L_UMBER, "RES_SHARDS",        OFT_RESIST },
    { OF_RES_(GF_CHAOS),         "Res Chaos",       TERM_VIOLET,  "RES_CHAOS",         OFT_RESIST },
    { OF_RES_(GF_DISEN),         "Res Disenchant",  TERM_VIOLET,  "RES_DISEN",         OFT_RESIST },
    { OF_RES_(GF_TIME),          "Res Time",        TERM_L_BLUE,  "RES_TIME",          OFT_RESIST },
    { OF_RES_(GF_BLIND),         "Res Blindness",   TERM_L_DARK,  "RES_BLIND",         OFT_RESIST },
    { OF_RES_(GF_FEAR),          "Res Fear",        TERM_ORANGE,  "RES_FEAR",          OFT_RESIST },
    { OF_IM_(GF_ACID),           "Immune Acid",     TERM_GREEN,   "IM_ACID",           OFT_RESIST },
    { OF_IM_(GF_ELEC),           "Immune Elec",     TERM_BLUE,    "IM_ELEC",           OFT_RESIST },
    { OF_IM_(GF_FIRE),           "Immune Fire",     TERM_RED,     "IM_FIRE",           OFT_RESIST },
    { OF_IM_(GF_COLD),           "Immune Cold",     TERM_L_WHITE, "IM_COLD",           OFT_RESIST },
    { OF_IM_(GF_POIS),           "Immune Poison",   TERM_L_GREEN, "IM_POIS",           OFT_RESIST },
    { OF_IM_(GF_LIGHT),          "Immune Light",    TERM_YELLOW,  "IM_LIGHT",          OFT_RESIST },
    { OF_IM_(GF_DARK),           "Immune Dark",     TERM_L_DARK,  "IM_DARK",           OFT_RESIST },
    { OF_IM_(GF_NETHER),         "Immune Nether",   TERM_L_DARK,  "IM_NETHER",         OFT_RESIST },
    { OF_IM_(GF_BLIND),          "Immune Blindness",TERM_L_DARK,  "IM_BLIND",          OFT_RESIST },
    { OF_IM_(GF_FEAR),           "Immune Fear",     TERM_ORANGE,  "IM_FEAR",           OFT_RESIST },
    { OF_VULN_(GF_ACID),         "Vuln Acid",       TERM_RED,     "VULN_ACID",         OFT_RESIST },
    { OF_VULN_(GF_ELEC),         "Vuln Elec",       TERM_RED,     "VULN_ELEC",         OFT_RESIST },
    { OF_VULN_(GF_FIRE),         "Vuln Fire",       TERM_RED,     "VULN_FIRE",         OFT_RESIST },
    { OF_VULN_(GF_COLD),         "Vuln Cold",       TERM_RED,     "VULN_COLD",         OFT_RESIST },
    { OF_VULN_(GF_POIS),         "Vuln Poison",     TERM_RED,     "VULN_POIS",         OFT_RESIST },
    { OF_VULN_(GF_LIGHT),        "Vuln Light",      TERM_RED,     "VULN_LIGHT",        OFT_RESIST },
    { OF_VULN_(GF_DARK),         "Vuln Dark",       TERM_RED,     "VULN_DARK",         OFT_RESIST },
    { OF_VULN_(GF_CONF),         "Vuln Confusion",  TERM_RED,     "VULN_CONF",         OFT_RESIST },
    { OF_VULN_(GF_NETHER),       "Vuln Nether",     TERM_RED,     "VULN_NETHER",       OFT_RESIST },
    { OF_VULN_(GF_NEXUS),        "Vuln Nexus",      TERM_RED,     "VULN_NEXUS",        OFT_RESIST },
    { OF_VULN_(GF_SOUND),        "Vuln Sound",      TERM_RED,     "VULN_SOUND",        OFT_RESIST },
    { OF_VULN_(GF_SHARDS),       "Vuln Shards",     TERM_RED,     "VULN_SHARDS",       OFT_RESIST },
    { OF_VULN_(GF_CHAOS),        "Vuln Chaos",      TERM_RED,     "VULN_CHAOS",        OFT_RESIST },
    { OF_VULN_(GF_DISEN),        "Vuln Disenchant", TERM_RED,     "VULN_DISEN",        OFT_RESIST },
    { OF_VULN_(GF_BLIND),        "Vuln Blindness",  TERM_RED,     "VULN_BLIND",        OFT_RESIST },
    { OF_VULN_(GF_FEAR),         "Vuln Fear",       TERM_RED,     "VULN_FEAR",         OFT_RESIST },

    /* Abilities */
    { OF_FREE_ACT,          "Free Action",     TERM_L_RED,   "FREE_ACT",          OFT_ABILITY },
    { OF_SEE_INVIS,         "See Invisible",   TERM_L_BLUE,  "SEE_INVIS",         OFT_ABILITY },
    { OF_REGEN,             "Regeneration",    TERM_GREEN,   "REGEN",             OFT_ABILITY | OFT_LORE },
    { OF_HOLD_LIFE,         "Hold Life",       TERM_YELLOW,  "HOLD_LIFE",         OFT_ABILITY },
    { OF_REFLECT,           "Reflection",      TERM_ORANGE,  "REFLECT",           OFT_ABILITY },
    { OF_LEVITATION,        "Levitation",      TERM_L_BLUE,  "LEVITATION",        OFT_ABILITY | OFT_LORE },
    { OF_SLOW_DIGEST,       "Slow Digestion",  TERM_GREEN,   "SLOW_DIGEST",       OFT_ABILITY | OFT_LORE },
    { OF_WARNING,           "Warning",         TERM_YELLOW,  "WARNING",           OFT_ABILITY },
    { OF_NO_MAGIC,          "Anti-Magic",      TERM_RED,     "NO_MAGIC",          OFT_ABILITY },
    { OF_NO_SUMMON,         "Anti-Summoning",  TERM_VIOLET,  "NO_SUMMON",         OFT_ABILITY },
    { OF_NO_TELE,           "Anti-Teleport",   TERM_RED,     "NO_TELE",           OFT_ABILITY },
    { OF_NO_ENCHANT,        "No Enchant",      TERM_L_BLUE,  "NO_ENCHANT",        OFT_ABILITY },
    { OF_NO_REMOVE,         "No Remove",       TERM_L_BLUE,  "NO_REMOVE",         OFT_ABILITY },
    { OF_EASY_SPELL,        "Easy Spell",      TERM_L_GREEN, "EASY_SPELL",        OFT_ABILITY | OFT_LORE },
    { OF_DEC_MANA,          "Economical Mana", TERM_L_BLUE,  "DEC_MANA",          OFT_ABILITY | OFT_LORE },
    { OF_LIGHT,             "Light",           TERM_YELLOW,  "LIGHT",             OFT_ABILITY | OFT_LORE },
    { OF_DARKNESS,          "Darkness",        TERM_L_DARK,  "DARKNESS",          OFT_ABILITY | OFT_LORE },
    { OF_LORE1,             "Pseudo-Identify", TERM_L_BLUE,  "LORE1",             OFT_ABILITY },
    { OF_LORE2,             "Identify",        TERM_L_BLUE,  "LORE2",             OFT_ABILITY },
    { OF_ACTIVATE,          "Activate",        TERM_L_BLUE,  "ACTIVATE",          OFT_ABILITY },
    { OF_IGNORE_ACID,       "Ignore Acid",     TERM_L_WHITE, "IGNORE_ACID",       OFT_OTHER },
    { OF_IGNORE_ELEC,       "Ignore Elec",     TERM_L_WHITE, "IGNORE_ELEC",       OFT_OTHER },
    { OF_IGNORE_FIRE,       "Ignore Fire",     TERM_L_WHITE, "IGNORE_FIRE",       OFT_OTHER },
    { OF_IGNORE_COLD,       "Ignore Cold",     TERM_L_WHITE, "IGNORE_COLD",       OFT_OTHER },

    /* Auras */
    { OF_AURA_ELEC,         "Aura Elec",       TERM_BLUE,    "AURA_ELEC",         OFT_ABILITY | OFT_LORE },
    { OF_AURA_FIRE,         "Aura Fire",       TERM_RED,     "AURA_FIRE",         OFT_ABILITY | OFT_LORE },
    { OF_AURA_COLD,         "Aura Cold",       TERM_L_WHITE, "AURA_COLD",         OFT_ABILITY | OFT_LORE },
    { OF_AURA_SHARDS,       "Aura Shards",     TERM_L_UMBER, "AURA_SHARDS",       OFT_ABILITY | OFT_LORE },
    { OF_AURA_REVENGE,      "Revenge",         TERM_VIOLET,  "AURA_REVENGE",      OFT_ABILITY },
    { OF_AURA_FEAR,         "Aura Fear",       TERM_ORANGE,  "AURA_FEAR",         OFT_ABILITY },

    /* Telepathy */
    { OF_TELEPATHY,         "Telepathy",       TERM_YELLOW,  "TELEPATHY",         OFT_ESP },
    { OF_ESP_EVIL,          "ESP Evil",        TERM_YELLOW,  "ESP_EVIL",          OFT_ESP },
    { OF_ESP_GOOD,          "ESP Good",        TERM_WHITE,   "ESP_GOOD",          OFT_ESP },
    { OF_ESP_NONLIVING,     "ESP Nonliving",   TERM_L_BLUE,  "ESP_NONLIVING",     OFT_ESP },
    { OF_ESP_UNIQUE,        "ESP Unique",      TERM_VIOLET,  "ESP_UNIQUE",        OFT_ESP },
    { OF_ESP_DRAGON,        "ESP Dragon",      TERM_RED,     "ESP_DRAGON",        OFT_ESP },
    { OF_ESP_DEMON,         "ESP Demon",       TERM_L_RED,   "ESP_DEMON",         OFT_ESP },
    { OF_ESP_UNDEAD,        "ESP Undead",      TERM_L_DARK,  "ESP_UNDEAD",        OFT_ESP },
    { OF_ESP_ANIMAL,        "ESP Animal",      TERM_L_BLUE,  "ESP_ANIMAL",        OFT_ESP },
    { OF_ESP_HUMAN,         "ESP Human",       TERM_SLATE,   "ESP_HUMAN",         OFT_ESP },
    { OF_ESP_ORC,           "ESP Orc",         TERM_L_UMBER, "ESP_ORC",           OFT_ESP },
    { OF_ESP_TROLL,         "ESP Troll",       TERM_GREEN,   "ESP_TROLL",         OFT_ESP },
    { OF_ESP_GIANT,         "ESP Giant",       TERM_UMBER,   "ESP_GIANT",         OFT_ESP },

    /* Weapons */
    { OF_SLAY_EVIL,         "Slay Evil",       TERM_YELLOW,  "SLAY_EVIL",         OFT_SLAY },
    { OF_SLAY_GOOD,         "Slay Good",       TERM_L_WHITE, "SLAY_GOOD",         OFT_SLAY },
    { OF_SLAY_LIVING,       "Slay Living",     TERM_ORANGE,  "SLAY_LIVING",       OFT_SLAY },
    { OF_SLAY_DRAGON,       "Slay Dragons",    TERM_RED,     "SLAY_DRAGON",       OFT_SLAY },
    { OF_SLAY_DEMON,        "Slay Demons",     TERM_L_RED,   "SLAY_DEMON",        OFT_SLAY },
    { OF_SLAY_UNDEAD,       "Slay Undead",     TERM_L_DARK,  "SLAY_UNDEAD",       OFT_SLAY },
    { OF_SLAY_ANIMAL,       "Slay Animals",    TERM_GREEN,   "SLAY_ANIMAL",       OFT_SLAY },
    { OF_SLAY_HUMAN,        "Slay Humans",     TERM_SLATE,   "SLAY_HUMAN",        OFT_SLAY },
    { OF_SLAY_ORC,          "Slay Orcs",       TERM_L_UMBER, "SLAY_ORC",          OFT_SLAY },
    { OF_SLAY_TROLL,        "Slay Trolls",     TERM_GREEN,   "SLAY_TROLL",        OFT_SLAY },
    { OF_SLAY_GIANT,        "Slay Giants",     TERM_UMBER,   "SLAY_GIANT",        OFT_SLAY },

    { OF_KILL_EVIL,         "Kill Evil",       TERM_YELLOW,  "KILL_EVIL",         OFT_SLAY },
    { OF_KILL_DRAGON,       "Kill Dragons",    TERM_RED,     "KILL_DRAGON",       OFT_SLAY },
    { OF_KILL_DEMON,        "Kill Demons",     TERM_L_RED,   "KILL_DEMON",        OFT_SLAY },
    { OF_KILL_UNDEAD,       "Kill Undead",     TERM_L_DARK,  "KILL_UNDEAD",       OFT_SLAY },
    { OF_KILL_ANIMAL,       "Kill Animals",    TERM_GREEN,   "KILL_ANIMAL",       OFT_SLAY },
    { OF_KILL_HUMAN,        "Kill Humans",     TERM_SLATE,   "KILL_HUMAN",        OFT_SLAY },
    { OF_KILL_ORC,          "Kill Orcs",       TERM_L_UMBER, "KILL_ORC",          OFT_SLAY },
    { OF_KILL_TROLL,        "Kill Trolls",     TERM_GREEN,   "KILL_TROLL",        OFT_SLAY },
    { OF_KILL_GIANT,        "Kill Giants",     TERM_UMBER,   "KILL_GIANT",        OFT_SLAY },

    { OF_BRAND_ACID,        "Acid Brand",      TERM_GREEN,   "BRAND_ACID",        OFT_BRAND },
    { OF_BRAND_ELEC,        "Elec Brand",      TERM_BLUE,    "BRAND_ELEC",        OFT_BRAND },
    { OF_BRAND_FIRE,        "Flame Tongue",    TERM_RED,     "BRAND_FIRE",        OFT_BRAND },
    { OF_BRAND_COLD,        "Frost Brand",     TERM_L_WHITE, "BRAND_COLD",        OFT_BRAND },
    { OF_BRAND_POIS,        "Viper's Fang",    TERM_L_GREEN, "BRAND_POIS",        OFT_BRAND },
    { OF_BRAND_CHAOS,       "Mark of Chaos",   TERM_VIOLET,  "BRAND_CHAOS",       OFT_BRAND },
    { OF_BRAND_VAMP,        "Vampiric",        TERM_L_DARK,  "BRAND_VAMP",        OFT_BRAND },
    { OF_BRAND_MANA,        "Mana Brand",      TERM_L_BLUE,  "BRAND_MANA",        OFT_BRAND },
    { OF_BRAND_LIGHT,       "Light Brand",     TERM_YELLOW,  "BRAND_LIGHT",       OFT_BRAND },
    { OF_BRAND_DARK,        "Dark Brand",      TERM_L_DARK,  "BRAND_DARK",        OFT_BRAND },
    { OF_BRAND_TIME,        "Time Brand",      TERM_L_BLUE,  "BRAND_TIME",        OFT_BRAND },
    { OF_BRAND_PLASMA,      "Plasma Brand",    TERM_L_RED,   "BRAND_PLASMA",      OFT_BRAND },
    { OF_VORPAL,            "Sharpness",       TERM_L_RED,   "VORPAL",            OFT_BRAND },
    { OF_VORPAL2,           "*Sharpness*",     TERM_VIOLET,  "VORPAL2",           OFT_BRAND },
    { OF_IMPACT,            "Earthquakes",     TERM_L_UMBER, "IMPACT",            OFT_BRAND },
    { OF_STUN,              "Stuns",           TERM_ORANGE,  "STUN",              OFT_BRAND },
    { OF_CRIT,              "Criticals",       TERM_RED,     "CRIT",              OFT_BRAND },

    { OF_BLESSED,           "Blessed",         TERM_L_BLUE,  "BLESSED",           OFT_OTHER },
    { OF_RIDING,            "Riding",          TERM_ORANGE,  "RIDING",            OFT_OTHER },
    { OF_THROWING,          "Throwing",        TERM_L_DARK,  "THROWING",          OFT_OTHER },

    { OF_BLOWS,             "Attack Speed",    TERM_L_GREEN, "BLOWS",             OFT_BONUS | OFT_PVAL | OFT_LORE },
    { OF_DEC_BLOWS,         "Dec Attack Speed",TERM_RED,     "DEC_BLOWS",         OFT_BONUS | OFT_PVAL | OFT_LORE | OFT_DEC },
    { OF_WEAPONMASTERY,     "Weaponmastery",   TERM_L_GREEN, "WEAPONMASTERY",     OFT_ABILITY | OFT_PVAL | OFT_LORE },
    { OF_DUAL_WIELDING,     "Dual Wielding",   TERM_L_BLUE,  "DUAL_WIELDING",     OFT_ABILITY },
    { OF_XTRA_MIGHT,        "Extra Might",     TERM_L_GREEN, "XTRA_MIGHT",        OFT_BONUS | OFT_PVAL | OFT_LORE },
    { OF_XTRA_SHOTS,        "Shooting Speed",  TERM_L_GREEN, "XTRA_SHOTS",        OFT_BONUS | OFT_PVAL | OFT_LORE },

    /* Curses */
    { OF_DRAIN_EXP,         "Drain Exp",       TERM_YELLOW,  "DRAIN_EXP",         OFT_CURSE },
    { OF_TELEPORT,          "Teleport",        TERM_L_BLUE,  "TELEPORT",          OFT_CURSE },
    { OF_AGGRAVATE,         "Aggravate",       TERM_RED,     "AGGRAVATE",         OFT_CURSE },
    { OF_TY_CURSE,          "Ancient Curse",   TERM_VIOLET,  "TY_CURSE",          OFT_CURSE },

    { OF_SPELL_DAM,         "Spell Damage",    TERM_WHITE,   "SPELL_DAM",         OFT_OTHER | OFT_LORE },
    { OF_ARCHERY,           "Archery Damage",  TERM_WHITE,   "ARCHERY",           OFT_OTHER | OFT_LORE },
    { OF_MELEE,             "Melee Damage",    TERM_WHITE,   "MELEE",             OFT_OTHER | OFT_LORE },

    /* New object flags ... reorder when able */
    { 0 }
};

of_info_ptr of_parse_name(cptr token)
{
    static str_map_ptr _map = NULL;
    if (!_map)
    {
        int i;
        _map = str_map_alloc(NULL);
        for (i = 0; ; i++)
        {
            of_info_ptr info = &_of_tbl[i];
            if (!info->name) break;
            str_map_add(_map, info->parse, info);
        }
    }
    return str_map_find(_map, token);
}

of_info_ptr of_lookup(int id)
{
    static int_map_ptr _map = NULL;
    if (!_map)
    {
        int i;
        _map = int_map_alloc(NULL);
        for (i = 0; ; i++)
        {
            of_info_ptr info = &_of_tbl[i];
            if (!info->name) break;
            int_map_add(_map, info->id, info);
        }
    }
    return int_map_find(_map, id);
}

static bool _of_is_pval(of_info_ptr info) { return BOOL(info->flags & OFT_PVAL); }
vec_ptr of_lookup_pval(void) { return of_lookup_filter(_of_is_pval); }

static bool _of_is_stat(of_info_ptr info) { return BOOL(info->flags & OFT_STAT); }
vec_ptr of_lookup_stat(void) { return of_lookup_filter(_of_is_stat); }

static bool _of_is_bonus(of_info_ptr info) { return BOOL(info->flags & OFT_BONUS); }
vec_ptr of_lookup_bonus(void) { return of_lookup_filter(_of_is_bonus); }

static bool _of_is_resist(of_info_ptr info) { return BOOL(info->flags & OFT_RESIST); }
vec_ptr of_lookup_resist(void) { return of_lookup_filter(_of_is_resist); }

static bool _of_is_ability(of_info_ptr info) { return BOOL(info->flags & OFT_ABILITY); }
vec_ptr of_lookup_ability(void) { return of_lookup_filter(_of_is_ability); }

static bool _of_is_esp(of_info_ptr info) { return BOOL(info->flags & OFT_ESP); }
vec_ptr of_lookup_esp(void) { return of_lookup_filter(_of_is_esp); }

static bool _of_is_slay(of_info_ptr info) { return BOOL(info->flags & OFT_SLAY); }
vec_ptr of_lookup_slay(void) { return of_lookup_filter(_of_is_slay); }

static bool _of_is_brand(of_info_ptr info) { return BOOL(info->flags & OFT_BRAND); }
vec_ptr of_lookup_brand(void) { return of_lookup_filter(_of_is_brand); }

static bool _of_is_curse(of_info_ptr info) { return BOOL(info->flags & OFT_CURSE); }
vec_ptr of_lookup_curse(void) { return of_lookup_filter(_of_is_curse); }

static bool _of_is_other(of_info_ptr info) { return BOOL(info->flags & OFT_OTHER); }
vec_ptr of_lookup_other(void) { return of_lookup_filter(_of_is_other); }

vec_ptr of_lookup_filter(of_info_p p)
{
    vec_ptr v = vec_alloc(NULL);
    int     i;
    for (i = 0; ; i++)
    {
        of_info_ptr info = &_of_tbl[i];
        if (!info->name) break;
        if (p && !p(info)) continue;
        vec_add(v, info);
    }
    return v;
}

vec_ptr of_info(u32b flgs[OF_ARRAY_SIZE])
{
    return of_filter(flgs, NULL);
}

vec_ptr of_filter(u32b flgs[OF_ARRAY_SIZE], of_info_p p)
{
    vec_ptr v = vec_alloc(NULL);
    int     i;
    for (i = 0; ; i++)
    {
        of_info_ptr info = &_of_tbl[i];
        if (!info->name) break;
        if (p && !p(info)) continue;
        if (!have_flag(flgs, info->id)) continue;
        vec_add(v, info);
    }
    return v;
}

/* Optimized, but I doubt this is worth optimizing. Artifact creation
 * needs to know whether or not to roll a pval. */
bool of_has_pval(u32b flgs[OF_ARRAY_SIZE])
{
    static u32b mask[OF_ARRAY_SIZE]; /* static memory is 0 initialized by standard C */
    int i;

    assert(OF_STR < 32); /* rely on non-zero first word to detect an initialized mask */
    if (!mask[0])
    {
        for (i = 0; ; i++)
        {
            of_info_ptr info = &_of_tbl[i];
            if (!info->name) break;
            if (!(info->flags & OFT_PVAL)) continue;
            add_flag(mask, info->id);
        }
    }

    for (i = 0; i < OF_ARRAY_SIZE; i++)
    {
        if (flgs[i] & mask[i])
            return TRUE;
    }
    return FALSE;
}

/* For random artifacts, its nice to allow high pvals on boots and
 * rings, provided that OF_SPEED is the only pval flag for the object. */
bool of_has_nonspeed_pval(u32b flgs[OF_ARRAY_SIZE])
{
    static u32b mask[OF_ARRAY_SIZE];
    int i;

    assert(OF_STR < 32);
    if (!mask[0])
    {
        for (i = 0; ; i++)
        {
            of_info_ptr info = &_of_tbl[i];
            if (!info->name) break;
            if (info->id == OF_SPEED) continue;
            if (!(info->flags & OFT_PVAL)) continue;
            add_flag(mask, info->id);
        }
    }

    for (i = 0; i < OF_ARRAY_SIZE; i++)
    {
        if (flgs[i] & mask[i])
            return TRUE;
    }
    return FALSE;
}

/* And here is a non-optimized way to check for a certain kind of flag. */
bool of_has(u32b flgs[OF_ARRAY_SIZE], of_info_p p)
{
    int i;
    for (i = 0; ; i++)
    {
        of_info_ptr info = &_of_tbl[i];
        if (!info->name) break;
        if (p && !p(info)) continue;
        if (have_flag(flgs, info->id)) return TRUE;
    }
    return FALSE;
}

/************************************************************************
 * Object Drops
 ***********************************************************************/
static bool _replace_art(art_ptr art)
{
    if (art->generated) return TRUE;
    if ( random_artifacts
      && !(art->gen_flags & OFG_FIXED_ART)
      && randint0(100) < random_artifact_pct )
    {
        return TRUE;
    }
    return FALSE;
}

static bool _obj_kind_is_good = FALSE;
static int _obj_kind_hack = 0;
static bool _kind_is_hi_book(int k_idx)
{
    obj_kind_ptr kind;
    if (!obj_kind_is_spellbook(k_idx)) return FALSE;
    kind = &k_info[k_idx];
    if (kind->sval < SV_BOOK_MIN_GOOD) return FALSE;
    if (kind->tval == TV_ARCANE_BOOK) return FALSE;
    return TRUE;
}
static bool _obj_kind_hook(int k_idx)
{
    /* Aside: kind_is_good() will reject high level books once a certain number have been
     * found. For monsters with DROP_GOOD, this means they will roll a new object until
     * they get a non-book class of objects. For Quests and Room templates, OBJ(BOOK, DEPTH+5),
     * for example, will yield no object at all which is probably a bad thing. */
    if (_obj_kind_is_good && !kind_is_good(k_idx) && _obj_kind_hack != OBJ_TYPE_HI_BOOK)
        return FALSE;

    switch (_obj_kind_hack)
    {
    case OBJ_TYPE_DEVICE:       return obj_kind_is_device(k_idx);
    case OBJ_TYPE_JEWELRY:      return obj_kind_is_jewelry(k_idx);
    case OBJ_TYPE_BOOK:         return obj_kind_is_spellbook(k_idx);
    case OBJ_TYPE_HI_BOOK:      return _kind_is_hi_book(k_idx);
    case OBJ_TYPE_BODY_ARMOR:   return obj_kind_is_body_armor(k_idx);
    case OBJ_TYPE_OTHER_ARMOR:  return kind_is_other_armor(k_idx);
    case OBJ_TYPE_WEAPON:       return obj_kind_is_weapon(k_idx);
    case OBJ_TYPE_BOW_AMMO:     return kind_is_bow_ammo(k_idx);
    case OBJ_TYPE_MISC:         return obj_kind_is_misc(k_idx);
    default:                    return k_info[k_idx].tval == _obj_kind_hack;
    }
}

/* Make an object from drop info. level is the base level and mode
 * are extra flags (e.g. AM_UNIQUE or AM_VAULT). We assume you
 * plan to give this object to the player, so standard arts are
 * marked as generated! This routine can fail returning NULL. */
obj_ptr obj_drop_make(obj_drop_ptr info, int level, int mode)
{
    obj_t forge = {0};

    level = MAX(1, level + info->boost);

    /* initialize the mode for apply_magic, extending the user supplied
     * value with any exta AM_* flags from the drop info */
    mode |= info->flags & AM_MASK;

    if (info->flags & OBJ_DROP_STD_ART)
    {
        art_ptr art = arts_lookup(info->object);
        if (no_artifacts)
            object_prep(&forge, lookup_kind(TV_SCROLL, SV_SCROLL_ACQUIREMENT));
        else if (_replace_art(art))
        {
            /* Monster drops should not replace standard arts. Quest rewards,
             * on the other hand, definitely should generate replacements! */
            if ((info->flags & OBJ_DROP_MON) && art->generated)
                return NULL;
            art_create_replacement(&forge, art, mode);
        }
        else
        {
            art_create_std(&forge, art, mode);
        }
    }
    else if (info->flags & OBJ_DROP_RANDOM)
    {
        /* XXX Historical: Allow an object(80%) or gold(20%) */
        if ((info->flags & OBJ_DROP_SOME_GOLD) && randint0(100) < 20)
        {
            make_gold(&forge, level);
        }
        else
        {
            if (info->flags & OBJ_DROP_RAND_ART)
                mode |= AM_GOOD | AM_GREAT | AM_SPECIAL | AM_NO_FIXED_ART;
            else if (info->flags & OBJ_DROP_RAND_EGO) /* EGO(*) should allow artifacts */
                mode |= AM_GOOD | AM_GREAT;
            else if (info->boost) /* XXX historically, DEPTH+N implied AM_GOOD */
                mode |= AM_GOOD;
            make_object(&forge, level, mode);
        }
    }
    else if ((info->flags & OBJ_DROP_TYPE) && info->object == TV_GOLD)
    {
        if (info->flags & OBJ_DROP_COIN)
            make_gold_aux(&forge, info->extra, level);
        else
            make_gold(&forge, level);
    }
    else if (info->object)
    {
        int k_idx;

        if (info->flags & OBJ_DROP_TYPE)
        {
            /* make sure certain objects actually get generated. get_obj_num()
             * will fail to pick up objects that are outside depth restrictions. */
            if (info->object == TV_JUNK || info->object == TV_SKELETON)
                level = 1;
            else if (info->object == TV_RING || info->object == TV_AMULET)
                level = MAX(level, 10);

            _obj_kind_is_good = FALSE;
            if (info->boost > 0 || (mode & AM_GOOD))
                _obj_kind_is_good = TRUE;
            _obj_kind_hack = info->object;
            get_obj_num_hook = _obj_kind_hook;
            get_obj_num_prep();
            k_idx = get_obj_num(level);
            get_obj_num_hook = NULL;
            get_obj_num_prep();
        }
        else
            k_idx = info->object;

        if (k_idx)
        {
            if (info->flags & OBJ_DROP_RAND_ART)
            {
                mode |= AM_GOOD | AM_GREAT | AM_SPECIAL | AM_NO_FIXED_ART;
            }
            else if (info->flags & OBJ_DROP_RAND_EGO)
            {
                mode |= AM_GOOD | AM_GREAT | AM_NO_FIXED_ART;
            }
            else if (info->flags & OBJ_DROP_STD_EGO)
            {
                mode |= AM_GOOD | AM_GREAT | AM_FORCE_EGO;
                apply_magic_ego = info->extra;
            }
            else if (info->boost)
                mode |= AM_GOOD;

            object_prep(&forge, k_idx);
            if (obj_is_device(&forge) && (info->flags & OBJ_DROP_EFFECT))
            {
                /* Hack: There is only a single k_idx for each class of devices, so
                 * we use the ego index to pick an effect. This means there is no way
                 * to actually grant an ego device ...*/
                if (!device_init_fixed(&forge, info->extra))
                {
                    if (info->extra)
                    {
                        char     name[255];
                        effect_t e = {0};
                        e.type = info->extra;
                        sprintf(name, "%s", do_effect(&e, SPELL_NAME, 0));
                        msg_format("Software Bug: %s is not a valid effect for this device.", name);
                        msg_print("Generating a random device instead.");
                    }
                    device_init(&forge, level, 0);
                }
            }
            else
            {
                apply_magic(&forge, level, mode);
                obj_make_pile(&forge);
            }
        }
    }
    if (forge.k_idx) return obj_copy(&forge);
    return NULL;
}

/************************************************************************
 * Object Drop Parsing
 * Pulled out of init1.c
 ***********************************************************************/
struct _object_type_s
{
    cptr name;
    int  type;
    int  ego_type;
};
typedef struct _object_type_s _object_type_t;

/* Use the following keywords in OBJ(), EGO() and ART() directives
   to force a random object of the indicated tval.
   Regexp: \#define TV_{[A-Z_0-9]+}[ ]+[0-9]+ -> { "\1", TV_\1 }, */
static _object_type_t _object_types[] =
{
    { "JUNK",               TV_JUNK },
    { "SKELETON",           TV_SKELETON },
    { "STATUE",             TV_STATUE },
    { "FIGURINE",           TV_FIGURINE },
    { "CHEST",              TV_CHEST },
    { "SHOT",               TV_SHOT, EGO_TYPE_AMMO },
    { "ARROW",              TV_ARROW, EGO_TYPE_AMMO },
    { "BOLT",               TV_BOLT, EGO_TYPE_AMMO },
    { "BOW",                TV_BOW, EGO_TYPE_BOW },
    { "DIGGING",            TV_DIGGING, EGO_TYPE_DIGGER },
    { "HAFTED",             TV_HAFTED, EGO_TYPE_WEAPON },
    { "POLEARM",            TV_POLEARM, EGO_TYPE_WEAPON },
    { "SWORD",              TV_SWORD, EGO_TYPE_WEAPON },
    { "BOOTS",              TV_BOOTS, EGO_TYPE_BOOTS },
    { "GLOVES",             TV_GLOVES, EGO_TYPE_GLOVES },
    { "HELM",               TV_HELM, EGO_TYPE_HELMET },
    { "CROWN",              TV_CROWN, EGO_TYPE_CROWN },
    { "SHIELD",             TV_SHIELD, EGO_TYPE_SHIELD },
    { "CLOAK",              TV_CLOAK, EGO_TYPE_CLOAK },
    { "SOFT_ARMOR",         TV_SOFT_ARMOR, EGO_TYPE_BODY_ARMOR },
    { "HARD_ARMOR",         TV_HARD_ARMOR, EGO_TYPE_BODY_ARMOR },
    { "DRAG_ARMOR",         TV_DRAG_ARMOR, EGO_TYPE_BODY_ARMOR },
    { "LIGHT",              TV_LIGHT, EGO_TYPE_LIGHT },
    { "AMULET",             TV_AMULET, EGO_TYPE_AMULET },
    { "RING",               TV_RING, EGO_TYPE_RING },
    { "STAFF",              TV_STAFF, EGO_TYPE_DEVICE },
    { "WAND",               TV_WAND, EGO_TYPE_DEVICE },
    { "ROD",                TV_ROD, EGO_TYPE_DEVICE },
    { "SCROLL",             TV_SCROLL },
    { "POTION",             TV_POTION },
    { "FOOD",               TV_FOOD },
    { "LIFE_BOOK",          TV_LIFE_BOOK },
    { "SORCERY_BOOK",       TV_SORCERY_BOOK },
    { "NATURE_BOOK",        TV_NATURE_BOOK },
    { "CHAOS_BOOK",         TV_CHAOS_BOOK },
    { "DEATH_BOOK",         TV_DEATH_BOOK },
    { "TRUMP_BOOK",         TV_TRUMP_BOOK },
    { "ARCANE_BOOK",        TV_ARCANE_BOOK },
    { "CRAFT_BOOK",         TV_CRAFT_BOOK },
    { "DAEMON_BOOK",        TV_DAEMON_BOOK },
    { "CRUSADE_BOOK",       TV_CRUSADE_BOOK },
    { "NECROMANCY_BOOK",    TV_NECROMANCY_BOOK },
    { "ARMAGEDDON_BOOK",    TV_ARMAGEDDON_BOOK },
    { "ILLUSION_BOOK",      TV_ILLUSION_BOOK },
    { "MUSIC_BOOK",         TV_MUSIC_BOOK },
    { "HISSATSU_BOOK",      TV_HISSATSU_BOOK },
    { "HEX_BOOK",           TV_HEX_BOOK },
    { "RAGE_BOOK",          TV_RAGE_BOOK },
    { "BURGLARY_BOOK",      TV_BURGLARY_BOOK },
    { "BLESS_BOOK",         TV_BLESS_BOOK },
    { "GOLD",               TV_GOLD },
    { "DEVICE",             OBJ_TYPE_DEVICE, EGO_TYPE_DEVICE },
    { "JEWELRY",            OBJ_TYPE_JEWELRY },
    { "BOOK",               OBJ_TYPE_BOOK },
    { "HI_BOOK",            OBJ_TYPE_HI_BOOK },
    { "BODY_ARMOR",         OBJ_TYPE_BODY_ARMOR, EGO_TYPE_BODY_ARMOR },
    { "OTHER_ARMOR",        OBJ_TYPE_OTHER_ARMOR },
    { "WEAPON",             OBJ_TYPE_WEAPON, EGO_TYPE_WEAPON },
    { "BOW_AMMO",           OBJ_TYPE_BOW_AMMO, EGO_TYPE_AMMO },
    { "MISC",               OBJ_TYPE_MISC },
    { 0, 0 }
};

static int _lookup_ego_type(int object)
{
    int i;
    for (i = 0; ; i++)
    {
        if (!_object_types[i].name) return EGO_TYPE_NONE;
        if (_object_types[i].type == object)
            return _object_types[i].ego_type;
    }
}

/* OBJ(WAND_ROCKET)      -> _parse_effect(TV_WAND, "ROCKET")     -> EFFECT_ROCKET
 * OBJ(STAFF_MANA_STORM) -> _parse_effect(TV_STAFF, "MANA_STORM")-> EFFECT_MANA_STORM */
static errr _parse_effect(int tval, cptr arg, obj_drop_ptr info, int options)
{
    int effect_id = effect_parse_type(arg);
    if (!effect_id)
    {
        msg_format("Unkown effect: %s", arg);
        return PARSE_ERROR_GENERIC;
    }
    if (!device_is_valid_effect(tval, effect_id))
    {
        msg_format("Invalid effect for this device type: %s (%d)", arg, effect_id);
        return PARSE_ERROR_GENERIC;
    }
    info->object = lookup_kind(tval, SV_ANY);
    info->extra = effect_id;
    info->flags |= OBJ_DROP_EFFECT;
    if (trace_doc)
    {
        char     name[255];
        effect_t e = {0};
        e.type = info->extra;
        sprintf(name, "%s", do_effect(&e, SPELL_NAME, 0));
        doc_printf(trace_doc, "Mapping effect <color:B>%s</color> to <color:R>%s</color> (%d).\n",
            arg, name, info->extra);
    }
    return 0;
}
static errr _parse_gold(cptr arg, obj_drop_ptr info, int options)
{
    if (strcmp(arg, "COPPER") == 0)
        info->extra = SV_COPPER;
    else if (strcmp(arg, "SILVER") == 0)
        info->extra = SV_SILVER;
    else if (strcmp(arg, "GOLD") == 0)
        info->extra = SV_GOLD;
    else if (strcmp(arg, "MITHRIL") == 0)
        info->extra = SV_MITHRIL;
    else if (strcmp(arg, "ADAMANT") == 0)
        info->extra = SV_ADAMANT;
    else
    {
        msg_format("Invalid gold type: %s", arg);
        return PARSE_ERROR_GENERIC;
    }
    info->object = TV_GOLD;
    info->flags |= OBJ_DROP_TYPE | OBJ_DROP_COIN;
    return 0;
}

/* OBJ(*)                       Any object
   OBJ(*, DEPTH+7)              Any object, 7 levels OoD
   OBJ(potion of healing)       Potion of Healing
   OBJ(POTION)                  Any potion
   OBJ(diamond edge, DEPTH+100) A really deep diamond edge
   OBJ(STAFF_MANA_STORM)        A staff with EFFECT_MANA_STORM
   OBJ(mushroom of cure serious wounds)
   OBJ(potion of cure serious wounds)   i.e. you must disambiguate!
*/
errr obj_drop_parse_obj(char **args, int arg_ct, obj_drop_ptr info, int options)
{
    switch (arg_ct)
    {
    case 2:
    {
        char *flags[10];
        int   flag_ct = z_string_split(args[1], flags, 10, "|");
        int   i, n;

        for (i = 0; i < flag_ct; i++)
        {
            char* flag = flags[i];
            if (sscanf(flag, "DEPTH+%d", &n) == 1)
            {
                info->boost = n;
            }
            /* XXX Removed for monster drops ... This 'feature' wasn't being used anyway
            else if (sscanf(flag, "%d%%", &n) == 1)
            {
                info->pct = n;
            } */
            else if (strcmp(flag, "GOOD") == 0)
            {
                info->flags |= AM_GOOD;
            }
            else if (strcmp(flag, "GREAT") == 0) /* XXX replace OBJ(RING):EGO(*) with OBJ(RING, GREAT) */
            {
                info->flags |= AM_GOOD | AM_GREAT;
            }
            else if (strcmp(flag, "TAILORED") == 0)
            {
                info->flags |= AM_TAILORED;
            }
            else
            {
                msg_format("Error: Invalid object option %s.", flag);
                return PARSE_ERROR_GENERIC;
            }
        }
        /* vvvvvvvvvvvvv Fall Through vvvvvvvvvvvvv */
    }
    case 1:
        if (streq(args[0], "*"))
        {
            info->flags |= OBJ_DROP_RANDOM;
        }
        else if (streq(args[0], "*$"))
        {
            info->flags |= OBJ_DROP_RANDOM | OBJ_DROP_SOME_GOLD;
        }
        else
        {
            int i;
            /* OBJ(WAND_ROCKET) ... note: OBJ(WAND) will fall thru to normal type handling */
            if (prefix(args[0], "WAND_"))
                return _parse_effect(TV_WAND, args[0] + strlen("WAND_"), info, options);
            if (prefix(args[0], "ROD_"))
                return _parse_effect(TV_ROD, args[0] + strlen("ROD_"), info, options);
            if (prefix(args[0], "STAFF_"))
                return _parse_effect(TV_STAFF, args[0] + strlen("STAFF_"), info, options);

            /* OBJ(GOLD_COPPER) to force Creeping Copper Coins to drop copper coins, etc
             * OBJ(GOLD) rolls random coin types and is handled below in _object_types[] */
            if (prefix(args[0], "GOLD_"))
                return _parse_gold(args[0] + strlen("GOLD_"), info, options);

            /* OBJ(SWORD) */
            for (i = 0; ; i++)
            {
                if (!_object_types[i].name) break;
                if (streq(args[0], _object_types[i].name))
                {
                    info->object = _object_types[i].type;
                    info->flags |= OBJ_DROP_TYPE;
                    return 0;
                }
            }
            /* OBJ(^dagger$) */
            info->object = parse_lookup_kind(args[0], options);
            if (!info->object)
            {
                /* OBJ(212) ... whatever that might be?? */
                info->object = atoi(args[0]);
                if (!info->object)
                {
                    msg_format("Invalid object: %s", args[0]);
                    return PARSE_ERROR_GENERIC;
                }
            }
        }
        break;
    default:
        msg_print("Error: Invalid OBJ() directive. Syntax: OBJ(<which> [,<flags>]).");
        return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    }
    return 0;
}

static int _lookup_ego(cptr name, int type, int options)
{
    int i;
    for (i = 1; i < max_e_idx; i++)
    {
        ego_type *e_ptr = &e_info[i];
        char      buf[255];
        if (!e_ptr->name) continue;
        if (type && !(e_ptr->type & type)) continue;
        parse_prep_name(buf, e_ptr->name);
        if (strstr(buf, name))
        {
            if (trace_doc)
                doc_printf(trace_doc, "Matching ego <color:B>%s</color> to <color:R>%s</color> (%d).\n", name, e_ptr->name, i);
            return i;
        }
    }
    return 0;
}

static bool _is_digit(char c)
{
    if ('0' <= c && c <= '9')
        return TRUE;
    return FALSE;
}

static bool _is_numeric(const char *token) /* [0-9]+ */
{
    const char *c = token;
    if (!*c) return FALSE;
    if (!_is_digit(*c)) return FALSE;
    for (c++; *c; c++)
    {
        if (!_is_digit(*c)) return FALSE;
    }
    return TRUE;
}

/* OBJ(RING):EGO(speed)          Ring of Speed
   OBJ(RING):EGO(*)              Any ego ring
   OBJ(CLOAK, DEPTH+20):EGO(*)   Any ego cloak generated 20 levels OoD
   OBJ(RING, DEPTH+50):EGO(speed) Ring of Speed generated 50 level OoD
   OBJ(SWORD):EGO(pattern)       A pattern blade
*/   
errr obj_drop_parse_ego(char **args, int arg_ct, obj_drop_ptr info, int options)
{
    switch (arg_ct)
    {
    case 1:
        if (streq(args[0], "*"))
        {
            info->flags |= OBJ_DROP_RAND_EGO;
        }
        else
        {
            if (_is_numeric(args[0]))
                info->extra = atoi(args[0]);
            else if (info->flags & OBJ_DROP_TYPE)
            {
                int type = _lookup_ego_type(info->object);
                info->extra = _lookup_ego(args[0], type, options);
            }
            else if (info->object && !(info->flags & OBJ_DROP_RANDOM))
            {
                int type = _lookup_ego_type(k_info[info->object].tval);
                info->extra = _lookup_ego(args[0], type, options);
            }
            else
                info->extra = _lookup_ego(args[0], 0, options);
            if (!info->extra)
            {
                msg_format("Error: Unknown Ego %s.", args[0]);
                return PARSE_ERROR_GENERIC;
            }
            info->flags |= OBJ_DROP_STD_EGO;
        }
        break;

    default:
        msg_print("Error: Invalid EGO() directive. Syntax: EGO(<which>).");
        return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    }
    return 0;
}

/* OBJ(CLOAK, DEPTH+20):ART(*)   Rand-art cloak generated 20 levels OoD
   ART(|.Crisdurian)             The Executioner's Sword 'Crisdurian' */
errr obj_drop_parse_art(char **args, int arg_ct, obj_drop_ptr info, int options)
{
    switch (arg_ct)
    {
    case 1:
        if (streq(args[0], "*"))
        {
            info->flags |= OBJ_DROP_RAND_ART;
        }
        else
        {
            art_ptr art = arts_parse(args[0]);
            if (!art)
            {
                msg_format("<color:v>Error</color>: Unknown Artifact <color:R>%s</color>.", args[0]);
                return PARSE_ERROR_GENERIC;
            }
            info->flags |= OBJ_DROP_STD_ART;
            info->object = art->id;
            assert(info->object == art->id);
        }
        break;

    default:
        msg_print("Error: Invalid ART() directive. Syntax: ART(<which>).");
        return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    }
    return 0;
}

errr obj_drop_parse_cmd(char *cmd, obj_drop_ptr info, int options)
{
    char *name;
    char *args[10];
    int   arg_ct = parse_args(cmd, &name, args, 10);

    if (arg_ct < 0)
    {
        msg_format("Error: Malformed argument %s. Missing )?", name);
        return PARSE_ERROR_GENERIC;
    }

    if (streq(name, "OBJ"))
    {
        return obj_drop_parse_obj(args, arg_ct, info, options);
    }
    else if (streq(name, "EGO"))
    {
        return obj_drop_parse_ego(args, arg_ct, info, options);
    }
    else if (streq(name, "ART"))
    {
        return obj_drop_parse_art(args, arg_ct, info, options);
    }
    msg_format("Error: Unkown %s directive.", name);
    return PARSE_ERROR_GENERIC;
}

/*       v--- buf
 * O:50%:OBJ(SWORD):EGO(morgul)
 * O:1d4+1:OBJ(GOLD)
 *         ^--- buf */
errr obj_drop_parse(char *buf, obj_drop_ptr info, int options)
{
    errr  result = 0;
    char *commands[10];
    int   command_ct = z_string_split(buf, commands, 10, ":");
    int   i;

    for (i = 0; i < command_ct; i++)
    {
        char *command = commands[i];
        result = obj_drop_parse_cmd(command, info, options);
        if (result) break;
    }

    return result;
}

