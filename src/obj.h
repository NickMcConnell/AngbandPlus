#ifndef INCLUDED_OBJ_H
#define INCLUDED_OBJ_H

#include "c-vec.h"

/************************************************************************
 * Dice (XdY+Z)
 * XXX Move this someplace more general
 * XXX Rethink "constructor" syntax: foo_t foo(...) feels right, but
 *     clients like to name variables as foo_t foo = foo(...)?
 *     Consider foo_make() or foo_init(). Of course, reserve foo_alloc()
 *     and foo_free() for dynamic constructors/destructors.
 ************************************************************************/
typedef struct {
    s16b dd;
    s16b ds;     
    s16b base;
    s16b scale; /* per mil scaling: e.g. 1750 means 1.75x */
} dice_t, *dice_ptr;
extern dice_t dice_create(int dd, int ds, int base);
extern int    dice_roll(dice_t dice);
extern int    dice_lo_roll(dice_t dice);
extern int    dice_avg_roll(dice_t dice);
extern int    dice_hi_roll(dice_t dice);
extern cptr   dice_format(dice_t dice);
extern errr   dice_parse(dice_ptr dice, char *token);
extern cptr   dice_info_dam(dice_t dice);
extern cptr   dice_info_dam_each(dice_t dice);
extern cptr   dice_info_heal(dice_t dice);
extern cptr   dice_info_range(dice_t dice);
extern cptr   dice_info_dur(dice_t dice);
extern cptr   dice_info_power(dice_t dice);
extern cptr   dice_info(dice_t dice, cptr heading); /* "dam " or "dur " (note trailing space!) */
extern void   dice_doc(dice_t dice, doc_ptr doc);
extern dice_t dice_load(savefile_ptr file);
extern void   dice_save(dice_t dice, savefile_ptr file);

/************************************************************************
 * Object Type
 * XXX TV_* is historical for "type value"? Anyway, we'll use tv_* as well.
 * XXX Gaps is TV_* were probably leaving room for TV_FOO_BEGIN and TV_FOO_END
 *     ranges. I've replaced that with flags.
 * XXX Adding/Changing TV_* codes requires maintaining _tv_tbl in obj.c
 ************************************************************************/
enum {
    TV_NONE = 0,
    TV_SKELETON,
    TV_BOTTLE,
    TV_JUNK,
    TV_WHISTLE,
    TV_SPIKE,
    TV_CHEST = 7,
    TV_FIGURINE,
    TV_STATUE,
    TV_CORPSE,
    TV_CAPTURE,
    TV_SHOT = 16,
    TV_ARROW,
    TV_BOLT,
    TV_BOW,
    TV_DIGGING,
    TV_HAFTED,
    TV_POLEARM,
    TV_SWORD,
    TV_BOOTS = 30,
    TV_GLOVES,
    TV_HELM,
    TV_CROWN,
    TV_SHIELD,
    TV_CLOAK,
    TV_SOFT_ARMOR,
    TV_HARD_ARMOR,
    TV_DRAG_ARMOR,
    TV_LIGHT,
    TV_AMULET,
    TV_RING = 45,
    TV_QUIVER,
    TV_CARD = 50,
    TV_STAFF = 55,
    TV_WAND = 65,
    TV_ROD,
    TV_PARCHMENT = 69,
    TV_SCROLL,
    TV_POTION = 75,
    TV_FLASK = 77,
    TV_FOOD = 80,
    TV_RUNE,
    TV_LIFE_BOOK = 90,   /* XXX realm_t.book should give the tval */
    TV_SORCERY_BOOK,
    TV_NATURE_BOOK,
    TV_CHAOS_BOOK,
    TV_DEATH_BOOK,
    TV_TRUMP_BOOK,
    TV_ARCANE_BOOK,
    TV_CRAFT_BOOK,
    TV_DAEMON_BOOK,
    TV_CRUSADE_BOOK,
    TV_NECROMANCY_BOOK,
    TV_ARMAGEDDON_BOOK,
    TV_ILLUSION_BOOK,
    TV_MUSIC_BOOK = 105,  /* XXX realm_t.book should give the tval */
    TV_HISSATSU_BOOK,
    TV_HEX_BOOK,
    TV_RAGE_BOOK,
    TV_BURGLARY_BOOK,
    TV_BLESS_BOOK,
    TV_GOLD = 127,
};
/* First, we flag the slot type for equipment. Many of these could
 * be inferred from TV_* alone, but many can not (eg BODY_ARMOR)
 * Using flags allows things to change in the future (TV_DRAGON_BOOTS)
 * as well as allowing for convenient masks for broad classification. */
#define TVF_WEAPON      0x00000001  /* melee weapons only (no longer includes bows) */
#define TVF_SHIELD      0x00000002
#define TVF_BOW         0x00000004
#define TVF_QUIVER      0x00000008
#define TVF_AMMO        0x00000010
#define TVF_RING        0x00000020
#define TVF_AMULET      0x00000040
#define TVF_LIGHT       0x00000080
#define TVF_BODY_ARMOR  0x00000100
#define TVF_CLOAK       0x00000200
#define TVF_HELMET      0x00000400
#define TVF_GLOVES      0x00000800
#define TVF_BOOTS       0x00001000

/* Masks */
#define TVF_ARMOR (TVF_SHIELD | TVF_BODY_ARMOR | TVF_CLOAK | TVF_HELMET | TVF_GLOVES | TVF_BOOTS)
#define TVF_JEWELRY (TVF_RING | TVF_AMULET)
#define TVF_WEARABLE (TVF_WEAPON | TVF_BOW | TVF_QUIVER | TVF_JEWELRY | TVF_LIGHT | TVF_ARMOR)
#define TVF_ENCHANTABLE (TVF_WEAPON | TVF_BOW | TVF_AMMO | TVF_ARMOR)
#define TVF_EQUIPMENT (TVF_WEARABLE | TVF_AMMO)  /* XXX autopicker */

/* Next, we flag for other broad types of objects */
#define TVF_DEVICE      0x00002000
#define TVF_SPELLBOOK   0x00004000
#define TVF_POTION      0x00008000
#define TVF_SCROLL      0x00010000
#define TVF_JUNK        0x00020000
#define TVF_MISC        0x00040000
#define TVF_FOOD        0x00080000  /* XXX give mushrooms TV_MUSHROOM? */

/* Finally, some additional info */
#define TVF_STACKABLE   0x00100000  /* tv_info_t.stack and obj_kind_t.stack determine max */
#define TVF_HATES_ACID  0x00200000
#define TVF_HATES_ELEC  0x00400000
#define TVF_HATES_FIRE  0x00800000
#define TVF_HATES_COLD  0x01000000

typedef struct {
    int  id;
    cptr name;
    byte color;
    cptr parse[3];  /* private for tf_parse_name; allow aliases ("SWORD", "sword", "|") */
    int  flags;
    int  stack;     /* default stack size if TVF_STACKABLE */
    int  sort;      /* XXX we sort inv by tval order ... consider replacing? */
} tv_info_t, *tv_info_ptr;

extern tv_info_ptr tv_parse_name(cptr token);
extern tv_info_ptr tv_lookup(int id);
extern byte        tv_color(int id);

extern bool tv_is_weapon(int id);
extern bool tv_is_shield(int id);
extern bool tv_is_bow(int id);
extern bool tv_is_quiver(int id);
extern bool tv_is_ammo(int id);
extern bool tv_is_ring(int id);
extern bool tv_is_amulet(int id);
extern bool tv_is_light(int id);
extern bool tv_is_body_armor(int id);
extern bool tv_is_cloak(int id);
extern bool tv_is_helmet(int id);
extern bool tv_is_gloves(int id);
extern bool tv_is_boots(int id);
extern bool tv_is_armor(int id);
extern bool tv_is_jewelry(int id);
extern bool tv_is_wearable(int id);
extern bool tv_is_enchantable(int id);
extern bool tv_is_equipment(int id);
extern bool tv_is_device(int id);
extern bool tv_is_spellbook(int id);
extern bool tv_is_potion(int id);
extern bool tv_is_scroll(int id);
extern bool tv_is_junk(int id);
extern bool tv_is_misc(int id);
extern bool tv_is_food(int id);
extern bool tv_is_stackable(int id);
extern bool tv_is_weapon_ammo(int id);
extern bool tv_is_bow_weapon_ammo(int id);

extern vec_ptr tv_lookup_(int flag);

/* XXX weapon_exp[tval - TV_WEAPON_BEGIN][sval] for skill code ... sigh */
#define TV_WEAPON_BEGIN TV_BOW
#define TV_WEAPON_END   TV_SWORD

/************************************************************************
 * Object Kind
 ************************************************************************/
extern bool obj_kind_is_weapon(int k_idx);
extern bool obj_kind_is_shield(int k_idx);
extern bool obj_kind_is_bow(int k_idx);
extern bool obj_kind_is_quiver(int k_idx);
extern bool obj_kind_is_ammo(int k_idx);
extern bool obj_kind_is_ring(int k_idx);
extern bool obj_kind_is_amulet(int k_idx);
extern bool obj_kind_is_light(int k_idx);
extern bool obj_kind_is_body_armor(int k_idx);
extern bool obj_kind_is_cloak(int k_idx);
extern bool obj_kind_is_helmet(int k_idx);
extern bool obj_kind_is_gloves(int k_idx);
extern bool obj_kind_is_boots(int k_idx);
extern bool obj_kind_is_armor(int k_idx);
extern bool obj_kind_is_jewelry(int k_idx);
extern bool obj_kind_is_wearable(int k_idx);
extern bool obj_kind_is_enchantable(int k_idx);
extern bool obj_kind_is_equipment(int k_idx);
extern bool obj_kind_is_device(int k_idx);
extern bool obj_kind_is_spellbook(int k_idx);
extern bool obj_kind_is_potion(int k_idx);
extern bool obj_kind_is_scroll(int k_idx);
extern bool obj_kind_is_junk(int k_idx);
extern bool obj_kind_is_misc(int k_idx);
extern bool obj_kind_is_food(int k_idx);
extern bool obj_kind_is_stackable(int k_idx);
extern bool obj_kind_is_weapon_ammo(int k_idx);
extern bool obj_kind_is_bow_weapon_ammo(int k_idx);

extern bool obj_kind_is_(int k_idx, int tv, int sv);

/************************************************************************
 * Object (obj_)
 ************************************************************************/
struct object_type;

/* Creation */
extern obj_ptr obj_alloc(void);
extern obj_ptr obj_split(obj_ptr obj, int amt);
extern obj_ptr obj_copy(obj_ptr obj);
extern void    obj_free(obj_ptr obj);

extern term_char_t obj_display_char(obj_ptr obj);

#define OBJ_RELEASE_QUIET       0x0001
#define OBJ_RELEASE_ENCHANT     0x0002
#define OBJ_RELEASE_ID          0x0004
#define OBJ_RELEASE_DELAYED_MSG 0x0008
extern void    obj_release(obj_ptr obj, int options);
extern void    gear_notice_id(obj_ptr obj);
extern void    gear_notice_enchant(obj_ptr obj);
extern void    obj_make_pile(obj_ptr obj);

/* Commands (Top Level User Interface) */
extern void obj_destroy_ui(void);
extern void obj_inscribe_ui(void);
extern void obj_uninscribe_ui(void);
extern void obj_inspect_ui(void);
extern void gear_ui(int which);

extern void obj_destroy(obj_ptr obj, int amt);
extern void obj_drop(obj_ptr obj, int amt);
extern void obj_drop_at(obj_ptr obj, int amt, int x, int y, int break_chance);
extern void obj_describe_charges(obj_ptr obj);

/* Predicates */
extern bool obj_exists(obj_ptr obj);
extern bool obj_is_deleted(obj_ptr obj);
extern bool obj_is_(obj_ptr obj, int tv, int sv);
extern bool obj_has_flag(obj_ptr obj, int which);
extern bool obj_has_known_flag(obj_ptr obj, int which);

extern bool obj_is_weapon(obj_ptr obj);
extern bool obj_is_shield(obj_ptr obj);
extern bool obj_is_bow(obj_ptr obj);
extern bool obj_is_quiver(obj_ptr obj);
extern bool obj_is_ammo(obj_ptr obj);
extern bool obj_is_ring(obj_ptr obj);
extern bool obj_is_amulet(obj_ptr obj);
extern bool obj_is_light(obj_ptr obj);
extern bool obj_is_body_armor(obj_ptr obj);
extern bool obj_is_centaur_armor(obj_ptr obj);
extern bool obj_is_cloak(obj_ptr obj);
extern bool obj_is_helmet(obj_ptr obj);
extern bool obj_is_gloves(obj_ptr obj);
extern bool obj_is_boots(obj_ptr obj);
extern bool obj_is_armor(obj_ptr obj);
extern bool obj_is_jewelry(obj_ptr obj);
extern bool obj_is_wearable(obj_ptr obj);
extern bool obj_is_equipment(obj_ptr obj);
extern bool obj_is_device(obj_ptr obj);
extern bool obj_is_spellbook(obj_ptr obj);
extern bool obj_is_potion(obj_ptr obj);
extern bool obj_is_scroll(obj_ptr obj);
extern bool obj_is_junk(obj_ptr obj);
extern bool obj_is_misc(obj_ptr obj);
extern bool obj_is_food(obj_ptr obj);
extern bool obj_is_weapon_ammo(obj_ptr obj);
extern bool obj_is_bow_weapon_ammo(obj_ptr obj);
extern bool obj_is_gold(obj_ptr obj);
extern bool obj_is_not_gold(obj_ptr obj);

extern bool obj_is_readable_book(obj_ptr obj);
extern bool obj_can_shoot(obj_ptr obj);
extern bool obj_is_rod(obj_ptr obj);
extern bool obj_is_staff(obj_ptr obj);
extern bool obj_is_wand(obj_ptr obj);
extern bool obj_is_dragon_armor(obj_ptr obj);

extern bool obj_is_known(obj_ptr obj);
extern bool obj_is_unknown(obj_ptr obj);
extern bool obj_can_sense1(obj_ptr obj);
extern bool obj_can_sense2(obj_ptr obj);
extern bool obj_is_art(obj_ptr obj);
extern bool obj_is_std_art(obj_ptr obj);
extern bool obj_is_rand_art(obj_ptr obj);
extern bool obj_is_specified_art(obj_ptr obj, cptr which);
extern bool obj_is_ego(obj_ptr obj);
extern bool obj_is_cursed(obj_ptr obj);
extern bool obj_is_broken(obj_ptr obj);
extern bool obj_is_blessed(obj_ptr obj);
extern bool obj_is_found(obj_ptr obj);
extern bool obj_is_inscribed(obj_ptr obj);

/* Helpers */
extern mon_race_ptr corpse_race(obj_ptr corpse);
extern bool         corpse_race_is_char(obj_ptr corpse, char c);
extern bool         corpse_race_is_char_ex(obj_ptr corpse, cptr s);

extern bool    obj_is_chest(obj_ptr obj);
extern bool    chest_has_trap(obj_ptr chest);
extern bool    chest_has_known_trap(obj_ptr chest);
extern bool    chest_is_empty(obj_ptr chest);
extern bool    chest_is_not_empty(obj_ptr chest);
extern bool    chest_is_locked(obj_ptr chest);
extern int     chest_open(obj_ptr chest);
extern int     chest_disarm(obj_ptr chest);
extern int     chest_unlock(obj_ptr chest);


/* Sorting */
extern void obj_clear_scratch(obj_ptr obj); /* Call before sorting ... scratch is used to memoize obj_value */
extern int  obj_cmp(obj_ptr left, obj_ptr right);

/* Menus: These helpers handle the inscription hacks (@mw !* !q) */
extern char obj_label(obj_ptr obj);
extern bool obj_confirm_choice(obj_ptr obj);

/* Stacking */
extern int  obj_stack_max(obj_ptr obj);
extern bool obj_can_combine(obj_ptr dest, obj_ptr obj, int options);
extern int  obj_combine(obj_ptr dest, obj_ptr obj, int options);
extern void obj_delayed_describe(obj_ptr obj);

/* Helpers */
extern void obj_clear_dun_info(obj_ptr obj);

/* Savefiles */
extern void obj_load(obj_ptr obj, savefile_ptr file);
extern void obj_save(obj_ptr obj, savefile_ptr file);

/*
 * Object Flags (OF_*)
 *
 * Do not delete or reorder, and only add new flags to the end unless
 * you are ready to break savefiles. If you reorder, update _of_tbl to
 * match since of_lookup(id) is optimized for O(1) rather than O(N) lookup.
 *
 * We no longer support negative pvals, but instead now have various OF_DEC_*
 * flags. I find that mixing good attributes with bad ones provides for more
 * interesting cursed objects, as well as a few noncursed egos and artifacts.
 *
 */
enum obj_flags_e {
    OF_INVALID = -1,   /* 0 is being used, and I'd hate to waste a bit :) */

    /* Flavor/Description */
    OF_HIDE_TYPE = 0,  /* By design, the 0 flag is useless. cf The Weaponsmith and object_type.xtra3 */
    OF_SHOW_MODS,
    OF_FULL_NAME,
    OF_FIXED_FLAVOR,
    OF_FAKE,

    /* Stats (Code often assumes order here: e.g. OF_STR + loop_variable) */
    OF_STR,      OF_INT,      OF_WIS,      OF_DEX,      OF_CON,      OF_CHR,
    OF_DEC_STR,  OF_DEC_INT,  OF_DEC_WIS,  OF_DEC_DEX,  OF_DEC_CON,  OF_DEC_CHR,
    OF_SUST_STR, OF_SUST_INT, OF_SUST_WIS, OF_SUST_DEX, OF_SUST_CON, OF_SUST_CHR,

    /* Skills/Bonuses */
    OF_SPEED,
    OF_STEALTH,
    OF_SEARCH,
    OF_INFRA,
    OF_TUNNEL,
    OF_MAGIC_MASTERY,
    OF_MAGIC_RESISTANCE,
    OF_SPELL_POWER,
    OF_SPELL_CAP,
    OF_DEVICE_POWER,
    OF_LIFE,

    OF_DEC_SPEED,
    OF_DEC_STEALTH,
    OF_DEC_MAGIC_MASTERY,
    OF_DEC_SPELL_POWER,
    OF_DEC_SPELL_CAP,
    OF_DEC_LIFE,

    /* Resists ... we waste a few bits here for convenience */
    OF_RES_MIN,
    OF_RES_MAX = OF_RES_MIN + 31,
    OF_IM_MIN,
    OF_IM_MAX = OF_IM_MIN + 31,
    OF_VULN_MIN,
    OF_VULN_MAX = OF_VULN_MIN + 31,

    /* Abilities */
    OF_FREE_ACT,
    OF_SEE_INVIS,
    OF_REGEN,
    OF_HOLD_LIFE,
    OF_REFLECT,
    OF_LEVITATION,
    OF_SLOW_DIGEST,
    OF_WARNING,
    OF_NO_MAGIC,
    OF_NO_SUMMON,
    OF_NO_TELE,
    OF_NO_ENCHANT,
    OF_NO_REMOVE,
    OF_EASY_SPELL,
    OF_DEC_MANA,
    OF_LIGHT,
    OF_DARKNESS,
    OF_LORE1,
    OF_LORE2,

    OF_ACTIVATE, /* Present, but not required to Activate (obj_has_effect() suffices).
                    This is a very useful crutch for object lore, though. */

    OF_IGNORE_ACID, OF_IGNORE_ELEC, OF_IGNORE_FIRE, OF_IGNORE_COLD,

    /* Auras */
    OF_AURA_ELEC, OF_AURA_FIRE, OF_AURA_COLD,
    OF_AURA_SHARDS, OF_AURA_REVENGE, OF_AURA_FEAR,

    /* Telepathy */
    OF_TELEPATHY,
    OF_ESP_EVIL,
    OF_ESP_GOOD,
    OF_ESP_NONLIVING,
    OF_ESP_UNIQUE,
    OF_ESP_DRAGON,
    OF_ESP_DEMON,
    OF_ESP_UNDEAD,
    OF_ESP_ANIMAL,
    OF_ESP_HUMAN,
    OF_ESP_ORC,
    OF_ESP_TROLL,
    OF_ESP_GIANT,

    /* Weapons */
    OF_SLAY_EVIL,
    OF_SLAY_GOOD,
    OF_SLAY_LIVING,
    OF_SLAY_DRAGON,
    OF_SLAY_DEMON,
    OF_SLAY_UNDEAD,
    OF_SLAY_ANIMAL,
    OF_SLAY_HUMAN,
    OF_SLAY_ORC,
    OF_SLAY_TROLL,
    OF_SLAY_GIANT,

    OF_KILL_EVIL,
    OF_KILL_DRAGON,
    OF_KILL_DEMON,
    OF_KILL_UNDEAD,
    OF_KILL_ANIMAL,
    OF_KILL_HUMAN,
    OF_KILL_ORC,
    OF_KILL_TROLL,
    OF_KILL_GIANT,

    OF_BRAND_ACID,
    OF_BRAND_ELEC,
    OF_BRAND_FIRE,
    OF_BRAND_COLD,
    OF_BRAND_POIS,
    OF_BRAND_CHAOS,
    OF_BRAND_VAMP,
    OF_BRAND_MANA,
    OF_BRAND_LIGHT,
    OF_BRAND_DARK,
    OF_BRAND_TIME,
    OF_BRAND_PLASMA,
    OF_VORPAL,
    OF_VORPAL2,
    OF_IMPACT,
    OF_STUN,
    OF_CRIT,

    OF_BLESSED,
    OF_RIDING,
    OF_THROWING,

    OF_BLOWS,
    OF_DEC_BLOWS,
    OF_WEAPONMASTERY,
    OF_DUAL_WIELDING,

    /* Bows */
    OF_XTRA_MIGHT,
    OF_XTRA_SHOTS,

    /* Curses */
    OF_DRAIN_EXP,
    OF_TELEPORT,
    OF_AGGRAVATE,
    OF_TY_CURSE,

    OF_SPELL_DAM, /* (xxxxx, +to_d) applies to spell damage */
    OF_ARCHERY,   /* (+to_h, +to_d) applies to archery */
    OF_MELEE,     /* (+to_h, +to_d) applies to melee */

    /* New Flags ... Move to proper location next time savefiles break */

    /* A few places loop from 0 <= i < OF_COUNT ... (init1, race_sword and race_ring) */
    OF_COUNT,
};
#define OF_RES_(A)  (OF_RES_MIN + (A))
#define OF_IM_(A)   (OF_IM_MIN + (A))
#define OF_VULN_(A) (OF_VULN_MIN + (A))
#define OF_ARRAY_SIZE          8

/* Object Flag Types (OFT_*) */
#define OFT_PVAL      0x0001 /* Flag requires and uses a pval */
#define OFT_DEC       0x0002 /* Flag decrements rather than increments. Usually OFT_PVAL as well. */
#define OFT_LORE      0x0008 /* Flag is learned by simply equipping the object */

#define OFT_STAT      0x0010 /* Categories for wizard reporting. Not really flags ... only have 1! */
#define OFT_BONUS     0x0020
#define OFT_RESIST    0x0040
#define OFT_ABILITY   0x0080
#define OFT_ESP       0x0100
#define OFT_SLAY      0x0200
#define OFT_BRAND     0x0400
#define OFT_CURSE     0x0800
#define OFT_OTHER     0x1000

typedef struct {
    int  id;
    cptr name;
    byte color;
    cptr parse;
    int  flags;
} of_info_t, *of_info_ptr;
typedef bool (*of_info_p)(of_info_ptr info);

extern of_info_ptr of_parse_name(cptr token);
extern of_info_ptr of_lookup(int id);
extern vec_ptr     of_lookup_pval(void);
extern vec_ptr     of_lookup_stat(void);
extern vec_ptr     of_lookup_bonus(void);
extern vec_ptr     of_lookup_resist(void);
extern vec_ptr     of_lookup_ability(void);
extern vec_ptr     of_lookup_esp(void);
extern vec_ptr     of_lookup_slay(void);
extern vec_ptr     of_lookup_brand(void);
extern vec_ptr     of_lookup_curse(void);
extern vec_ptr     of_lookup_other(void);
extern vec_ptr     of_lookup_filter(of_info_p p);
extern vec_ptr     of_info(u32b flgs[OF_ARRAY_SIZE]);
extern vec_ptr     of_filter(u32b flgs[OF_ARRAY_SIZE], of_info_p p);
extern bool        of_has_pval(u32b flgs[OF_ARRAY_SIZE]);
extern bool        of_has_nonspeed_pval(u32b flgs[OF_ARRAY_SIZE]);
extern bool        of_has(u32b flgs[OF_ARRAY_SIZE], of_info_p p);

/************************************************************************
 * Object Drops
 ************************************************************************/
/* Object Drop Info
 * Used for monster drops (../lib/edit/r_info.txt) as well as
 * quest scripts (e.g. ../lib/edit/q_old_castle.txt) and rooms
 * (../lib/edit/v_info.txt).
 * See mon_drop_t as well in mon.h. */
typedef struct {
    s32b object;/* k_idx, a_idx, or kind info, depending on flags */
    u32b flags; /* AM_* flags or OBJ_DROP_* extensions defined below */
    s16b extra; /* e_idx or effect_id depending on flags */
    byte boost; /* level boost for this drop. Implies AM_GOOD if nonzero */
} obj_drop_t, *obj_drop_ptr;
/* Extra flags extend AM_* flags */
#define OBJ_DROP_STD_ART   0x01000000 /* object field is the a_idx */
#define OBJ_DROP_RAND_ART  0x02000000 /* force AM_SPECIAL | AM_NO_FIXED_ART */
#define OBJ_DROP_STD_EGO   0x04000000 /* extra field is the e_idx */
#define OBJ_DROP_RAND_EGO  0x08000000 /* force AM_GREAT | AM_NO_FIXED_ART */
#define OBJ_DROP_TYPE      0x10000000 /* object field is either a TV_* or a special code */
#define OBJ_DROP_EFFECT    0x20000000 /* for devices, extra field is EFFECT_* code */
#define OBJ_DROP_RANDOM    0x40000000 /* OBJ(*), ART(*), EGO(*) ... don't care what kind. */
#define OBJ_DROP_SOME_GOLD 0x80000000 /* Historical: 20% gold; 80% objects. Combined with OBJ_DROP_RANDOM */
#define OBJ_DROP_COIN      0x00800000 /* Force a specific coin type for Creeping Coins */
#define OBJ_DROP_MON       0x00400000 /* A monster did this, rather than a quest or vault */
/* XXX Comment on OBJ_DROP_SOME_GOLD
 * The old system used flags for dice, so there was just a
 * single drop rule. By default, you got 80% objects and 20%
 * gold from the rule, but you could override this with
 * RF1_ONLY_ITEM (100% object) and RF1_ONLY_GOLD (100% gold).
 * When converting r_info, I reproduced this using OBJ(*$)
 * directives, but I'd rather have a separate rule for gold. */

enum obj_types_e                      /* OBJ(DEVICE), etc */
{
    OBJ_TYPE_TVAL_MAX = 255,
    OBJ_TYPE_DEVICE,
    OBJ_TYPE_JEWELRY,
    OBJ_TYPE_BOOK,
    OBJ_TYPE_HI_BOOK,
    OBJ_TYPE_BODY_ARMOR,
    OBJ_TYPE_OTHER_ARMOR,
    OBJ_TYPE_WEAPON,
    OBJ_TYPE_BOW_AMMO,
    OBJ_TYPE_MISC,
};

/* Make an object at indicated level using extra AM_* flags like AM_UNIQUE or AM_VAULT if desired.
 * This routine can fail returning NULL. Be sure to obj_free the result. Standard artifacts will
 * be marked as generated. */
extern obj_ptr obj_drop_make(obj_drop_ptr info, int level, int mode);

/* Parsing routines. Some out of date documentation can be found in ../lib/edit/v_info.txt. 
 * Vaults and quests support non-object directives so need lower level api help. r_info can
 * just use the top level parser to consume the rest of the current O: line */
extern errr    obj_drop_parse_obj(char **args, int arg_ct, obj_drop_ptr info, int options); /* Just OBJ(...) */
extern errr    obj_drop_parse_ego(char **args, int arg_ct, obj_drop_ptr info, int options); /* Just EGO(...) */
extern errr    obj_drop_parse_art(char **args, int arg_ct, obj_drop_ptr info, int options); /* Just ART(...) */
extern errr    obj_drop_parse_cmd(char *cmd, obj_drop_ptr info, int options);
extern errr    obj_drop_parse(char *buf, obj_drop_ptr info, int options);

#endif
