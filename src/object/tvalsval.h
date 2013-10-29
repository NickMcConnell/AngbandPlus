#ifndef INCLUDED_OBJECT_TVALSVAL_H
#define INCLUDED_OBJECT_TVALSVAL_H


/*** Object "tval" and "sval" codes ***/

/*
 * PS: to regenerate the inside of an sval enum, do this:
 * $ grep --context 2 I:<TVAL> object.txt | grep ^[NI] |
 *   perl -pe 'y/[a-z]\- /[A-Z]__/' | perl -pe 's/(&_|~|')//g' | cut -d: -f3 |
 *   perl -00 -pe 's/([^\n]*)\n([^\n]*)\n/\tSV_\1\ =\ \2,\n/g'
 */

/*
 * The values for the "tval" field of various objects.
 *
 * This value is the primary means by which items are sorted in the
 * player inventory, followed by "sval" and "cost".
 *
 * Note that a "BOW" with tval = 19 and sval S = 10*N+P takes a missile
 * weapon with tval = 16+N, and does (xP) damage when so combined.  This
 * fact is not actually used in the source, but it kind of interesting.
 *
 * Note that as of 2.7.8, the "item flags" apply to all items, though
 * only armor and weapons and a few other items use any of these flags.
 */

#define TV_SKELETON		1      /* Skeletons ('s') */
#define TV_BOTTLE		2      /* Empty bottles ('!') */
#define TV_JUNK			3      /* Sticks, Pottery, etc ('~') */
#define TV_SPIKE		5      /* Spikes ('~') */
#define TV_CHEST		7      /* Chests ('~') */
#define TV_SHOT			16      /* Ammo for slings */
#define TV_ARROW        17      /* Ammo for bows */
#define TV_BOLT         18      /* Ammo for x-bows */
#define TV_BOW          19      /* Slings/Bows/Xbows */
#define TV_DIGGING      20      /* Shovels/Picks */
#define TV_HAFTED       21      /* Priest Weapons */
#define TV_POLEARM      22      /* Axes and Pikes */
#define TV_SWORD        23      /* Edged Weapons */
#define TV_BOOTS        30      /* Boots */
#define TV_GLOVES       31      /* Gloves */
#define TV_HELM         32      /* Helms */
#define TV_CROWN        33      /* Crowns */
#define TV_SHIELD       34      /* Shields */
#define TV_CLOAK        35      /* Cloaks */
#define TV_SOFT_ARMOR   36      /* Soft Armor */
#define TV_HARD_ARMOR   37      /* Hard Armor */
#define TV_DRAG_ARMOR   38      /* Dragon Scale Mail */
#define TV_LITE         39      /* Lites (including Specials) */
#define TV_AMULET       40      /* Amulets (including Specials) */
#define TV_RING         45      /* Rings (including Specials) */
#define TV_STAFF        55
#define TV_WAND         65
#define TV_ROD          66
#define TV_SCROLL       70
#define TV_SPELL        71  /* Spell page */
#define TV_POTION       75
#define TV_FLASK        77
#define TV_FOOD         80
#define TV_BOOK         85 /* TODO: Book conversion */
#define TV_GOLD         100     /* Gold can only be picked up by players */



/* The "sval" codes for TV_SHOT/TV_ARROW/TV_BOLT */
#define SV_AMMO_LIGHT           0       /* pebbles */
#define SV_AMMO_NORMAL          1       /* shots, arrows, bolts */
#define SV_AMMO_HEAVY           2       /* seeker arrows and bolts */
/* define SV_AMMO_SILVER                3       */ /* silver arrows and bolts */

/* The "sval" codes for TV_BOW (note information in "sval") */
#define SV_SLING                        2       /* (x2) */
#define SV_SHORT_BOW            12      /* (x2) */
#define SV_LONG_BOW                     13      /* (x3) */
#define SV_LIGHT_XBOW           23      /* (x3) */
#define SV_HEAVY_XBOW           24      /* (x4) */
#define SV_ELVEN_BOW            34      /* (x4) */
#define SV_HOBBIT_SLING         33      /* (x3) */


/* The sval codes for TV_LITE */
#define SV_LITE_TORCH           0
#define SV_LITE_LANTERN         1
#define SV_LITE_NOLDOR          2
#define SV_LITE_GALADRIEL       4
#define SV_LITE_ELENDIL         5
#define SV_LITE_THRAIN          6

enum sval_digging /* tval 20 */
{
        SV_SHOVEL = 1,
        SV_GNOMISH_SHOVEL = 2,
        SV_DWARVEN_SHOVEL = 3,
        SV_PICK = 4,
        SV_ORCISH_PICK = 5,
        SV_DWARVEN_PIC = 6
};

enum sval_hafted /* tval 21 */
{
        SV_WHIP = 2, /* 1d6 */
        SV_QUARTERSTAFF = 3, /* 1d9 */
        SV_MACE = 4, /* 2d4 */
        SV_BALL_AND_CHAIN = 6, /* 2d4 */
        SV_WAR_HAMMER = 8, /* 3d3 */
        SV_LUCERNE_HAMMER = 10, /* 2d5 */
        SV_MORNING_STAR = 12, /* 2d6 */
        SV_FLAIL = 13, /* 2d6 */
        SV_LEAD_FILLED_MACE = 15, /* 3d4 */
        SV_TWO_HANDED_FLAIL = 18, /* 3d6 */
        SV_MACE_OF_DISRUPTION = 20,  /* 5d8 */
        SV_GROND = 50 /* 3d4 */
};

enum sval_polearm /* tval 22 */
{
        SV_SPEAR = 2, /* 1d6 */
        SV_AWL_PIKE = 4, /* 1d8 */
        SV_TRIDENT = 5, /* 1d9 */
        SV_PIKE = 8, /* 2d5 */
        SV_BEAKED_AXE = 10, /* 2d6 */
        SV_BROAD_AXE = 11, /* 2d6 */
        SV_GLAIVE = 13, /* 2d6 */
        SV_HALBERD = 15, /* 3d4 */
        SV_SCYTHE = 17, /* 5d3 */
        SV_LANCE = 20, /* 2d8 */
        SV_BATTLE_AXE = 22, /* 2d8 */
        SV_GREAT_AXE = 25, /* 4d4 */
        SV_LOCHABER_AXE = 28, /* 3d8 */
        SV_SCYTHE_OF_SLICING = 30, /* 8d4 */
};

enum sval_sword /* tval 23 */
{
        SV_BROKEN_DAGGER = 1, /* 1d1 */
        SV_BROKEN_SWORD = 2, /* 1d2 */
        SV_DAGGER = 4, /* 1d4 */
        SV_MAIN_GAUCHE = 5, /* 1d5 */
        SV_RAPIER = 7, /* 1d6 */
        SV_SMALL_SWORD = 8, /* 1d6 */
        SV_SHORT_SWORD = 10, /* 1d7 */
        SV_SABRE = 11, /* 1d7 */
        SV_CUTLASS = 12, /* 1d7 */
        SV_TULWAR = 15, /* 2d4 */
        SV_BROAD_SWORD = 16, /* 2d5 */
        SV_LONG_SWORD = 17, /* 2d5 */
        SV_SCIMITAR = 18, /* 2d5 */
        SV_KATANA = 20, /* 3d4 */
        SV_BASTARD_SWORD = 21, /* 3d4 */
        SV_TWO_HANDED_SWORD = 25, /* 3d6 */
        SV_EXECUTIONERS_SWORD = 28, /* 4d5 */
        SV_BLADE_OF_CHAOS = 30 /* 6d5 */
};

enum sval_boots /* tval 30 */
{
        SV_PAIR_OF_SOFT_LEATHER_BOOTS = 2,
        SV_PAIR_OF_HARD_LEATHER_BOOTS = 3,
        SV_PAIR_OF_METAL_SHOD_BOOTS = 6
};

enum sval_gloves /* tval 31 */
{
        SV_SET_OF_LEATHER_GLOVES = 1,
        SV_SET_OF_GAUNTLETS = 2,
        SV_SET_OF_MITHRIL_GAUNTLETS = 3,
        SV_SET_OF_CAESTUS = 4,
        SV_SET_OF_ALCHEMISTS_GLOVES = 5
};

enum sval_helm /* tval 32 */
{
        SV_HARD_LEATHER_CAP = 2,
        SV_METAL_CAP = 3,
        SV_IRON_HELM = 5,
        SV_STEEL_HELM = 6
};

enum sval_crown /* tval 33 */
{
        SV_IRON_CROWN = 10,
        SV_GOLDEN_CROWN = 11,
        SV_JEWEL_ENCRUSTED_CROWN = 12,
        SV_MORGOTH = 50
};

enum sval_shield /* tval 34 */
{
        SV_SMALL_LEATHER_SHIELD = 2,
        SV_SMALL_METAL_SHIELD = 3,
        SV_LARGE_LEATHER_SHIELD = 4,
        SV_LARGE_METAL_SHIELD = 5,
        SV_SHIELD_OF_DEFLECTION = 10
};

enum sval_cloak /* tval 35 */
{
        SV_CLOAK = 1,
        SV_FUR_CLOAK = 2,
        SV_ELVEN_CLOAK = 3,
        SV_ETHEREAL_CLOAK = 4
};

enum sval_soft_armor /* tval 36 */
{
        SV_ROBE = 2,
        SV_SOFT_LEATHER_ARMOR = 4,
        SV_STUDDED_LEATHER_ARMOR = 7,
        SV_HARD_LEATHER_ARMOR = 6,
        SV_LEATHER_SCALE_MAIL = 11
};

enum sval_hard_armor /* tval 37 */
{
        SV_METAL_SCALE_MAIL = 1,
        SV_CHAIN_MAIL = 2,
        SV_AUGMENTED_CHAIN_MAIL = 3,
        SV_BAR_CHAIN_MAIL = 4,
        SV_METAL_BRIGANDINE_ARMOR = 5,
        SV_PARTIAL_PLATE_ARMOR = 6,
        SV_METAL_LAMELLAR_ARMOR = 7,
        SV_FULL_PLATE_ARMOR = 8,
        SV_RIBBED_PLATE_ARMOR = 9,
        SV_MITHRIL_CHAIN_MAIL = 10,
        SV_MITHRIL_PLATE_MAIL = 11,
        SV_ADAMANTITE_PLATE_MAIL = 12
};

enum sval_dragon_armor /* tval 38 */
{
        SV_DRAGON_BLACK = 1,
        SV_DRAGON_BLUE  = 2,
        SV_DRAGON_WHITE = 3,
        SV_DRAGON_RED = 4,
        SV_DRAGON_GREEN = 5,
        SV_DRAGON_MULTIHUED = 6,
        SV_DRAGON_SHINING = 10,
        SV_DRAGON_LAW = 12,
        SV_DRAGON_BRONZE = 14,
        SV_DRAGON_GOLD = 16,
        SV_DRAGON_CHAOS = 18,
        SV_DRAGON_BALANCE = 20,
        SV_DRAGON_POWER = 30
};

enum sval_amulet /* tval 40 */
{
        SV_AMULET_DOOM = 0,
        SV_AMULET_TELEPORT = 1,
        SV_AMULET_ADORNMENT = 2,
        SV_AMULET_SLOW_DIGEST = 3,
        SV_AMULET_RESIST_ACID = 4,
        SV_AMULET_SEARCHING = 5,
        SV_AMULET_WISDOM = 6,
        SV_AMULET_CHARISMA = 7,
        SV_AMULET_THE_MAGI = 8,
        SV_AMULET_CARLAMMAS = 10,
        SV_AMULET_INGWE = 11,
        SV_AMULET_DWARVES = 12,
};

enum sval_ring /* tval 45 */
{
        SV_RING_WOE = 0,
        SV_RING_AGGRAVATION = 1,
        SV_RING_WEAKNESS = 2,
        SV_RING_STUPIDITY = 3,
        SV_RING_TELEPORTATION = 4,
        SV_RING_ESP= 5,
        SV_RING_SLOW_DIGESTION = 6,
        SV_RING_FEATHER_FALL = 7,
        SV_RING_RESIST_FIRE = 8,
        SV_RING_RESIST_COLD = 9,
        SV_RING_SUSTAIN_STR = 10,
        SV_RING_SUSTAIN_INT = 11,
        SV_RING_SUSTAIN_WIS = 12,
        SV_RING_SUSTAIN_DEX = 13,
        SV_RING_SUSTAIN_CON = 14,
        SV_RING_SUSTAIN_CHR = 15,
        SV_RING_PROTECTION = 16,
        SV_RING_ACID = 17,
        SV_RING_FLAMES = 18,
        SV_RING_ICE = 19,
        SV_RING_RESIST_POIS = 20,
        SV_RING_FREE_ACTION = 21,
        SV_RING_SEE_INVIS = 22,
        SV_RING_SEARCHING = 23,
        SV_RING_STR = 24,
        SV_RING_INT = 25,
        SV_RING_DEX = 26,
        SV_RING_CON = 27,
        SV_RING_ACCURACY = 28,
        SV_RING_DAMAGE = 29,
        SV_RING_SLAYING = 30,
        SV_RING_SPEED = 31,
        SV_RING_BARAHIR = 32,
        SV_RING_TULKAS = 33,
        SV_RING_NARYA = 34,
        SV_RING_NENYA = 35,
        SV_RING_VILYA = 36,
        SV_RING_POWER = 37,
        SV_RING_ELEC = 38,
        SV_RING_IRON = 42,
        SV_RING_SILVER = 43,
        SV_RING_GOLD = 44,
        SV_RING_MITHRIL = 45
};

enum sval_staff /* sval 55 */ /* N.B. This is not actually needed */
{
        SV_STAFF_DARKNESS = 0,
        SV_STAFF_SLOWNESS = 1,
        SV_STAFF_HASTE_MONSTERS = 2,
        SV_STAFF_SUMMONING = 3,
        SV_STAFF_TELEPORTATION = 4,
        SV_STAFF_IDENTIFY = 5,
        SV_STAFF_REMOVE_CURSE = 6,
        SV_STAFF_STARLITE = 7,
        SV_STAFF_LITE = 8,
        SV_STAFF_MAPPING = 9,
        SV_STAFF_DETECT_GOLD = 10,
        SV_STAFF_DETECT_ITEM = 11,
        SV_STAFF_DETECT_TRAP = 12,
        SV_STAFF_DETECT_DOOR = 13,
        SV_STAFF_DETECT_INVIS = 14,
        SV_STAFF_DETECT_EVIL = 15,
        SV_STAFF_CURE_LIGHT = 16,
        SV_STAFF_CURING = 17,
        SV_STAFF_HEALING = 18,
        SV_STAFF_THE_MAGI = 19,
        SV_STAFF_SLEEP_MONSTERS = 20,
        SV_STAFF_SLOW_MONSTERS = 21,
        SV_STAFF_SPEED = 22,
        SV_STAFF_PROBING = 23,
        SV_STAFF_DISPEL_EVIL = 24,
        SV_STAFF_POWER = 25,
        SV_STAFF_HOLINESS = 26,
        SV_STAFF_GENOCIDE = 27,
        SV_STAFF_EARTHQUAKES = 28,
        SV_STAFF_DESTRUCTION = 29
};

enum sval_wand /* sval 65 */ /* N.B. This is not actually needed */
{
        SV_WAND_HEAL_MONSTER = 0,
        SV_WAND_HASTE_MONSTER = 1,
        SV_WAND_CLONE_MONSTER = 2,
        SV_WAND_TELEPORT_AWAY = 3,
        SV_WAND_DISARMING = 4,
        SV_WAND_TRAP_DOOR_DEST = 5,
        SV_WAND_STONE_TO_MUD = 6,
        SV_WAND_LITE = 7,
        SV_WAND_SLEEP_MONSTER = 8,
        SV_WAND_SLOW_MONSTER = 9,
        SV_WAND_CONFUSE_MONSTER = 10,
        SV_WAND_FEAR_MONSTER = 11,
        SV_WAND_DRAIN_LIFE = 12,
        SV_WAND_POLYMORPH = 13,
        SV_WAND_STINKING_CLOUD = 14,
        SV_WAND_MAGIC_MISSILE = 15,
        SV_WAND_ACID_BOLT = 16,
        SV_WAND_ELEC_BOLT = 17,
        SV_WAND_FIRE_BOLT = 18,
        SV_WAND_COLD_BOLT = 19,
        SV_WAND_ACID_BALL = 20,
        SV_WAND_ELEC_BALL = 21,
        SV_WAND_FIRE_BALL = 22,
        SV_WAND_COLD_BALL = 23,
        SV_WAND_WONDER = 24,
        SV_WAND_ANNIHILATION = 25,
        SV_WAND_DRAGON_FIRE = 26,
        SV_WAND_DRAGON_COLD = 27,
        SV_WAND_DRAGON_BREATH = 28
};

enum sval_rod /* sval 66 */ /* N.B. This is not actually needed */
{
        SV_ROD_DETECT_TRAP = 0,
        SV_ROD_DETECT_DOOR = 1,
        SV_ROD_IDENTIFY = 2,
        SV_ROD_RECALL = 3,
        SV_ROD_ILLUMINATION = 4,
        SV_ROD_MAPPING = 5,
        SV_ROD_DETECTION = 6,
        SV_ROD_PROBING = 7,
        SV_ROD_CURING = 8,
        SV_ROD_HEALING = 9,
        SV_ROD_RESTORATION = 10,
        SV_ROD_SPEED = 11,
/* aimed rods */
        SV_ROD_TELEPORT_AWAY = 20,
        SV_ROD_DISARMING = 21,
        SV_ROD_LITE = 22,
        SV_ROD_SLEEP_MONSTER = 23,
        SV_ROD_SLOW_MONSTER = 24,
        SV_ROD_DRAIN_LIFE = 25,
        SV_ROD_POLYMORPH = 26,
        SV_ROD_ACID_BOLT = 27,
        SV_ROD_ELEC_BOLT = 28,
        SV_ROD_FIRE_BOLT = 29,
        SV_ROD_COLD_BOLT = 30,
        SV_ROD_ACID_BALL = 31,
        SV_ROD_ELEC_BALL = 32,
        SV_ROD_FIRE_BALL = 33,
        SV_ROD_COLD_BALL = 34,
        SV_ROD_IDENT_TRAP = 35,
/* artifact rods */
        SV_ROD_CLEARTHINKING = 45,
        SV_ROD_DREAMS = 46,
        SV_ROD_ESCAPE = 47,
        SV_ROD_PLENTY = 48,
        SV_ROD_KNOWLEDGE = 49
};

/* Hacky defines */ /* Removed for Angband/65 0.0.0 */
/* define SV_SCROLL_PHASE_DOOR                  1 */
/* define SV_SCROLL_WORD_OF_RECALL              29 */
/* define SV_SCROLL_RUNE_OF_PROTECTION  38 */

enum sval_scroll /* sval 70 */ /* N.B. This is not actually needed */
{
        SV_SCROLL_DARKNESS = 0,
        SV_SCROLL_AGGRAVATE_MONSTER = 1,
        SV_SCROLL_CURSE_ARMOR = 2,
        SV_SCROLL_CURSE_WEAPON = 3,
        SV_SCROLL_SUMMON_MONSTER = 4,
        SV_SCROLL_SUMMON_UNDEAD = 5,
        SV_SCROLL_TREASURE = 6,
        SV_SCROLL_TRAP_CREATION = 7,
        SV_SCROLL_PHASE_DOOR = 8, /* Used? */
        SV_SCROLL_TELEPORT = 9,
        SV_SCROLL_TELEPORT_LEVEL = 10,
        SV_SCROLL_WORD_OF_RECALL = 11, /* Used? */
        SV_SCROLL_IDENTIFY = 12,
        SV_SCROLL_STAR_IDENTIFY = 13,
        SV_SCROLL_REMOVE_CURSE = 14,
        SV_SCROLL_STAR_REMOVE_CURSE = 15,
        SV_SCROLL_ENCHANT_ARMOR = 16,
        SV_SCROLL_ENCHANT_WEAPON_TO_HIT = 17,
        SV_SCROLL_ENCHANT_WEAPON_TO_DAM = 18,
        SV_SCROLL_STAR_ENCHANT_ARMOR = 20,
        SV_SCROLL_STAR_ENCHANT_WEAPON = 21,
        SV_SCROLL_RECHARGING = 22,
        SV_SCROLL_LIGHT = 24,
        SV_SCROLL_MAPPING = 25,
        SV_SCROLL_DETECT_GOLD = 26,
        SV_SCROLL_DETECT_ITEM = 27,
        SV_SCROLL_DETECT_TRAP = 28,
        SV_SCROLL_DETECT_DOOR = 29,
        SV_SCROLL_DETECT_INVIS = 30,
        SV_SCROLL_SATISFY_HUNGER = 32,
        SV_SCROLL_BLESSING = 33,
        SV_SCROLL_HOLY_CHANT = 34,
        SV_SCROLL_HOLY_PRAYER = 35,
        SV_SCROLL_MONSTER_CONFUSION = 36,
        SV_SCROLL_PROTECTION_FROM_EVIL = 37,
        SV_SCROLL_RUNE_OF_PROTECTION = 38, /* Used? */
        SV_SCROLL_TRAP_DOOR_DESTRUCTION = 39,
        SV_SCROLL_STAR_DESTRUCTION = 41,
        SV_SCROLL_DISPEL_UNDEAD = 42,
        SV_SCROLL_GENOCIDE = 44,
        SV_SCROLL_MASS_GENOCIDE = 45,
        SV_SCROLL_ACQUIREMENT = 46,
        SV_SCROLL_STAR_ACQUIREMENT = 47
}; 

/*
 * Special "sval" limit -- last "normal" drink
 */
#define SV_FOOD_MAX_DRINK   3

enum sval_potion /* tval 75 */ /* This is not really needed */
{
        SV_POTION_WATER = 0,
        SV_POTION_APPLE_JUICE = 1,
        SV_POTION_SLIME_MOLD = 2,
        SV_POTION_SLOWNESS = 4,
        SV_POTION_SALT_WATER = 5,
        SV_POTION_POISON = 6,
        SV_POTION_BLINDNESS = 7,
        SV_POTION_FIRE = 8,
        SV_POTION_CONFUSION = 9,
        SV_POTION_COLD = 10,
        SV_POTION_SLEEP = 11,
        SV_POTION_ACID = 12,
        SV_POTION_LOSE_MEMORIES = 13,
        SV_POTION_ELEC = 14,
        SV_POTION_RUINATION = 15,
        SV_POTION_DEC_STR = 16,
        SV_POTION_DEC_INT = 17,
        SV_POTION_DEC_WIS = 18,
        SV_POTION_DEC_DEX = 19,
        SV_POTION_DEC_CON = 20,
        SV_POTION_DEC_CHR = 21,
        SV_POTION_DETONATIONS = 22,
        SV_POTION_DEATH = 23,
        SV_POTION_INFRAVISION = 24,
        SV_POTION_DETECT_INVIS = 25,
        SV_POTION_SLOW_POISON = 26,
        SV_POTION_CURE_POISON = 27,
        SV_POTION_BOLDNESS = 28,
        SV_POTION_SPEED = 29,
        SV_POTION_RESIST_HEAT = 30,
        SV_POTION_RESIST_COLD = 31,
        SV_POTION_HEROISM = 32,
        SV_POTION_BERSERK_STRENGTH = 33,
        SV_POTION_CURE_LIGHT = 34,
        SV_POTION_CURE_SERIOUS = 35,
        SV_POTION_CURE_CRITICAL = 36,
        SV_POTION_HEALING = 37,
        SV_POTION_STAR_HEALING = 38,
        SV_POTION_LIFE = 39,
        SV_POTION_RESTORE_MANA = 40,
        SV_POTION_RESTORE_EXP = 41,
        SV_POTION_RES_STR = 42,
        SV_POTION_RES_INT = 43,
        SV_POTION_RES_WIS = 44,
        SV_POTION_RES_DEX = 45,
        SV_POTION_RES_CON = 46,
        SV_POTION_RES_CHR = 47,
        SV_POTION_INC_STR = 48,
        SV_POTION_INC_INT = 49,
        SV_POTION_INC_WIS = 50,
        SV_POTION_INC_DEX = 51,
        SV_POTION_INC_CON = 52,
        SV_POTION_INC_CHR = 53,
        SV_POTION_AUGMENTATION = 55,
        SV_POTION_ENLIGHTENMENT = 56,
        SV_POTION_STAR_ENLIGHTENMENT = 57,
        SV_POTION_SELF_KNOWLEDGE = 58,
        SV_POTION_EXPERIENCE = 59,
        SV_POTION_LIFT = 60
};

/*
 * Special "sval" limit -- first "normal" food
 */
#define SV_FOOD_MAX_SHROOM   30

/* Angband/65 0.0.1 has more 'standard' foods than Vanilla Angband */
enum sval_food /* tval 80 */ /* TODO : Check nasty & nice effects (can sell, etc.) */
{
        SV_FOOD_POISON = 0,
        SV_FOOD_BLINDNESS = 1,
        SV_FOOD_PARANOIA = 2,
        SV_FOOD_CONFUSION = 3,
        SV_FOOD_HALLUCINATION = 4,
        SV_FOOD_PARALYSIS = 5,
        SV_FOOD_WEAKNESS = 6,
        SV_FOOD_SICKNESS = 7,
        SV_FOOD_STUPIDITY = 8,
        SV_FOOD_NAIVETY = 9,
        SV_FOOD_UNHEALTH = 10,
        SV_FOOD_DISEASE = 11,
        SV_FOOD_CURE_POISON = 12,
        SV_FOOD_CURE_BLINDNESS = 13,
        SV_FOOD_CURE_PARANOIA = 14,
        SV_FOOD_CURE_CONFUSION = 15,
        SV_FOOD_CURE_SERIOUS = 16,
        SV_FOOD_RESTORE_STR = 17,
        SV_FOOD_RESTORE_CON = 18,
        SV_FOOD_RESTORING = 19, 
        /* Leave a break for more mushrooms */
        SV_FOOD_BISCUIT = 32,
        SV_FOOD_JERKY = 33,
        SV_FOOD_RATION = 35,
        SV_FOOD_SLIME_MOLD = 36,
        SV_FOOD_WAYBREAD = 37,
        SV_FOOD_PINT_OF_ALE = 38,
        SV_FOOD_PINT_OF_WINE = 39
};


enum sval_book /* tval 85 */
{
        SV_BOOK_SMALL = 1,
        SV_BOOK_AVG = 2,
        SV_BOOK_LARGE = 3
};

enum sval_gold /* tval 100 */
{
        SV_GOLD_ANY = -1,
        SV_COPPER = 0,
        SV_SILVER = 1,
        SV_GARNETS = 2,
        SV_GOLD = 3,
        SV_OPALS = 4,
        SV_SAPPHIRES = 5,
        SV_RUBIES = 6,
        SV_DIAMONDS = 7,
        SV_EMERALDS = 8,
        SV_MITHRIL = 9,
        SV_ADAMANTITE = 10,

        SV_GOLD_MAX
};


/*
 * Special "sval" limit -- first "aimed" rod
 */
#define SV_ROD_MIN_DIRECTION    12

/*
 * Special "sval" limit -- first "large" chest
 */
#define SV_CHEST_MIN_LARGE      4

/*
 * Special "sval" limit -- first "good" magic/prayer book
 */
#define SV_BOOK_MIN_GOOD        4

/*
 * Special "sval" value -- unknown "sval"
 */
#define SV_UNKNOWN                      255





#endif /* INCLUDED_OBJECT_TVALSVAL_H */
