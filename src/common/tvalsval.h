/*
 * File: tvalsval.h
 * Purpose: Object "tval" and "sval" codes
 */


#ifndef INCLUDED_TVALSVAL_H
#define INCLUDED_TVALSVAL_H


/*** Object "tval" and "sval" codes ***/


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
#define TV_NULL         0
#define TV_SKELETON     1
#define TV_BOTTLE       2
#define TV_STONE        3
#define TV_CORPSE       4
#define TV_SPIKE        5
#define TV_CHEST        7
#define TV_HORN         14
#define TV_ROCK         15
#define TV_SHOT         16
#define TV_ARROW        17
#define TV_BOLT         18
#define TV_BOW          19
#define TV_DIGGING      20
#define TV_HAFTED       21
#define TV_POLEARM      22
#define TV_SWORD        23
#define TV_MSTAFF       24
#define TV_BOOTS        30
#define TV_GLOVES       31
#define TV_HELM         32
#define TV_CROWN        33
#define TV_SHIELD       34
#define TV_CLOAK        35
#define TV_SOFT_ARMOR   36
#define TV_HARD_ARMOR   37
#define TV_DRAG_ARMOR   38
#define TV_LIGHT        39
#define TV_AMULET       40
#define TV_RING         45
#define TV_STAFF        55
#define TV_WAND         65
#define TV_ROD          66
#define TV_SCROLL       70
#define TV_POTION       75
#define TV_FLASK        77
#define TV_FOOD         80
#define TV_CROP         81
#define TV_MAGIC_BOOK   90
#define TV_PRAYER_BOOK  91
#define TV_SORCERY_BOOK 92
#define TV_SHADOW_BOOK  93
#define TV_HUNT_BOOK    94
#define TV_PSI_BOOK     95
#define TV_DEATH_BOOK   96
#define TV_ELEM_BOOK    97
#define TV_SUMMON_BOOK  98
#define TV_GOLD         100
#define TV_MAX          101

#define is_food(T) (((T)->tval == TV_FOOD) || ((T)->tval == TV_CROP))


/* The "sval" codes for TV_SKELETON */
#define SV_SKELETON_CANINE  3
#define SV_SKELETON_RODENT  4
#define SV_SKELETON_ELF     5
#define SV_SKELETON_KOBOLD  6
#define SV_SKELETON_ORC     7
#define SV_SKELETON_HUMAN   8
#define SV_SKELETON_SKULL   9
#define SV_SKELETON_TROLL   10
#define SV_SKELETON_ETTIN   11


/* The "sval" codes for TV_CORPSE */
#define SV_CORPSE_HUMAN 0
#define SV_CORPSE_OTHER 1


/* The "sval" codes for TV_ROCK/TV_SHOT/TV_ARROW/TV_BOLT */
#define SV_AMMO_LIGHT   0
#define SV_AMMO_NORMAL  1
#define SV_AMMO_HEAVY   2
#define SV_AMMO_MITHRIL 3
#define SV_AMMO_MAGIC   4


/* The "sval" codes for TV_BOW (note information in "sval") */
#define SV_SLING        2
#define SV_SHORT_BOW    12
#define SV_LONG_BOW     13
#define SV_LIGHT_XBOW   23
#define SV_HEAVY_XBOW   24


enum sval_digging /* tval 20 */
{
    SV_SHOVEL = 1,
    SV_PICK = 2,
    SV_MATTOCK = 3
};


enum sval_hafted /* tval 21 */
{
    SV_WHIP = 1,
    SV_BALL_AND_CHAIN = 2,
    SV_MORNING_STAR = 3,
    SV_FLAIL = 4,
    SV_TWO_HANDED_GREAT_FLAIL = 5,
    SV_MACE = 10,
    SV_LEAD_FILLED_MACE = 11,
    SV_QUARTERSTAFF = 12,
    SV_WAR_HAMMER = 13,
    SV_MAUL = 14,
    SV_GREAT_HAMMER = 15,
    SV_MACE_OF_DISRUPTION = 20,

    /* Artifacts */
    SV_GROND = 50
};


enum sval_polearm /* tval 22 */
{
    SV_SPEAR = 1,
    SV_AWL_PIKE = 2,
    SV_TRIDENT = 3,
    SV_HALBERD = 4,
    SV_PIKE = 5,
    SV_BEAKED_AXE = 10,
    SV_BROAD_AXE = 11,
    SV_BATTLE_AXE = 12,
    SV_LOCHABER_AXE = 13,
    SV_GREAT_AXE = 14,
    SV_SCYTHE = 20,
    SV_GLAIVE = 21,
    SV_LANCE = 22,
    SV_SCYTHE_OF_SLICING = 23,
    SV_LUCERNE_HAMMER = 24
};


enum sval_sword /* tval 23 */
{
    SV_DAGGER = 1,
    SV_MAIN_GAUCHE = 2,
    SV_RAPIER = 3,
    SV_SHORT_SWORD = 4,
    SV_CUTLASS = 5,
    SV_TULWAR = 6,
    SV_SCIMITAR = 7,
    SV_LONG_SWORD = 8,
    SV_BROAD_SWORD = 9,
    SV_BASTARD_SWORD = 10,
    SV_KATANA = 11,
    SV_ZWEIHANDER = 12,
    SV_EXECUTIONERS_SWORD = 13,
    SV_BLADE_OF_CHAOS = 14,
    SV_DARK_SWORD = 15
};


enum sval_boots /* tval 30 */
{
    SV_PAIR_OF_LEATHER_SANDALS = 1,
    SV_PAIR_OF_LEATHER_BOOTS = 2,
    SV_PAIR_OF_IRON_SHOD_BOOTS = 3,
    SV_PAIR_OF_STEEL_SHOD_BOOTS = 4,
    SV_PAIR_OF_MITHRIL_SHOD_BOOTS = 5,
    SV_PAIR_OF_ETHEREAL_SLIPPERS = 6,
    SV_PAIR_OF_WITAN_BOOTS = 7
};


enum sval_gloves /* tval 31 */
{
    SV_SET_OF_LEATHER_GLOVES = 1,
    SV_SET_OF_GAUNTLETS = 2,
    SV_SET_OF_MITHRIL_GAUNTLETS = 3,
    SV_SET_OF_CAESTUS = 4,
    SV_SET_OF_ALCHEMISTS_GLOVES = 5,
    SV_SET_OF_ELVEN_GLOVES = 7
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

    /* Artifacts */
    SV_MORGOTH = 50
};


enum sval_shield /* tval 34 */
{
    SV_WICKER_SHIELD = 2,
    SV_SMALL_METAL_SHIELD = 3,
    SV_LEATHER_SHIELD = 4,
    SV_LARGE_METAL_SHIELD = 5,
    SV_MITHRIL_SHIELD = 10,
    SV_ORCISH_SHIELD = 11
};


enum sval_cloak /* tval 35 */
{
    SV_CLOAK = 1,
    SV_FUR_CLOAK = 2,
    SV_ELVEN_CLOAK = 3,
    SV_ETHEREAL_CLOAK = 4,
    SV_KOLLA = 5
};


enum sval_soft_armour /* tval 36 */
{
    SV_ROBE = 2,
    SV_SOFT_LEATHER_ARMOUR = 4,
    SV_STUDDED_LEATHER_ARMOUR = 7,
    SV_HARD_LEATHER_ARMOUR = 6,
    SV_LEATHER_SCALE_MAIL = 11
};

enum sval_hard_armour /* tval 37 */
{
    SV_METAL_SCALE_MAIL = 1,
    SV_CHAIN_MAIL = 2,
    SV_AUGMENTED_CHAIN_MAIL = 3,
    SV_BAR_CHAIN_MAIL = 4,
    SV_METAL_BRIGANDINE_ARMOUR = 5,
    SV_PARTIAL_PLATE_ARMOUR = 6,
    SV_METAL_LAMELLAR_ARMOUR = 7,
    SV_FULL_PLATE_ARMOUR = 8,
    SV_RIBBED_PLATE_ARMOUR = 9,
    SV_MITHRIL_CHAIN_MAIL = 10,
    SV_MITHRIL_PLATE_MAIL = 11,
    SV_ADAMANTITE_PLATE_MAIL = 12
};


enum sval_dragon_armour /* tval 38 */
{
    SV_DRAGON_BLACK = 1,
    SV_DRAGON_BLUE  = 2,
    SV_DRAGON_WHITE = 3,
    SV_DRAGON_RED = 4,
    SV_DRAGON_GREEN = 5,
    SV_DRAGON_MULTIHUED = 6,
    SV_DRAGON_SHINING = 10,
    SV_DRAGON_LAW = 12,
    SV_DRAGON_GOLD = 16,
    SV_DRAGON_CHAOS = 18,
    SV_DRAGON_BALANCE = 20,
    SV_DRAGON_POWER = 30,
    SV_DRAGON_CRYSTAL = 40,
    SV_DRAGON_SILVER = 41,
    SV_DRAGON_SHADOW = 42,
    SV_DRAGON_DLISK = 43,
    SV_DRAGON_WATER = 44
};


/* The sval codes for TV_LIGHT */
#define SV_LIGHT_TORCH      0
#define SV_LIGHT_LANTERN    1
#define SV_LIGHT_LAMP       2
#define SV_LIGHT_GALADRIEL  4
#define SV_LIGHT_ELENDIL    5
#define SV_LIGHT_THRAIN     6

#define is_lamp(T) \
    (((T)->sval == SV_LIGHT_LANTERN) || ((T)->sval == SV_LIGHT_LAMP))


enum sval_amulet /* tval 40 */
{
    SV_AMULET_WISDOM = 1,
    SV_AMULET_CHARISMA = 2,
    SV_AMULET_RESIST_LIGHTNING = 3,
    SV_AMULET_RESIST_ACID = 4,
    SV_AMULET_RESISTANCE = 5,
    SV_AMULET_SUSTENANCE = 6,
    SV_AMULET_THE_MAGI = 7,
    SV_AMULET_ESP = 8,
    SV_AMULET_DEVOTION = 9,
    SV_AMULET_WEAPONMASTERY = 10,
    SV_AMULET_TRICKERY = 11,
    SV_AMULET_REGENERATION = 15,
    SV_AMULET_INFRAVISION = 16,
    SV_AMULET_SEARCHING = 17,
    SV_AMULET_TELEPORTATION = 18,
    SV_AMULET_SLOW_DIGESTION = 19,
    SV_AMULET_ADORNMENT = 20,
    SV_AMULET_INERTIA = 21,
    SV_AMULET_THE_MOON = 22,
    SV_AMULET_TERKEN = 23,
    SV_AMULET_SPEED = 24,

    /* Artifacts */
    SV_AMULET_CARLAMMAS = 50,
    SV_AMULET_INGWE = 51,
    SV_AMULET_DWARVES = 52,
    SV_AMULET_ELESSAR = 53,
    SV_AMULET_EVENSTAR = 54,
    SV_AMULET_TORIS = 55,
    SV_AMULET_SILMARIL = 56
};


enum sval_ring /* tval 45 */
{
    SV_RING_STRENGTH = 1,
    SV_RING_INTELLIGENCE = 2,
    SV_RING_DEXTERITY = 3,
    SV_RING_CONSTITUTION = 4,
    SV_RING_SPEED = 5,
    SV_RING_SEARCHING = 6,
    SV_RING_BODYKEEPING = 9,
    SV_RING_SOULKEEPING = 10,
    SV_RING_RESIST_POISON = 12,
    SV_RING_RESIST_FIRE = 13,
    SV_RING_RESIST_COLD = 14,
    SV_RING_LIGHT = 16,
    SV_RING_FLAMES = 18,
    SV_RING_ACID = 19,
    SV_RING_ICE = 20,
    SV_RING_LIGHTNING = 21,
    SV_RING_DAMAGE = 23,
    SV_RING_ACCURACY = 24,
    SV_RING_SLAYING = 25,
    SV_RING_PROTECTION = 26,
    SV_RING_TELEPORTATION = 27,
    SV_RING_RECKLESS_ATTACKS = 28,
    SV_RING_OPEN_WOUNDS = 29,
    SV_RING_OF_ESCAPING = 30,
    SV_RING_OF_THE_MOUSE = 31,
    SV_RING_OF_THE_DOG = 32,
    SV_RING_SLOW_DIGESTION = 34,
    SV_RING_FEATHER_FALLING = 35,
    SV_RING_FREE_ACTION = 36,
    SV_RING_SEE_INVISIBLE = 37,
    SV_RING_DELVING = 38,
    SV_RING_POLYMORPHING = 39,

    /* Artifacts */
    SV_RING_BARAHIR = 50,
    SV_RING_TULKAS = 51,
    SV_RING_NARYA = 52,
    SV_RING_NENYA = 53,
    SV_RING_VILYA = 54,
    SV_RING_POWER = 55,
    SV_RING_NAZGUL = 56
};


/* Hacky defines */
#define SV_SCROLL_PHASE_DOOR        1
#define SV_SCROLL_TELEPORT          2
#define SV_SCROLL_WORD_OF_RECALL    29
#define SV_SCROLL_LIFE              46


/* Hacky defines */
#define SV_POTION_AUGMENTATION  7
#define SV_POTION_EXPERIENCE    8
#define SV_POTION_HEALING       12
#define SV_POTION_RESTORE_MANA  16
#define SV_POTION_WATER         40
#define SV_POTION_SLIME_MOLD    50
#define SV_POTION_POISON        54

#define SV_POTION_MAX           58


enum sval_food /* tval 80 */
{
    SV_FOOD_RATION = 1,
    SV_FOOD_SLIME_MOLD = 2,
    SV_FOOD_WAYBREAD = 3,
    SV_FOOD_PINT_OF_ALE = 4,
    SV_FOOD_TROUT = 5
};


/* The "sval" codes for mushrooms */
#define SV_SHROOM_SECOND_SIGHT  6
#define SV_SHROOM_FAST_RECOVERY 7
#define SV_SHROOM_VIGOR         8
#define SV_SHROOM_CLEAR_MIND    9
#define SV_SHROOM_EMERGENCY     10
#define SV_SHROOM_TERROR        11
#define SV_SHROOM_STONE         12
#define SV_SHROOM_DEBILITY      14
#define SV_SHROOM_SPRINTING     15
#define SV_SHROOM_PURGING       16


enum sval_crop /* tval 81 */
{
    SV_FOOD_POTATO = 1,
    SV_FOOD_HEAD_OF_CABBAGE = 2,
    SV_FOOD_CARROT = 3,
    SV_FOOD_BEET = 4,
    SV_FOOD_SQUASH = 5,
    SV_FOOD_EAR_OF_CORN = 6
};


/*
 * Special "sval" value -- Elemental spellbook
 */
#define SV_ELEM_BOOK  4


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
 * Special "sval" limit -- first mushroom
 */
#define SV_FOOD_MIN_SHROOM  6


/*
 * Special "sval" values -- "aimed" rods
 */
#define is_rod_aimed(T) \
    (((T)->sval >= 7) && ((T)->sval <= 16)) || \
        (((T)->sval >= 20) && ((T)->sval <= 24))


/*
 * Special "sval" limit -- first "large" chest
 */
#define SV_CHEST_MIN_LARGE  4


/*  
 * Special "sval" value -- unknown "sval"
 */
#define SV_UNKNOWN  255


#endif /* INCLUDED_TVALSVAL_H */
