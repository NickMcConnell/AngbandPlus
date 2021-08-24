/* File: defines.h */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Purpose: global constants and macro definitions */

/*
 * Name of the version/variant
 */
#define VERSION_NAME "PosChengband"


/* Current Version */
#define VER_MAJOR 7
#define VER_MINOR 3
#define VER_PATCH 5
#define VER_EXTRA 0

/* Oldest Supported Version (cf rd_savefile_new_aux) */
#define MIN_VER_MAJOR 7
#define MIN_VER_MINOR 3

#define GAME_MODE_BEGINNER  0
#define GAME_MODE_NORMAL    1
#define GAME_MODE_XXX       2
#define GAME_MODE_MONSTER   3
#define GAME_MODE_MAX       4

/*
 * Maximum dungeon height in grids, must be a multiple of SCREEN_HGT,
 * probably hard-coded to SCREEN_HGT * 3.
 */
#define MAX_HGT         66

/*
 * Maximum dungeon width in grids, must be a multiple of SCREEN_WID,
 * probably hard-coded to SCREEN_WID * 3.
 */
#define MAX_WID         198

/*
 * Maximum number of player "sex" types (see "table.c", etc)
 */
#define MAX_SEXES            2


/* The number of "patrons" available (for Chaos Warriors) */
#define MAX_PATRON          16

/* Number of entries in the sanity-blast descriptions */
#define MAX_SAN_HORROR 20
#define MAX_SAN_FUNNY 22
#define MAX_SAN_COMMENT 5

#define MAX_RACIAL_POWERS 100

/* Chaos mutations */
#define MUT_FLAG_SIZE       6    /* Room for growth */

/* Hallucination stuff */
#define MAX_SILLY_ATTACK 29

#define MAX_SPELLS            70 /* Possessing a GCAWDL gives 62 spells (and about 25 powers)! */

/*
 * Size of memory reserved for initialization of some arrays
 * This approach currently wastes a bit of memory (445474 bytes)
 * by insisting on a single allocation size for all info files.
 * r_info requires 116771 bytes of text info, but k_info, a_info
 * and d_info only need about 11k, 27k and 1k respectively. Also,
 * its a bit tedious to figure current memory requirements. Why
 * not just malloc() string data and let the C library handle things?
 */
#define FAKE_NAME_SIZE  3 * 1024
#define FAKE_TEXT_SIZE  3 * 1024
#define FAKE_TAG_SIZE   3 * 1024    /* max is 2092 (f_info) */


/*
 * Maximum dungeon level.  The player can never reach this level
 * in the dungeon, and this value is used for various calculations
 * involving object and monster creation.  It must be at least 100.
 * Setting it below 128 may prevent the creation of some objects.
 */
#define MAX_DEPTH       128


/*
 * Number of keymap modes
 */
#define KEYMAP_MODES    2

/*
 * Mode for original keyset commands
 */
#define KEYMAP_MODE_ORIG    0

/*
 * Mode for roguelike keyset commands
 */
#define KEYMAP_MODE_ROGUE    1


/*
 * OPTION: Maximum number of macros (see "io.c")
 * Default: assume at most 256 macros are used
 */
#define MACRO_MAX       256

/*
 * OPTION: Maximum number of "quarks" (see "io.c")
 * Default: assume at most 512 different inscriptions are used
 */
#define QUARK_MAX       1024
/* Was 512... 256 quarks added for random artifacts */

/*
 * Maximum value storable in a "byte" (hard-coded)
 */
#define MAX_UCHAR       255

/*
 * Maximum value storable in a "s16b" (hard-coded)
 */
#define MAX_SHORT       32767

/*
 * Maximum length of object's name: Note, players can
 * enter an arbitrarily long inscription which is appended
 * to object descriptions, making the fixed buffer string
 * handling approach foolish. Convert to c-string.c someday ... (237 matches)
 */
#define MAX_NLEN        255
#define MAX_NLEN_OBJ    255
#define MAX_NLEN_MON     80

/*
 * Special internal key
 */
#define SPECIAL_KEY_QUEST    255
#define SPECIAL_KEY_QUIT     252

/*
 * Random energy
 */
#define ENERGY_NEED() (randnor(100, 25))


/*
 * Misc constants
 */
#define TOWN_DAWN         10000    /* Number of ticks from dawn to dawn XXX */
#define TURNS_PER_TICK    10       /* Number of energy-gain-turns per ticks */
#define MAX_DAYS          20000    /* Maximum days */
#define BREAK_GLYPH       550      /* Rune of protection resistance */
#define BREAK_MON_TRAP    299      /* For explosive runes */
#define BTH_PLUS_ADJ       3       /* Adjust BTH per plus-to-hit */
#define MON_DRAIN_LIFE       2       /* Percent of player exp drained per hit */
#define USE_DEVICE           3       /* x> Harder devices x< Easier devices     */


/*** Pet constants ***/


/*
 * Commands
 */
#define PET_DISMISS             1
#define PET_TARGET              2
#define PET_STAY_CLOSE          3
#define PET_FOLLOW_ME           4
#define PET_SEEK_AND_DESTROY    5
#define PET_ALLOW_SPACE         6
#define PET_STAY_AWAY           7
#define PET_OPEN_DOORS          8
#define PET_TAKE_ITEMS          9
#define PET_TELEPORT            10
#define PET_ATTACK_SPELL        11
#define PET_SUMMON_SPELL        12
#define PET_BALL_SPELL          13
#define PET_RIDING              14
#define PET_NAME                15
#define PET_RYOUTE              16
#define PET_NO_BREEDING         17
#define PET_HILITE              18

/*
 * Follow distances
 */
#define PET_CLOSE_DIST          1
#define PET_FOLLOW_DIST         6
#define PET_SEEK_DIST           10
#define PET_DESTROY_DIST        255
#define PET_SPACE_DIST          (-10)
#define PET_AWAY_DIST           (-25)

#define PF_OPEN_DOORS   0x0001
#define PF_PICKUP_ITEMS 0x0002
#define PF_TELEPORT     0x0004
#define PF_ATTACK_SPELL 0x0008
#define PF_SUMMON_SPELL 0x0010
#define PF_BALL_SPELL   0x0020
#define PF_RYOUTE       0x0040
#define PF_NO_BREEDING  0x0080
#define PF_HILITE       0x0100


#define CAN_TWO_HANDS_WIELDING() (!plr->riding || (plr->pet_extra_flags & PF_RYOUTE))


/*
 * There is a 1/20 (5%) chance of inflating the requested object level
 * during the creation of an object (see "get_obj_num()" in "object.c").
 * Lower values yield better objects more often.
 */
#define GREAT_OBJ       10

/*
 * There is a 1/50 (2%) chance of inflating the requested monster level
 * during the creation of a monsters (see "get_mon_num()" in "monster.c").
 * Lower values yield harder monsters more often.
 */
#define NASTY_MON       50              /* 1/chance of inflated monster level */

/* 1/x chance of hurting even if invulnerable! */
#define PENETRATE_INVULNERABILITY 13



/*
 * Refueling constants
 */
#define FUEL_TORCH      5000    /* Maximum amount of fuel in a torch */
#define FUEL_LAMP       15000   /* Maximum amount of fuel in a lantern */


/*
 * More maximum values
 */
#define MAX_SIGHT       20      /* Maximum view distance */
#define MAX_RANGE       18      /* Maximum range (spells, etc) */
#define AAF_LIMIT       100     /* Limit of sensing radius */
#define AAF_LIMIT_RING  255

/*
 * A monster can only "multiply" (reproduce) if there are fewer than 100
 * monsters on the level capable of such spontaneous reproduction.  This
 * is a hack which prevents the "m_list[]" array from exploding due to
 * reproducing monsters.  Messy, but necessary.
 */
#define MAX_REPRO       100

/*
 * Player constants
 */
#define PY_MAX_EXP      99999999L       /* Maximum exp */
#define PY_MAX_GOLD     999999999L      /* Maximum gold */
#define PY_MAX_LEVEL    50              /* Maximum level */

/*
 * Player "food" crucial values
 */
#define PY_FOOD_MAX      15000   /* Food value (Bloated) */
#define PY_FOOD_VAMP_MAX 11000   /* cf race_vampire.c */
#define PY_FOOD_FULL     10000   /* Food value (Normal) */
#define PY_FOOD_ALERT     2000   /* Food value (Hungry) */
#define PY_FOOD_WEAK      1000   /* Food value (Weak) */
#define PY_FOOD_FAINT      500   /* Food value (Fainting) */
#define PY_FOOD_STARVE     100   /* Food value (Starving) */

/*
 * Player regeneration constants
 */
#define PY_REGEN_NORMAL         197     /* Regen factor*2^16 when full */
#define PY_REGEN_WEAK           98      /* Regen factor*2^16 when weak */
#define PY_REGEN_FAINT          33      /* Regen factor*2^16 when fainting */
#define PY_REGEN_HPBASE         1442    /* Min amount hp regen*2^16 */
#define PY_REGEN_MNBASE         524     /* Min amount mana regen*2^16 */

/*
 * Possible realms that can be chosen;
 * currently used only by birth.c and tables.c
 */
#define CH_NONE         0x000000
#define CH_LIFE         0x000001
#define CH_SORCERY      0x000002
#define CH_NATURE       0x000004
#define CH_CHAOS        0x000008
#define CH_DEATH        0x000010
#define CH_TRUMP        0x000020
#define CH_ARCANE       0x000040
#define CH_ENCHANT      0x000080
#define CH_DAEMON       0x000100
#define CH_CRUSADE      0x000200
#define CH_NECROMANCY   0x000400
#define CH_ARMAGEDDON   0x000800
#define CH_ILLUSION     0x001000
#define CH_MUSIC        0x008000    /* This is 16th bit */
#define CH_HISSATSU     0x010000
#define CH_HEX          0x020000
#define CH_RAGE         0x040000
#define CH_BURGLARY     0x080000
#define CH_BLESS        0x100000



/*
 * Magic realms
 */
#define REALM_NONE         0
#define REALM_LIFE         1
#define REALM_SORCERY      2
#define REALM_NATURE       3
#define REALM_CHAOS        4
#define REALM_DEATH        5
#define REALM_TRUMP        6
#define REALM_ARCANE       7
#define REALM_CRAFT        8
#define REALM_DAEMON       9
#define REALM_CRUSADE      10
#define REALM_NECROMANCY   11
#define REALM_ARMAGEDDON   12
#define REALM_ILLUSION     13
#define MAX_MAGIC          13
#define MIN_TECHNIC        16
#define REALM_MUSIC        16
#define REALM_HISSATSU     17
#define REALM_HEX          18  /* Malediction */
#define REALM_RAGE         19
#define REALM_BURGLARY     20
#define REALM_BLESS        21  /* Benediction */
#define MAX_REALM          21

#define VALID_REALM        (MAX_REALM + MAX_MAGIC - MIN_TECHNIC + 1)
#define NUM_TECHNIC        (MAX_REALM - MIN_TECHNIC + 1)

#define is_magic(A) ((((A) > REALM_NONE) && ((A) < MAX_MAGIC + 1)) ? TRUE : FALSE)
#define tval2realm(A) ((A) - TV_LIFE_BOOK + 1)
#define realm2tval(A) ((A) + TV_LIFE_BOOK - 1)
#define technic2magic(A)      (is_magic(A) ? (A) : (A) - MIN_TECHNIC + 1 + MAX_MAGIC)
#define is_good_realm(REALM)   ((REALM) == REALM_LIFE || (REALM) == REALM_CRUSADE || (REALM) == REALM_BLESS)
#define is_evil_realm(REALM)   ((REALM) == REALM_DEATH || (REALM) == REALM_DAEMON || (REALM) == REALM_HEX)

/*
 * Magic-books for the realms
 */
#define REALM1_BOOK     (plr->realm1 + TV_LIFE_BOOK - 1)
#define REALM2_BOOK     (plr->realm2 + TV_LIFE_BOOK - 1)

/*
 * Mode constant for do_spell()
 */
#define SPELL_NAME            0
#define SPELL_DESC            1
#define SPELL_INFO            2
#define SPELL_CAST            3
#define SPELL_FAIL            4
#define SPELL_STOP            5
#define SPELL_CONT            6
#define SPELL_ENERGY          7
#define SPELL_COST_EXTRA      8
#define SPELL_GAIN_MUT        9      /* For Mutations */
#define SPELL_LOSE_MUT       10      /* For Mutations */
#define SPELL_MUT_DESC       11      /* For Mutations */
#define SPELL_CALC_BONUS     12      /* Mutations, Singing, etc.  Called from calc_bonuses() */
#define SPELL_PROCESS        13      /* Randomly activating mutations */
#define SPELL_COLOR          14      /* Terminal Color for display */
#define SPELL_FAIL_MIN       15
#define SPELL_SPOIL_NAME     16
#define SPELL_SPOIL_DESC     17
#define SPELL_HELP_DESC      18
#define SPELL_VALUE          19      /* For valuing object activations */
#define SPELL_ON_BROWSE      20      /* Custom Handler for browsing the spell */
#define SPELL_STAT_NAME      21      /* In case display name (SPELL_NAME) is duplicated/renamed */
#define SPELL_CALC_INNATE    22      /* For Mutations */

/*
 * Indexes of the various "stats" (hard-coded by savefiles, etc).
 */
#define A_NONE -1
#define A_STR   0
#define A_INT   1
#define A_WIS   2
#define A_DEX   3
#define A_CON   4
#define A_CHR   5

#define MAX_STATS 6

/*
 * Player sex constants (hard-coded by save-files, arrays, etc)
 */
#define SEX_FEMALE              0
#define SEX_MALE                1


#define DEMIGOD_MINOR           0
#define DEMIGOD_ZEUS            1
#define DEMIGOD_POSEIDON        2
#define DEMIGOD_HADES           3
#define DEMIGOD_ATHENA          4
#define DEMIGOD_ARES            5
#define DEMIGOD_HERMES          6
#define DEMIGOD_APOLLO          7
#define DEMIGOD_ARTEMIS         8
#define DEMIGOD_HEPHAESTUS      9
#define DEMIGOD_HERA            10
#define DEMIGOD_DEMETER         11
#define DEMIGOD_APHRODITE       12
#define DEMIGOD_MAX             13

#define MAX_DEMIGOD_POWERS        5

#define SPIDER_PHASE  0
#define SPIDER_MAX    1

#define LICH_ARCH     0
#define LICH_MONASTIC 1
/*#define LICH_REAVER   2
  #define LICH_IRON     3*/
#define LICH_MAX      2

#define DEMON_BALROG     0
#define DEMON_MARILITH   1
#define DEMON_KHORNE     2
#define DEMON_CYBERDEMON 3
#define DEMON_MAX        4

#define DRAGON_RED      0
#define DRAGON_WHITE    1
#define DRAGON_BLUE     2
#define DRAGON_BLACK    3
#define DRAGON_GREEN    4
#define DRAGON_NETHER   5
#define DRAGON_LAW      6
#define DRAGON_CHAOS    7
#define DRAGON_BALANCE  8
#define DRAGON_ETHEREAL 9
#define DRAGON_CRYSTAL  10
#define DRAGON_BRONZE   11
#define DRAGON_GOLD     12
#define DRAGON_STEEL    13
#define DRAGON_MAX      14

#define DRACONIAN_RED      0
#define DRACONIAN_WHITE    1
#define DRACONIAN_BLUE     2
#define DRACONIAN_BLACK    3
#define DRACONIAN_GREEN    4
#define DRACONIAN_BRONZE   5
#define DRACONIAN_CRYSTAL  6
#define DRACONIAN_GOLD     7
#define DRACONIAN_SHADOW   8
#define DRACONIAN_MAX      9

#define GIANT_FIRE      0
#define GIANT_FROST     1
#define GIANT_STORM     2
#define GIANT_TITAN     3
#define GIANT_HRU       4
#define GIANT_MAX       5

#define GOLEM_COLOSSUS  0
#define GOLEM_SKY       1
#define GOLEM_MAX       2

#define TROLL_ETTIN    0
#define TROLL_STORM    1
#define TROLL_SPIRIT   2
#define TROLL_KING     3
#define TROLL_MAX      4

#define ELEMENTAL_EARTH   0
#define ELEMENTAL_AIR     1
#define ELEMENTAL_WATER   2
#define ELEMENTAL_FIRE    3
#define ELEMENTAL_MAX     4

/* TODO */
#define RACE_IS_NONLIVING    0x0001
#define RACE_IS_DEMON        0x0002
#define RACE_IS_UNDEAD       0x0004
#define RACE_IS_MONSTER      0x0008
#define RACE_IS_ILLITERATE   0x0010
#define RACE_MARTIAL_ARTS    0x0020
#define RACE_MAGE_BONUS      0x0040

/* Pseudo-ID: Sense1 is the traditional equipable item sensing.
 * Sense2 is jewelry, lights and magical devices (mage like sensing). */
#define CLASS_SENSE1_STRONG  0x0001
#define CLASS_SENSE1_WEAK    0x0002
#define CLASS_SENSE1_SLOW    0x0004
#define CLASS_SENSE1_MED     0x0008
#define CLASS_SENSE1_FAST    0x0010
#define CLASS_SENSE2_STRONG  0x0020
#define CLASS_SENSE2_WEAK    0x0040
#define CLASS_SENSE2_SLOW    0x0080
#define CLASS_SENSE2_MED     0x0100
#define CLASS_SENSE2_FAST    0x0200
#define CLASS_MARTIAL_ARTS   0x0400
#define CLASS_MAGE_BONUS     0x0800

#define DEPRECATED           0x80000000 /* race, class, personality (TODO) */

/* Mimicry uses races too ... Its just that players
   cannot choose these races during birth. */
#define MIMIC_NONE            -1                  /* RACE_HUMAN is 0 and Dopplegangers can mimic humans! */

/*#define prace_is_(A) (plr->mimic_form == (A) || (plr->mimic_form == MIMIC_NONE && (A) < MAX_RACES && plr->prace == (A)))*/
#define psubclass_is_(A, B) (plr->pclass == (A) && plr->psubclass == (B))
#define weaponmaster_is_(B) (plr->pclass == CLASS_WEAPONMASTER && plr->psubclass == (B))
#define warlock_is_(B) (plr->pclass == CLASS_WARLOCK && plr->psubclass == (B))
#define devicemaster_is_(B) (plr->pclass == CLASS_DEVICEMASTER && plr->psubclass == (B))
#define demigod_is_(B) (prace_is_(RACE_DEMIGOD) && plr->psubrace == (B))
#define dragon_is_(B) (prace_is_(RACE_MON_DRAGON) && plr->psubrace == (B))
#define giant_is_(B) (prace_is_(RACE_MON_GIANT) && plr->psubrace == (B))
#define demon_is_(B) (prace_is_(RACE_MON_DEMON) && plr->psubrace == (B))
#define elemental_is_(B) (prace_is_(RACE_MON_ELEMENTAL) && plr->psubrace == (B))
#define draconian_is_(B) (prace_is_(RACE_DRACONIAN) && plr->psubrace == (B))


/*
 * Player class constants (hard-coded by save-files). Arrays should never hard-code these values!
 */
/* Subclasses */
enum {
    WARLOCK_UNDEAD,
    WARLOCK_DRAGONS,
    WARLOCK_ANGELS,
    WARLOCK_DEMONS,
    WARLOCK_HOUNDS,
    WARLOCK_SPIDERS,
    WARLOCK_GIANTS,
    WARLOCK_MAX
};

enum {
    GRAY_MAGE_GOOD,
    GRAY_MAGE_NEUTRAL,
    GRAY_MAGE_EVIL,
    GRAY_MAGE_MAX,
};

#define WEAPONMASTER_NONE      -1
#define WEAPONMASTER_AXES       0
#define WEAPONMASTER_BOWS       1
#define WEAPONMASTER_CLUBS      2
#define WEAPONMASTER_CROSSBOWS  3
#define WEAPONMASTER_DAGGERS    4
#define WEAPONMASTER_POLEARMS   5
#define WEAPONMASTER_SHIELDS    6
#define WEAPONMASTER_SLINGS     7
#define WEAPONMASTER_STAVES     8
#define WEAPONMASTER_SWORDS     9
#define WEAPONMASTER_DIGGERS   10
#define WEAPONMASTER_MAX       11

#define DEVICEMASTER_WANDS   0
#define DEVICEMASTER_STAVES  1
#define DEVICEMASTER_RODS    2
#define DEVICEMASTER_POTIONS 3
#define DEVICEMASTER_SCROLLS 4
#define DEVICEMASTER_MAX     5


enum {
    PERS_ORDINARY = 0,
    PERS_MIGHTY,
    PERS_SHREWD,
    PERS_PIOUS,
    PERS_NIMBLE,
    PERS_FEARLESS,
    PERS_COMBAT,
    PERS_SEXY,
    PERS_LUCKY,
    PERS_PATIENT,
    PERS_CRAVEN,
    PERS_HASTY,
    MAX_PERSONALITIES,
};

/*
 * Number of feats we change to (Excluding default). Used in f_info.txt.
 */
#define MAX_FEAT_STATES     8

/*
 * Bit flags for teleportation
 */
#define TELEPORT_NONMAGICAL    0x00000001
#define TELEPORT_PASSIVE       0x00000002
#define TELEPORT_DEC_VALOUR    0x00000004
#define TELEPORT_LINE_OF_SIGHT 0x00000008
#define TELEPORT_DISENGAGE     0x00000010
#define TELEPORT_OUT_OF_SIGHT  0x00000020

/*
 * Bit flags for the *_can_enter() and monster_can_cross_terrain()
 */
#define CEM_RIDING              0x0001
#define CEM_P_CAN_ENTER_PATTERN 0x0002
#define CEM_MIMIC               0x0004


/*** Object "tval" and "sval" codes ***/

/* Any subvalue */
#define SV_ANY                     255

/* The "sval" codes for TV_FIGURINE */
#define SV_FIGURINE_NORMAL        0

#define SV_CAPTURE_NONE        0

/* The "sval" codes for TV_STATUE */
#define SV_WOODEN_STATUE        0
#define SV_CLAY_STATUE            1
#define SV_STONE_STATUE            2
#define SV_IRON_STATUE            3
#define SV_COPPER_STATUE        4
#define SV_SILVER_STATUE        5
#define SV_GOLDEN_STATUE        6
#define SV_IVORY_STATUE            7
#define SV_MITHRIL_STATUE        8
#define SV_ORNATE_STATUE        9

/* The "sval" codes for TV_CORPSE */
#define SV_SKELETON             0
#define SV_CORPSE            1

/* TV_ARROW */
#define SV_ARROW         1
#define SV_SHEAF_ARROW   2
#define SV_SILVER_ARROW  3
#define SV_MITHRIL_ARROW 4
#define SV_SEEKER_ARROW  5
#define SV_BLACK_ARROW  10

/* TV_BOLT */
#define SV_BOLT          1
#define SV_STEEL_BOLT    2
#define SV_MITHRIL_BOLT  3
#define SV_SEEKER_BOLT   4
#define SV_ADAMANTINE_BOLT 5

/* TV_SHOT */
#define SV_PEBBLE        1
#define SV_SHOT          2
#define SV_MITHRIL_SHOT  3
#define SV_LEAD_SHOT     4

/* The "sval" codes for TV_BOW. The weird sequencing is historic (Previously,
   sval%10 gave the bow multiplier, but this is now specified in k_info, etc).
   BTW: Don't change svals unless you plan on spending quite some time patching
   up s_info.txt for the proficiency system. */
#define SV_SLING                         2
#define SV_SHORT_BOW                    12
#define SV_LONG_BOW                     13
#define SV_GREAT_BOW                    14
#define SV_LIGHT_XBOW                   23
#define SV_HEAVY_XBOW                   24
#define SV_CRIMSON                      50
#define SV_RAILGUN                      51
#define SV_HARP                         70

/* The "sval" codes for TV_DIGGING */
#define SV_SHOVEL                        1
#define SV_GNOMISH_SHOVEL                2
#define SV_DWARVEN_SHOVEL                3
#define SV_PICK                          4
#define SV_ORCISH_PICK                   5
#define SV_DWARVEN_PICK                  6
#define SV_MATTOCK                       7

/* The "sval" values for TV_HAFTED */
#define SV_CLUB                          1    /* 1d4  */
#define SV_WHIP                          2    /* 1d6  */
#define SV_QUARTERSTAFF                  3    /* 1d9  */
#define SV_NUNCHAKU                      4    /* 2d3  */
#define SV_MACE                          5    /* 2d4  */
#define SV_BALL_AND_CHAIN                6    /* 2d4  */
#define SV_JO_STAFF                      7    /* 1d7  */
#define SV_WAR_HAMMER                    8    /* 3d3  */
#define SV_THREE_PIECE_ROD              11    /* 3d3  */
#define SV_MORNING_STAR                 12    /* 2d6  */
#define SV_FLAIL                        13    /* 2d6  */
#define SV_BO_STAFF                     14    /* 1d11 */
#define SV_LEAD_FILLED_MACE             15    /* 3d4  */
#define SV_TETSUBO                      16    /* 2d7  */
#define SV_TWO_HANDED_FLAIL             18    /* 3d6  */
#define SV_GREAT_HAMMER                 19    /* 4d6  */
#define SV_MACE_OF_DISRUPTION           20    /* 5d8  */
#define SV_WIZSTAFF                     21    /* 1d2  */
#define SV_GROND                        50    /* 3d9  */

/* The "sval" values for TV_POLEARM */
#define SV_HATCHET                       1    /* 1d5 */
#define SV_SPEAR                         2    /* 1d6 */
#define SV_SICKLE                        3    /* 2d3 */
#define SV_AWL_PIKE                      4    /* 1d8 */
#define SV_TRIDENT                       5    /* 1d9 */
#define SV_FAUCHARD                      6  /* 1d10 */
#define SV_BROAD_SPEAR                   7    /* 1d9 */
#define SV_PIKE                          8    /* 2d5 */
#define SV_NAGINATA                      9  /* 2d6 */
#define SV_BEAKED_AXE                   10    /* 2d6 */
#define SV_BROAD_AXE                    11    /* 2d6 */
#define SV_LUCERNE_HAMMER               12    /* 2d5  */
#define SV_GLAIVE                       13    /* 2d6 */
#define SV_LAJATANG                     14    /* 2d7 */
#define SV_HALBERD                      15    /* 3d4 */
#define SV_GUISARME                     16  /* 2d5 */
#define SV_SCYTHE                       17    /* 5d3 */
#define SV_LANCE                        20    /* 2d8 */
#define SV_BATTLE_AXE                   22    /* 2d8 */
#define SV_GREAT_AXE                    25    /* 4d4 */
#define SV_TRIFURCATE_SPEAR             26    /* 2d9 */
#define SV_LOCHABER_AXE                 28    /* 3d8 */
#define SV_HEAVY_LANCE                  29  /* 4d8 */
#define SV_SCYTHE_OF_SLICING            30    /* 8d4 */
#define SV_DEATH_SCYTHE                 50    /* 10d10 */
#define SV_DEATH_SCYTHE_HACK            51    /* 10d10 */


/* The "sval" codes for TV_SWORD */
#define SV_BROKEN_DAGGER                 1  /* 1d1 */
#define SV_BROKEN_SWORD                  2  /* 1d2 */
#define SV_DAGGER                        4  /* 1d4 */
#define SV_MAIN_GAUCHE                   5  /* 1d5 */
#define SV_TANTO                         6  /* 1d5 */
#define SV_RAPIER                        7  /* 1d6 */
#define SV_SMALL_SWORD                   8  /* 1d6 */
#define SV_BASILLARD                     9  /* 1d8 */
#define SV_SHORT_SWORD                  10  /* 1d7 */
#define SV_SABRE                        11  /* 1d7 */
#define SV_CUTLASS                      12  /* 1d7 */
#define SV_WAKIZASHI                    13  /* 2d4 */
#define SV_KHOPESH                      14  /* 2d4 */
#define SV_TULWAR                       15  /* 2d4 */
#define SV_BROAD_SWORD                  16  /* 2d5 */
#define SV_LONG_SWORD                   17  /* 2d5 */
#define SV_SCIMITAR                     18  /* 2d5 */
#define SV_NINJATO                      19  /* 1d9 */
#define SV_KATANA                       20  /* 3d4 */
#define SV_BASTARD_SWORD                21  /* 3d4 */
#define SV_GREAT_SCIMITAR               22  /* 4d5 */
#define SV_CLAYMORE                     23  /* 2d8 */
#define SV_ESPADON                      24  /* 2d9 */
#define SV_TWO_HANDED_SWORD             25  /* 3d6 */
#define SV_FLAMBERGE                    26  /* 3d7 */
#define SV_NO_DACHI                     27  /* 5d4 */
#define SV_EXECUTIONERS_SWORD           28  /* 4d5 */
#define SV_ZWEIHANDER                   29  /* 4d6 */
#define SV_BLADE_OF_CHAOS               30  /* 6d5 */
#define SV_DIAMOND_EDGE                 31  /* 7d5 */
#define SV_DOKUBARI                     32  /* 1d1 */
#define SV_HAYABUSA                     33  /* 1d6 */
#define SV_DRAGON_FANG                  34  /* 1d8 */

#define SV_FALCON_SWORD                 33
#define SV_POISON_NEEDLE                32

/* The "sval" codes for TV_SHIELD */
#define SV_SMALL_LEATHER_SHIELD          2
#define SV_SMALL_METAL_SHIELD            3
#define SV_LARGE_LEATHER_SHIELD          4
#define SV_LARGE_METAL_SHIELD            5
#define SV_DRAGON_SHIELD                 6
#define SV_KNIGHT_SHIELD                 7
#define SV_MIRROR_SHIELD                10
#define SV_YATA_MIRROR                  50

/* The "sval" codes for TV_HELM */
#define SV_HARD_LEATHER_CAP              2
#define SV_METAL_CAP                     3
#define SV_JINGASA                       4  /* 4 */
#define SV_IRON_HELM                     5
#define SV_STEEL_HELM                    6
#define SV_DRAGON_HELM                   7
#define SV_KABUTO                        8  /* 7 */

/* The "sval" codes for TV_CROWN */
#define SV_IRON_CROWN                   10
#define SV_GOLDEN_CROWN                 11
#define SV_JEWELED_CROWN                12
#define SV_CHAOS                        50

/* The "sval" codes for TV_BOOTS */
#define SV_PAIR_OF_SOFT_LEATHER_BOOTS    2
#define SV_PAIR_OF_HARD_LEATHER_BOOTS    3
#define SV_PAIR_OF_DRAGON_GREAVE         4
#define SV_PAIR_OF_METAL_SHOD_BOOTS      6

/* The "sval" codes for TV_CLOAK */
#define SV_CLOAK                         1
#define SV_ELVEN_CLOAK                   2
#define SV_FUR_CLOAK                     3
#define SV_ETHEREAL_CLOAK                5
#define SV_SHADOW_CLOAK                  6
#define SV_DRAGON_CLOAK                  7

/* The "sval" codes for TV_GLOVES */
#define SV_SET_OF_LEATHER_GLOVES         1
#define SV_SET_OF_GAUNTLETS              2
#define SV_SET_OF_DRAGON_GLOVES          3
#define SV_SET_OF_CESTI                  5
#define SV_HAND                             6

/* The "sval" codes for TV_SOFT_ARMOR */
#define SV_T_SHIRT                       0
#define SV_FILTHY_RAG                    1
#define SV_ROBE                          2
#define SV_PAPER_ARMOR                   3  /* 4 */
#define SV_SOFT_LEATHER_ARMOR            4
#define SV_SOFT_STUDDED_LEATHER          5
#define SV_HARD_LEATHER_ARMOR            6
#define SV_HARD_STUDDED_LEATHER          7
#define SV_RHINO_HIDE_ARMOR              8
#define SV_CORD_ARMOR                    9  /*  6 */
#define SV_PADDED_ARMOR                 10  /*  4 */
#define SV_LEATHER_SCALE_MAIL           11
#define SV_LEATHER_JACK                 12
#define SV_KUROSHOUZOKU                 13  /* Black Clothes */
#define SV_BLACK_CLOTHES                13
#define SV_STONE_AND_HIDE_ARMOR         15  /* 15 */
#define SV_ABUNAI_MIZUGI                50  /* Swimsuit */
#define SV_YOIYAMI_ROBE                 60  /* Robe of Twilight */
#define SV_NAMAKE_ARMOR                 63

/* The "sval" codes for TV_HARD_ARMOR */
#define SV_RUSTY_CHAIN_MAIL              1  /* 14- */
#define SV_RING_MAIL                     2  /* 12  */
#define SV_METAL_SCALE_MAIL              3  /* 13  */
#define SV_CHAIN_MAIL                    4  /* 14  */
#define SV_DOUBLE_RING_MAIL              5  /* 15  */
#define SV_AUGMENTED_CHAIN_MAIL          6  /* 16  */
#define SV_DOUBLE_CHAIN_MAIL             7  /* 16  */
#define SV_BAR_CHAIN_MAIL                8  /* 18  */
#define SV_METAL_BRIGANDINE_ARMOUR       9  /* 19  */
#define SV_SPLINT_MAIL                  10  /* 19  */
#define SV_DO_MARU                      11  /* 20  */
#define SV_PARTIAL_PLATE_ARMOUR         12  /* 22  */
#define SV_METAL_LAMELLAR_ARMOUR        13  /* 23  */
#define SV_HARAMAKIDO                   14  /* 17  */
#define SV_FULL_PLATE_ARMOUR            15  /* 25  */
#define SV_O_YOROI                      16  /* 24  */
#define SV_RIBBED_PLATE_ARMOUR          18  /* 28  */
#define SV_MITHRIL_CHAIN_MAIL           20  /* 28+ */
#define SV_MITHRIL_PLATE_MAIL           25  /* 35+ */
#define SV_ADAMANTITE_PLATE_MAIL        30  /* 40+ */

/* The "sval" codes for TV_DRAG_ARMOR */
#define SV_DRAGON_BLACK                  1
#define SV_DRAGON_BLUE                   2
#define SV_DRAGON_WHITE                  3
#define SV_DRAGON_RED                    4
#define SV_DRAGON_GREEN                  5
#define SV_DRAGON_MULTIHUED              6
#define SV_DRAGON_SHINING               10
#define SV_DRAGON_LAW                   12
#define SV_DRAGON_BRONZE                14
#define SV_DRAGON_GOLD                  16
#define SV_DRAGON_CHAOS                 18
#define SV_DRAGON_BALANCE               20
#define SV_DRAGON_POWER                 30

/* The sval codes for TV_LIGHT */
#define SV_LIGHT_TORCH                    0
#define SV_LIGHT_LANTERN                  1
#define SV_LIGHT_FEANOR                   2
#define SV_LIGHT_EDISON                   3
#define SV_LIGHT_GALADRIEL                4
#define SV_LIGHT_ELENDIL                  5
#define SV_LIGHT_JUDGE                    6
#define SV_LIGHT_LORE                     7
#define SV_LIGHT_PALANTIR                 8
#define SV_LIGHT_FLY_STONE                9
#define SV_LIGHT_EYE                        10
#define SV_LIGHT_NATURE                  11
#define SV_LIGHT_LIFE                    12
#define SV_LIGHT_SORCERY                 13
#define SV_LIGHT_CHAOS                   14
#define SV_LIGHT_DEATH                   15
#define SV_LIGHT_TRUMP                   16
#define SV_LIGHT_DAEMON                  17
#define SV_LIGHT_CRUSADE                 18
#define SV_LIGHT_CRAFT                   19
#define SV_LIGHT_WAR                     20
#define SV_LIGHT_ARMAGEDDON              21
#define SV_LIGHT_HYDRA                   22
#define SV_LIGHT_MIND                    23
#define SV_LIGHT_DARK                    24

#define SV_AMULT                         0
#define SV_RING                          0
#define SV_EXPRESS_CARD                  0

#define SV_QUIVER_AMMO       0
#define SV_QUIVER_MAGE       1

/* The "sval" codes for TV_SCROLL */
#define SV_SCROLL_DARKNESS               0
#define SV_SCROLL_AGGRAVATE_MONSTER      1
#define SV_SCROLL_CURSE_ARMOR            2
#define SV_SCROLL_CURSE_WEAPON           3
#define SV_SCROLL_SUMMON_MONSTER         4
#define SV_SCROLL_SUMMON_UNDEAD          5
#define SV_SCROLL_SUMMON_PET             6
#define SV_SCROLL_TRAP_CREATION          7
#define SV_SCROLL_PHASE_DOOR             8
#define SV_SCROLL_TELEPORT               9
#define SV_SCROLL_TELEPORT_LEVEL        10
#define SV_SCROLL_WORD_OF_RECALL        11
#define SV_SCROLL_IDENTIFY              12
#define SV_SCROLL_STAR_IDENTIFY         13
#define SV_SCROLL_REMOVE_CURSE          14
#define SV_SCROLL_STAR_REMOVE_CURSE     15
#define SV_SCROLL_ENCHANT_ARMOR         16
#define SV_SCROLL_ENCHANT_WEAPON_TO_HIT 17
#define SV_SCROLL_ENCHANT_WEAPON_TO_DAM 18
/* xxx enchant missile? */
#define SV_SCROLL_STAR_ENCHANT_ARMOR    20
#define SV_SCROLL_STAR_ENCHANT_WEAPON   21
#define SV_SCROLL_RECHARGING            22
#define SV_SCROLL_MUNDANITY             23
#define SV_SCROLL_LIGHT                 24
#define SV_SCROLL_MAPPING               25
#define SV_SCROLL_DETECT_GOLD           26
#define SV_SCROLL_DETECT_ITEM           27
#define SV_SCROLL_DETECT_TRAP           28
#define SV_SCROLL_DETECT_DOOR           29
#define SV_SCROLL_DETECT_INVIS          30
/* xxx (detect evil?) */
#define SV_SCROLL_SATISFY_HUNGER        32
#define SV_SCROLL_BLESSING              33
#define SV_SCROLL_HOLY_CHANT            34
#define SV_SCROLL_HOLY_PRAYER           35
#define SV_SCROLL_MONSTER_CONFUSION     36
#define SV_SCROLL_PROTECTION_FROM_EVIL  37
#define SV_SCROLL_RUNE_OF_PROTECTION    38
#define SV_SCROLL_TRAP_DOOR_DESTRUCTION 39
/* xxx */
#define SV_SCROLL_STAR_DESTRUCTION      41
#define SV_SCROLL_DISPEL_UNDEAD         42
#define SV_SCROLL_SPELL                 43
#define SV_SCROLL_GENOCIDE              44
#define SV_SCROLL_MASS_GENOCIDE         45
#define SV_SCROLL_ACQUIREMENT           46
#define SV_SCROLL_STAR_ACQUIREMENT      47
#define SV_SCROLL_FOREST_CREATION       48
#define SV_SCROLL_WALL_CREATION         49
#define SV_SCROLL_VENGEANCE             50
#define SV_SCROLL_RUMOR                 51
#define SV_SCROLL_ARTIFACT              52
#define SV_SCROLL_RESET_RECALL          53
#define SV_SCROLL_SUMMON_KIN            54
#define SV_SCROLL_CRAFTING              55
#define SV_SCROLL_RUSTPROOF             56
#define SV_SCROLL_DETECT_MONSTERS       57
#define SV_SCROLL_FIRE                  58
#define SV_SCROLL_ICE                   59
#define SV_SCROLL_CHAOS                 60
#define SV_SCROLL_MANA                  61
#define SV_SCROLL_BANISHMENT            62

/* The "sval" codes for TV_POTION */
#define SV_POTION_WATER                  0
#define SV_POTION_APPLE_JUICE            1
#define SV_POTION_SLIME_MOLD             2
/* xxx (fixed color) */
#define SV_POTION_SLOWNESS               4
#define SV_POTION_SALT_WATER             5
#define SV_POTION_POISON                 6
#define SV_POTION_BLINDNESS              7
/* xxx */
#define SV_POTION_CONFUSION              9
/* xxx */
#define SV_POTION_SLEEP                 11
/* xxx */
#define SV_POTION_LOSE_MEMORIES         13
/* xxx */
#define SV_POTION_RUINATION             15
#define SV_POTION_DEC_STR               16
#define SV_POTION_DEC_INT               17
#define SV_POTION_DEC_WIS               18
#define SV_POTION_DEC_DEX               19
#define SV_POTION_DEC_CON               20
#define SV_POTION_DEC_CHR               21
#define SV_POTION_DETONATIONS           22
#define SV_POTION_DEATH                 23
    #define SV_POTION_INFRAVISION           24
    #define SV_POTION_DETECT_INVIS          25
#define SV_POTION_SLOW_POISON           26
    #define SV_POTION_CURE_POISON           27
    #define SV_POTION_BOLDNESS              28
    #define SV_POTION_SPEED                 29
    #define SV_POTION_RESIST_HEAT           30
    #define SV_POTION_RESIST_COLD           31
    #define SV_POTION_HEROISM               32
    #define SV_POTION_BERSERK_STRENGTH      33
    #define SV_POTION_CURE_LIGHT            34
    #define SV_POTION_CURE_SERIOUS          35
    #define SV_POTION_CURE_CRITICAL         36
    #define SV_POTION_HEALING               37
    #define SV_POTION_STAR_HEALING          38
    #define SV_POTION_LIFE                  39
#define SV_POTION_RESTORE_MANA          40
    #define SV_POTION_RESTORE_EXP           41
    #define SV_POTION_RES_STR               42
    #define SV_POTION_RES_INT               43
    #define SV_POTION_RES_WIS               44
    #define SV_POTION_RES_DEX               45
    #define SV_POTION_RES_CON               46
    #define SV_POTION_RES_CHR               47
#define SV_POTION_INC_STR               48
#define SV_POTION_INC_INT               49
#define SV_POTION_INC_WIS               50
#define SV_POTION_INC_DEX               51
#define SV_POTION_INC_CON               52
#define SV_POTION_INC_CHR               53
/* xxx */
#define SV_POTION_AUGMENTATION          55
    #define SV_POTION_ENLIGHTENMENT         56
    #define SV_POTION_STAR_ENLIGHTENMENT    57
    #define SV_POTION_SELF_KNOWLEDGE        58
#define SV_POTION_EXPERIENCE            59
    #define SV_POTION_RESISTANCE            60
    #define SV_POTION_CURING                61
    #define SV_POTION_INVULNERABILITY       62
#define SV_POTION_NEW_LIFE              63
    #define SV_POTION_POLYMORPH             66
#define SV_POTION_BLOOD                    67
#define SV_POTION_GIANT_STRENGTH        68
#define SV_POTION_STONE_SKIN            69
#define SV_POTION_CLARITY               70
#define SV_POTION_GREAT_CLARITY         71
#define SV_POTION_CURE_MUTATION         72

/* The "sval" codes for TV_FLASK */
#define SV_FLASK_OIL                   0

/* The "sval" codes for TV_FOOD */
#define SV_FOOD_POISON                   0
#define SV_FOOD_BLINDNESS                1
#define SV_FOOD_PARANOIA                 2
#define SV_FOOD_CONFUSION                3
#define SV_FOOD_HALLUCINATION            4
#define SV_FOOD_PARALYSIS                5
#define SV_FOOD_WEAKNESS                 6
#define SV_FOOD_SICKNESS                 7
#define SV_FOOD_STUPIDITY                8
#define SV_FOOD_NAIVETY                  9
#define SV_FOOD_UNHEALTH                10
#define SV_FOOD_DISEASE                 11
#define SV_FOOD_CURE_POISON             12
#define SV_FOOD_CURE_BLINDNESS          13
#define SV_FOOD_CURE_PARANOIA           14
#define SV_FOOD_CURE_CONFUSION          15
#define SV_FOOD_CURE_SERIOUS            16
#define SV_FOOD_RESTORE_STR             17
#define SV_FOOD_RESTORE_CON             18
#define SV_FOOD_RESTORING               19
/* many missing mushrooms*/
#define SV_FOOD_MIN_MUSHROOM             0  /* Used by object_is_mushroom() */
#define SV_FOOD_MAX_MUSHROOM            31  /* Assume mushrooms are contiguous svals! */

#define SV_FOOD_BISCUIT                 32
#define SV_FOOD_JERKY                   33
#define SV_FOOD_RATION                  35
#define SV_FOOD_SLIME_MOLD              36
#define SV_FOOD_WAYBREAD                37
#define SV_FOOD_PINT_OF_ALE             38
#define SV_FOOD_PINT_OF_WINE            39
#define SV_FOOD_AMBROSIA                40

/* TV_RUNE */
#define SV_RUNE        1

/*
 * Special "sval" limit -- first "normal" food
 */
#define SV_FOOD_MIN_FOOD                32

/*
 * Special "sval" limit -- first "aimed" rod
 */
#define SV_ROD_MIN_DIRECTION    12

/*
 * Special "sval" limit -- first "large" chest
 */
#define SV_CHEST_MIN_LARGE      4
#define SV_CHEST_KANDUME        50

/*
 * Special "sval" limit -- first "good" magic/prayer book
 */
#define SV_BOOK_MIN_GOOD    2


/* force creeping coins et. al. to drop appropriate coin type */
#define SV_COPPER   3
#define SV_SILVER   6
#define SV_GOLD    11
#define SV_MITHRIL 17
#define SV_ADAMANT 18
#define SV_MAX_GOLD 18


/*** General flag values ***/


/*
 * Bit flags for the "project()" function
 *
 *   JUMP: Jump directly to the target location (this is a hack)
 *   BEAM: Work as a beam weapon (affect every grid passed through)
 *   THRU: Continue "through" the target (used for "bolts"/"beams")
 *   STOP: Stop as soon as we hit a monster (used for "bolts")
 *   GRID: Affect each grid in the "blast area" in some way
 *   ITEM: Affect each object in the "blast area" in some way
 *   KILL: Affect each monster in the "blast area" in some way
 *   HIDE: Hack -- disable "visual" feedback from projection
 *   DISI: Disintegrate non-permanent features
 *   PLAYER: Main target is player (used for riding player)
 *   AIMED: Target is only player or monster, so don't affect another.
 *          Depend on PROJECT_PLAYER.
 *          (used for minimum (rad == 0) balls on riding player)
 *   REFLECTABLE: Reflectable spell attacks (used for "bolts")
 *   ILLUSION: Fool monsters with CAVE_ILLUSION (use grid->mimic rather than grid->feat for FF_LOS|PROJECT)
 *   PATH: Only used for printing project path
 *   FAST: Hide "visual" of flying bolts until blast
 */
#define PROJECT_JUMP           0x000001
#define PROJECT_BEAM           0x000002
#define PROJECT_THRU           0x000004
#define PROJECT_STOP           0x000008
#define PROJECT_GRID           0x000010
#define PROJECT_ITEM           0x000020
#define PROJECT_KILL           0x000040
#define PROJECT_HIDE           0x000080
#define PROJECT_DISI           0x000100
#define PROJECT_PLAYER         0x000200
#define PROJECT_AIMED          0x000400
#define PROJECT_REFLECTABLE    0x000800
#define PROJECT_ILLUSION       0x001000
#define PROJECT_PATH           0x002000
#define PROJECT_FAST           0x004000
#define PROJECT_LOS            0x008000
#define PROJECT_FULL_DAM       0x010000

/*
 * Bit flags for the "enchant()" function
 */
#define ENCH_TOHIT      0x01 /* Enchant to hit */
#define ENCH_TODAM      0x02 /* Enchant to damage */
#define ENCH_TOAC       0x04 /* Enchant to AC */
#define ENCH_FORCE      0x08 /* Force enchantment */
#define ENCH_PSI_HACK   0x10
#define ENCH_MINOR_HACK 0x20

/*
 * Bit flags for the "target_set" function XXX XXX XXX
 *
 *      KILL: Target monsters
 *      LOOK: Describe grid fully
 *      XTRA: Currently unused flag
 *      GRID: Select from all grids
 *      MARK: Hack for The Duelist.  Only target viewable monsters.
 *            Don't restrict selection to LoS.
 *      DISI: Hack for The Bowmaster's Disintegration Arrow. Similar
 *            to MARK but also shows the path.
 *      BALL: Hack for get_fire_pos to correctly select the target
 *            based upon a direction key.
 *      LOS:  Requires Line of Sight
 */
#define TARGET_KILL        0x01
#define TARGET_LOOK        0x02
#define TARGET_XTRA        0x04
#define TARGET_GRID        0x08
#define TARGET_MARK        0x10
#define TARGET_DISI        0x20
#define TARGET_BALL        0x40
#define TARGET_LOS         0x80


/*
 * Bit flags for control of get_check_strict()
 */
#define CHECK_OKAY_CANCEL 0x01
#define CHECK_NO_ESCAPE   0x02
#define CHECK_NO_HISTORY  0x04
#define CHECK_DEFAULT_Y   0x08


/*
 * Bit flags for the "get_item" function
 */
#define USE_EQUIP               0x001   /* Allow equip items */
#define USE_INVEN               0x002   /* Allow inven items */
#define USE_FLOOR               0x004   /* Allow floor items */
#define USE_QUIVER              0x008
#define SHOW_FAIL_RATES         0x010
#define SHOW_VALUE              0x020   /* For Reforging */
#define OPTION_ALL              0x040   /* Allow user to select all (e.g. identify entire pack) */
#define OPTION_FORCE            0x080   /* TODO: Remove old hack code ... */
#define OPTION_UNLIMITED_QUIVER 0x100   /* TODO Remove old hack code ... */

/*
 * Bit flags for the "plr->notice" variable
 */
#define PN_OPTIMIZE_PACK   0x0001
#define PN_OPTIMIZE_QUIVER 0x0002
#define PN_CARRY           0x0004
#define PN_EXP             0x0008     /* check_experience() *after* melee, please! */
/* xxx (many) */


/*
 * Bit flags for the "plr->update" variable
 */
#define PU_BONUS        0x00000001     /* Calculate bonuses */
#define PU_TORCH        0x00000002     /* Calculate torch radius */
#define PU_INNATE       0x00000004     /* Calculate innate attacks */
#define PU_HP           0x00000010     /* Calculate chp and mhp */
#define PU_MANA         0x00000020     /* Calculate csp and msp */
#define PU_SPELLS       0x00000040     /* Calculate spells */
/* xxx (many) */
/* xxx (many) */
#define PU_UN_VIEW      0x00010000     /* Forget view */
#define PU_UN_LIGHT     0x00020000     /* Forget light */
/* xxx (many) */
#define PU_VIEW         0x00100000     /* Update view */
#define PU_LIGHT        0x00200000     /* Update light */
#define PU_MON_LIGHT    0x00400000     /* Monster illumination */
#define PU_DELAY_VIS    0x00800000     /* Mega-Hack -- Delayed visual update */
#define PU_MONSTERS     0x01000000     /* Update monsters */
/* xxx */
#define PU_FLOW         0x10000000     /* Update flow */
#define PU_MON_FLOW     0x20000000     /* mon->flow and pack->flow due to terrain alteration */
/* xxx (many) */


/*
 * Bit flags for the "plr->redraw" variable
 */
#define PR_LEV              0x00000001     /* Display Level */
#define PR_EXP              0x00000002     /* Display Experience */
#define PR_STATS            0x00000004     /* Display Stats */
#define PR_ARMOR            0x00000008     /* Display Armor */
#define PR_HP               0x00000010     /* Display Hitpoints */
#define PR_MANA             0x00000020     /* Display Mana */
#define PR_GOLD             0x00000040     /* Display Gold */
#define PR_DEPTH            0x00000080     /* Display Depth */
#define PR_EQUIPPY          0x00000100     /* Display equippy chars */
#define PR_EFFECTS          0x00000200     /* Display Extra (Cut) */
#define PR_STATUS           0x00000400     /* Display Status Bar */
#define PR_HEALTH_BARS      0x00000800
#define PR_STATE            0x00001000     /* Display Extra (State) */
#define PR_EXTRA            0x00002000     /* Display Extra Info */
#define PR_BASIC            0x00004000     /* Display Basic Info */
#define PR_MAP              0x00008000     /* Display Map */
#define PR_WIPE             0x00010000     /* Hack -- Total Redraw */
#define PR_MSG_LINE         0x00020000
#define PR_MSG_LINE_MAP     0x00040000     /* Optimization to fixup PR_MAP after msg line resize */

/* xxx */
/* xxx */
/* xxx */
/* xxx */

/*
 * Bit flags for the "plr->window" variable (etc)
 */
#define PW_INVEN        0x00000001     /* Display inven/equip */
#define PW_EQUIP        0x00000002     /* Display equip/inven */
#define PW_SPELL        0x00000004     /* Display spell list */
#define PW_WORLD_MAP    0x00000008
#define PW_OBJECT_LIST  0x00000010     /* Display object list */
#define PW_MONSTER_LIST 0x00000020     /* Display monster list */
#define PW_MESSAGE      0x00000040     /* Display messages */
#define PW_OVERHEAD     0x00000080     /* Display overhead view */
#define PW_MONSTER      0x00000100     /* Display monster recall */
#define PW_OBJECT       0x00000200     /* Display object recall */
#define PW_DUNGEON      0x00000400     /* Display dungeon view */
#define PW_SNAPSHOT     0x00000800     /* Display snap-shot */
/* xxx */
/* xxx */
#define PW_BORG_1       0x00004000     /* Display borg messages */
#define PW_BORG_2       0x00008000     /* Display borg status */

/*
 * Bit flags for the place_monster_???() (etc)
 */
#define PM_ALLOW_SLEEP    0x00000001
#define PM_ALLOW_GROUP    0x00000002
#define PM_FORCE_FRIENDLY 0x00000004
#define PM_FORCE_PET      0x00000008
#define PM_NO_KAGE        0x00000010
#define PM_NO_PET         0x00000020
#define PM_ALLOW_UNIQUE   0x00000040
#define PM_IGNORE_TERRAIN 0x00000080
#define PM_HASTE          0x00000100
#define PM_KAGE           0x00000200
#define PM_MULTIPLY       0x00000400
#define PM_ALLOW_CLONED   0x00000800
#define PM_WALL_SCUMMER   0x00001000
#define PM_RING_BEARER    0x00002000
#define PM_QUESTOR        0x00004000
#define PM_ILLUSION       0x00008000
#define PM_NO_FRIEND      0x00010000 /* e.g. hostile mon summons ents */


/* Bit flags for monster_desc() */
#define MD_OBJECTIVE      0x00000001 /* Objective (or Reflexive) */
#define MD_POSSESSIVE     0x00000002 /* Possessive (or Reflexive) */
#define MD_INDEF_HIDDEN   0x00000004 /* Use indefinites for hidden monsters ("something") */
#define MD_INDEF_VISIBLE  0x00000008 /* Use indefinites for visible monsters ("a kobold") */
#define MD_PRON_HIDDEN    0x00000010 /* Pronominalize hidden monsters */
#define MD_PRON_VISIBLE   0x00000020 /* Pronominalize visible monsters */
#define MD_ASSUME_HIDDEN  0x00000040 /* Assume the monster is hidden */
#define MD_ASSUME_VISIBLE 0x00000080 /* Assume the monster is visible */
#define MD_TRUE_NAME      0x00000100 /* Chameleon's true name */
#define MD_IGNORE_HALLU   0x00000200 /* Ignore hallucination, and penetrate shape change */
#define MD_NO_PET_ABBREV  0x00000400
#define MD_IGNORE_FUZZY   0x00000800

/*
 * Bit flags for object_desc()
 */
#define OD_NAME_ONLY        0x00000001  /* Omit values, pval, inscription */
#define OD_NAME_AND_ENCHANT 0x00000002  /* Omit pval, inscription */
#define OD_OMIT_INSCRIPTION 0x00000004  /* Omit inscription */
#define OD_OMIT_PREFIX      0x00000008  /* Omit numeric prefix */
#define OD_NO_PLURAL        0x00000010  /* Don't use plural */
#define OD_STORE            0x00000020  /* Assume to be aware and known */
#define OD_NO_FLAVOR        0x00000040  /* Allow to hidden flavor */
#define OD_FORCE_FLAVOR     0x00000080  /* Get un-shuffled flavor name */
#define OD_NAME_AND_DICE    0x00000100
#define OD_COLOR_CODED      0x00000200  /* For msg_print only */
#define OD_THROWING         0x00000400  /* buggy otherwise for throwing weapon info */
#define OD_SINGULAR         0x00000800  /* pretend obj->number = 1 */
#define OD_SHOW_DEVICE_INFO 0x00001000
#define OD_HIDE_DEVICE_FAIL 0x00002000  /* don't display fail rate twice in obj_prompt/magic_eater */

#define OD_LORE (OD_NAME_ONLY | OD_OMIT_PREFIX | OD_COLOR_CODED)

/*
 * Bit flags for the "plr->special_attack" variable. -LM-
 */
#define ATTACK_CONFUSE        0x00000001


#define KAMAE_GENBU             0x00000020
#define KAMAE_BYAKKO            0x00000040
#define KAMAE_SEIRYU            0x00000080
#define KAMAE_SUZAKU            0x00000100
#define KATA_IAI                0x00000200
#define KATA_FUUJIN             0x00000400
#define KATA_KOUKIJIN           0x00000800
#define KATA_MUSOU              0x00001000
#define NINJA_KAWARIMI          0x00002000
#define NINJA_S_STEALTH         0x00004000
#define DEFENSE_SANCTUARY       0x00008000
#define DEFENSE_INVISIBLE       0x00010000

#define MAX_KAMAE 4
#define KAMAE_MASK (KAMAE_GENBU | KAMAE_BYAKKO | KAMAE_SEIRYU | KAMAE_SUZAKU)

#define MAX_KATA 4
#define KATA_MASK (KATA_IAI | KATA_FUUJIN | KATA_KOUKIJIN | KATA_MUSOU)


#define ACTION_NONE       0
#define ACTION_SEARCH     1
#define ACTION_REST       2
#define ACTION_LEARN      3
#define ACTION_KAMAE      5
#define ACTION_KATA       6
#define ACTION_SING       7
#define ACTION_QUICK_WALK 8
#define ACTION_SPELL      9
#define ACTION_STALK      10
#define ACTION_GLITTER    11      /* Ring waiting for a suitable ring bearer ... */

/*** General index values ***/


/*
 * Legal restrictions for "summon_specific()"
 * Note: _summon_specific_types in init1.c relies on the exact order below for parsing.
 */
enum summon_specific_e {
    SUMMON_MONSTER = 0,
    SUMMON_ANT,
    SUMMON_SPIDER,
    SUMMON_HOUND,
    SUMMON_HYDRA,
    SUMMON_ANGEL,
    SUMMON_DEMON,
    SUMMON_UNDEAD,
    SUMMON_DRAGON,
    SUMMON_HI_UNDEAD,
    SUMMON_HI_DRAGON,
    SUMMON_HI_DEMON,
    SUMMON_AMBERITE,
    SUMMON_UNIQUE,
    SUMMON_BIZARRE1,
    SUMMON_BIZARRE2,
    SUMMON_BIZARRE3,
    SUMMON_BIZARRE4,
    SUMMON_BIZARRE5,
    SUMMON_BIZARRE6,
    SUMMON_CYBER,
    SUMMON_KIN,
    SUMMON_DAWN,
    SUMMON_ANIMAL,
    SUMMON_ANIMAL_RANGER,
    SUMMON_PHANTOM,
    SUMMON_BLUE_HORROR,
    SUMMON_LIVING,
    SUMMON_HI_DRAGON_LIVING,
    SUMMON_GOLEM,
    SUMMON_ELEMENTAL,
    SUMMON_VORTEX,
    SUMMON_HYBRID,
    SUMMON_BIRD,
    SUMMON_KAMIKAZE,
    SUMMON_KAMIKAZE_LIVING,
    SUMMON_MANES,
    SUMMON_LOUSE,
    SUMMON_GUARDIAN,
    SUMMON_KNIGHT,
    SUMMON_EAGLE,
    SUMMON_PIRANHA,
    SUMMON_ARMAGE_GOOD,
    SUMMON_ARMAGE_EVIL,
    SUMMON_SOFTWARE_BUG,
    SUMMON_OLYMPIAN,
    SUMMON_RAT,
    SUMMON_BAT,
    SUMMON_WOLF,
    SUMMON_DREAD,
    SUMMON_ZOMBIE,
    SUMMON_SKELETON,
    SUMMON_GHOST,
    SUMMON_VAMPIRE,
    SUMMON_WIGHT,
    SUMMON_LICH,
    SUMMON_KRAKEN,
    SUMMON_THIEF,
    SUMMON_ENT,
    SUMMON_CAMELOT,
    SUMMON_NIGHTMARE,
    SUMMON_YEEK,
    SUMMON_ORC,
    SUMMON_DARK_ELF,
    SUMMON_GIANT,
    SUMMON_UNDEAD_SUMMONER,
    SUMMON_MATURE_DRAGON,
    SUMMON_DRAGON_SUMMONER,
    SUMMON_CLUBBER_DEMON,
    SUMMON_BALROG,
    SUMMON_DEMON_SUMMONER,
    SUMMON_ULTIMATE,
    SUMMON_HUMAN,
    SUMMON_HORSE,
    SUMMON_MAGICAL,
    SUMMON_TROLL,
    SUMMON_CHAPEL_GOOD,
    SUMMON_CHAPEL_EVIL,
    SUMMON_RING_BEARER,
    SUMMON_ARCHER,
    SUMMON_MONK,
    SUMMON_MAGE,
    SUMMON_ELDRITCH_HORROR,
};

#define DAMAGE_FORCE    1
#define DAMAGE_GENO     2
#define DAMAGE_LOSELIFE 3
#define DAMAGE_ATTACK   4
#define DAMAGE_NOESCAPE 5
#define DAMAGE_USELIFE  6


/*
 * Game generated inscription indices. These are stored in the object,
 * and are used to index the string array from tables.c (game_inscriptions).
 *
 * For strong sensing, we have now have (3.0.3 and later):
 *
 *                    egos         artifacts
 *                    =========    =========
 * average -> good -> excellent -> special
 *         -> bad  -> awful     -> terrible
 *
 * For weak sensing, we have:
 *
 * FEEL_NONE -> enchanted
 *           -> cursed
 *
 * This means that FEEL_CURSED items might be egos or artifacts, and should
 * never be automatically destroyed, whereas FEEL_BAD items are known to be
 * cursed non-egos/arts and can be autodestroyed with impunity. See autopick.c
 * for details. Is "broken" still used?
 */
#define FEEL_NONE              0
#define FEEL_BROKEN            1   /* ?? */
#define FEEL_TERRIBLE          2
#define FEEL_AWFUL             3
#define FEEL_CURSED            4
#define FEEL_ENCHANTED         5
#define FEEL_AVERAGE           6
#define FEEL_GOOD              7
#define FEEL_EXCELLENT         8
#define FEEL_SPECIAL           9
#define FEEL_BAD               10

/*
 * Hack -- special "xtra" object powers
 */

/* Sustain one stat */
#define EGO_XTRA_SUSTAIN        1

/* High resist */
#define EGO_XTRA_POWER          2

/* Special ability */
#define EGO_XTRA_ABILITY        3

/*** Object flag values ***/


/*
 * Chest trap flags (see "tables.c")
 */
#define CHEST_LOSE_STR          0x0001
#define CHEST_LOSE_CON          0x0002
#define CHEST_POISON            0x0004
#define CHEST_PARALYZE          0x0008
#define CHEST_EXPLODE           0x0010
#define CHEST_SUMMON            0x0020
#define CHEST_SCATTER           0x0040
#define CHEST_E_SUMMON          0x0080
#define CHEST_BIRD_STORM        0x0100
#define CHEST_H_SUMMON          0x0200
#define CHEST_RUNES_OF_EVIL     0x0400
#define CHEST_ALARM             0x0800


/*
 * Special Object Flags
 */
#define IDENT_SENSE     0x01    /* Item has been "sensed" */
#define IDENT_FIXED     0x02    /* Item has been "haggled" */
#define IDENT_EMPTY     0x04    /* Item charges are known */
#define IDENT_KNOWN     0x08    /* Item abilities are known */
#define IDENT_STORE     0x10    /* Item is in a store's inventory */
#define IDENT_XXX6      0x20
#define IDENT_TRIED     0x40    /* Device has been tried, but still unknown */
#define IDENT_BROKEN    0x80    /* Item is permanently worthless */


/*
 * How object is marked (flags in object_type.mark)
 * OM_FOUND --- original boolean flag
 * OM_NOMSG --- temporary flag to suppress messages which were
 *              already printed in autopick_pickup_items().
 */
#define OM_FOUND           0x0001    /* original boolean flag */
#define OM_NO_MSG          0x0002    /* temporary flag to suppress messages */
#define OM_NO_QUERY        0x0004    /* Query for auto-pick was already answered as 'No' */
#define OM_AUTODESTROY     0x0008
#define OM_TOUCHED         0x0010    /* Object was touched by player */
#define OM_RESERVED        0x0020    /* Object reserved in the shop */
#define OM_WORN            0x0040    /* Object was previously being worn but is possibly no longer a legal piece of equipment (Mimics) */
#define OM_COUNTED         0x0080    /* Stats */
#define OM_EGO_COUNTED     0x0100    /* Stats */
#define OM_ART_COUNTED     0x0200    /* Stats */
#define OM_EFFECT_COUNTED  0x0400    /* Stats */
#define OM_DELAYED_MSG     0x0800    /* Describe inventory slot *after* resorting */
#define OM_PICKUP_MASK (OM_TOUCHED | OM_COUNTED | OM_EFFECT_COUNTED | OM_EGO_COUNTED | OM_ART_COUNTED)


/*
 * Special Monster Flags (all temporary)
 */
#define MFLAG_VIEW       0x00000001    /* Monster is in line of sight */
#define MFLAG_TEMP       0x00000002    /* Monster is marked for project_hack() */
#define MFLAG_TICK       0x00000004    /* mon_tim_tick */
/* The following are for mon_process and probably should be private */
#define MFLAG_IGNORE_PLR 0x00000008   /* handle ninja stealth ... mon_ai is allowed to consider plr */
#define MFLAG_WILL_RUN   0x00000010    /* not just T_FEAR, but pack morale, et. al. */
#define MFLAG_PROCESS_MASK (MFLAG_IGNORE_PLR | MFLAG_WILL_RUN)
#define MFLAG_LIGHT      0x00010000    /* monster's light is part of plr_light() */

#define MFLAG2_KAGE             0x00000001    /* Monster is kage */
#define MFLAG2_NOPET            0x00000002    /* Cannot make monster pet */
#define MFLAG2_NOGENO           0x00000004    /* Cannot genocide */
#define MFLAG2_CHAMELEON        0x00000008    /* Monster is chameleon */
#define MFLAG2_XXX5             0x00000010
#define MFLAG2_SHOW             0x00000020    /* Monster is recently memorized */
#define MFLAG2_MARK             0x00000040    /* Monster is currently memorized */
#define MFLAG2_TRIPPED          0x00000080
#define MFLAG2_VAULT            0x00000100
#define MFLAG2_NODESTRUCT       0x00000200
#define MFLAG2_AWARE            0x00000400
#define MFLAG2_DROP_BASIC       0x00000800
#define MFLAG2_DROP_UTILITY     0x00001000
#define MFLAG2_DROP_PRIZE       0x00002000
#define MFLAG2_DROP_MASK        (MFLAG2_DROP_BASIC | MFLAG2_DROP_UTILITY | MFLAG2_DROP_PRIZE)
#define MFLAG2_QUESTOR          0x00004000   /* using monster_race.flags1 & RF1_QUESTOR is error prone */
#define MFLAG2_FUZZY            0x00008000   /* fuzzy telepathy */
#define MFLAG2_HUNTED           0x00010000   /* somebody's out to get me; following mon->flow */
#define MFLAG2_HUNTER           0x00020000   /* temp: AI_HUNT(no prey)->AI_WANDER(stairs)->AI_HUNT(prey) */
#define MFLAG2_ILLUSION         0x00040000
#define MFLAG2_WEB              0x00080000   /* stuck in web: spell AI might allow escape */
#define MFLAG2_LORE             0x00100000   /* allow lore even if fuzzy */

#define PFLAG_TEMP       0x00000001
#define PFLAG_BIRTH      0x00000002

#define MON_HUNT_RAD   100
#define MON_WANDER_RAD 100
/*
 * Object Flags (OF_*)
 *
 * Old variables for object flags such as flags1, flags2, and flags3
 * are obsoleted.  Now single array flgs[OF_ARRAY_SIZE] contains all
 * object flags.  And each flag is refered by single index number
 * instead of a bit mask.
 *
 * All management of flags is now treated using a set of macros
 * instead of bit operations.

 * (FYI, the !! on have_flag makes this safe as assignment to a bool
 *  which is just a byte. See A.7.4.7 in K&R.)
 */

#define have_flag(ARRAY, INDEX) !!((ARRAY)[(INDEX)/32] & (1L << ((INDEX)%32)))
#define add_flag(ARRAY, INDEX) ((ARRAY)[(INDEX)/32] |= (1L << ((INDEX)%32)))
#define remove_flag(ARRAY, INDEX) ((ARRAY)[(INDEX)/32] &= ~(1L << ((INDEX)%32)))

/* Extra flags for object lore (OFL_*)
 * cf object_type.known_flags and .known_curse_flags for normal stuff.
 * cf .known_xtra for the following: */
#define OFL_DEVICE_POWER 0x01
#define OFL_DEVICE_FAIL  0x02

/* Object Flags for Generation (OFG_*) */
#define OFG_INSTA_ART           0x00000001     /* Item must be an artifact */
#define OFG_QUESTITEM           0x00000002     /* quest level item -KMW- */
#define OFG_XTRA_POWER          0x00000004     /* Extra power */
#define OFG_ONE_SUSTAIN         0x00000008     /* One sustain */
#define OFG_XTRA_RES_OR_POWER   0x00000010     /* Extra resistance or power */
#define OFG_XTRA_H_RES          0x00000020     /* Extra high resistance */
#define OFG_XTRA_E_RES          0x00000040     /* Extra element resistance */
#define OFG_XTRA_L_RES          0x00000080     /* Extra lordly resistance */
#define OFG_XTRA_D_RES          0x00000100     /* Extra dragon resistance */
#define OFG_XTRA_RES            0x00000200     /* Extra resistance */
#define OFG_CURSED              0x00000400     /* Item is Cursed */
#define OFG_HEAVY_CURSE         0x00000800     /* Item is Heavily Cursed */
#define OFG_PERMA_CURSE         0x00001000     /* Item is Perma Cursed */
#define OFG_RANDOM_CURSE0       0x00002000     /* Item is Random Cursed */
#define OFG_RANDOM_CURSE1       0x00004000     /* Item is Random Cursed */
#define OFG_RANDOM_CURSE2       0x00008000     /* Item is Random Cursed */
#define OFG_AWARE               0x00010000
#define OFG_TOWN                0x00020000     /* Item is allowed to be stocked in town */
#define OFG_FIXED_ART           0x00040000     /* Never replace this art when using random_artifacts */
#define OFG_FIXED_ACT           0x00080000     /* Keep original activation on replacement artifacts */

/* Object Flags for Curses (OFC_*) */
#define MAX_CURSE 17

#define OFC_CURSED              0x00000001
#define OFC_HEAVY_CURSE         0x00000002
#define OFC_PERMA_CURSE         0x00000004
#define OFC_XXX1                0x00000008
#define OFC_TY_CURSE            0x00000010
#define OFC_AGGRAVATE           0x00000020
#define OFC_DRAIN_EXP           0x00000040
#define OFC_SLOW_REGEN          0x00000080
#define OFC_ADD_L_CURSE         0x00000100
#define OFC_ADD_H_CURSE         0x00000200
#define OFC_CALL_ANIMAL         0x00000400
#define OFC_CALL_DEMON          0x00000800
#define OFC_CALL_DRAGON         0x00001000
#define OFC_COWARDICE           0x00002000
#define OFC_TELEPORT            0x00004000
#define OFC_LOW_MELEE           0x00008000
#define OFC_LOW_AC              0x00010000
#define OFC_LOW_MAGIC           0x00020000
#define OFC_FAST_DIGEST         0x00040000
#define OFC_DRAIN_HP            0x00080000
#define OFC_DRAIN_MANA          0x00100000
#define OFC_TELEPORT_SELF       0x00200000
#define OFC_CHAINSWORD          0x00400000

#define TRC_SPECIAL_MASK \
    (OFC_TY_CURSE | OFC_AGGRAVATE)

#define TRC_HEAVY_MASK   \
    (OFC_TY_CURSE | OFC_AGGRAVATE | OFC_DRAIN_EXP | OFC_ADD_H_CURSE | \
     OFC_CALL_DEMON | OFC_CALL_DRAGON | OFC_TELEPORT)

#define TRC_P_FLAG_MASK  \
    (OFC_TELEPORT_SELF | OFC_CHAINSWORD | \
     OFC_TY_CURSE | OFC_DRAIN_EXP | OFC_ADD_L_CURSE | OFC_ADD_H_CURSE | \
     OFC_CALL_ANIMAL | OFC_CALL_DEMON | OFC_CALL_DRAGON | OFC_COWARDICE | \
     OFC_TELEPORT | OFC_DRAIN_HP | OFC_DRAIN_MANA)

/*
 * Bit flags for apply_magic() (etc). Check out OBJ_DROP_* flags in obj.h
 * to avoid collisions when adding new flags here.
 */
#define AM_NO_FIXED_ART 0x00000001 /* Don't allow roll for fixed artifacts */
#define AM_GOOD         0x00000002 /* Generate good items */
#define AM_GREAT        0x00000004 /* Generate great items */
#define AM_SPECIAL      0x00000008 /* Generate artifacts (for debug mode only) */
#define AM_CURSED       0x00000010 /* Generate cursed/worthless items */
#define AM_CRAFTING     0x00000020
#define AM_AVERAGE      0x00000040
#define AM_TAILORED     0x00000080 /* For player monster races to force a wearable item */
#define AM_FORCE_EGO    0x00000100
#define AM_STOCK_TOWN   0x00000200
#define AM_STOCK_BM     0x00000400
#define AM_QUEST        0x00000800
#define AM_UNIQUE       0x00001000
#define AM_VAULT        0x00002000
#define AM_DEBUG        0x00004000 /* Don't _forge_extra */
#define AM_NO_DROP      0x00008000 /* Don't track artifact generation */
#define AM_MASK         0x0000FFFF

/*** Monster blow constants ***/


/*
 * New monster blow methods
 */
enum {
    RBM_NONE = 0,
    RBM_HIT,
    RBM_TOUCH,
    RBM_PUNCH,
    RBM_KICK,
    RBM_CLAW,
    RBM_BITE,
    RBM_STING,
    RBM_SLASH,
    RBM_BUTT,
    RBM_CRUSH,
    RBM_ENGULF,
    RBM_CHARGE,
    RBM_CRAWL,
    RBM_DROOL,
    RBM_SPIT,
    RBM_EXPLODE,
    RBM_GAZE,
    RBM_WAIL,
    RBM_SPORE,
    RBM_PECK,
    RBM_BEG,
    RBM_INSULT,
    RBM_MOAN,
    RBM_SHOW,
    /* martial arts (also uses PUNCH, KICK and BUTT)
     * Note: The goal is to allow players and monsters to use
     * the same martial arts attack code. cf monk_attack.c
     * and ../lib/edit/monk_attack.txt */
    RBM_MONK,
    RBM_STRIKE,
    RBM_KNEE,
    RBM_ELBOW,
    RBM_UPPERCUT,
    RBM_DOUBLE_KICK,
    RBM_CATS_CLAW,
    RBM_JUMP_KICK,
    RBM_EAGLES_CLAW,
    RBM_CIRCLE_KICK,
    RBM_IRON_FIST,
    RBM_FLYING_KICK,
    RBM_DRAGON_FIST,
    RBM_CRUSHING_BLOW,
    RBM_ZOMBIE_CLAW,
    RBM_GHOUL_TOUCH,
    RBM_LICH_FIST,
    RBM_REAVER_FIST,
    RBM_HAND_OF_VECNA,
    RBM_IMP_CLAW,
    RBM_DEVIL_CLAW,
    RBM_HELL_HAMMER,
    RBM_SATANS_CLAW,
    RBM_CHAOS_FIST,
    RBM_HELL_CLAW,
    RBM_VAMP_FIST,
    RBM_COUNT
};

/*
 * New monster blow effects
 * Note: monster blows can either use one of
 * the following, or they can directly use a 
 * GF_* effect code. Assert GF_COUNT < 5000.
 */
enum {
    /* damaging effects */
    RBE_HURT = 5000,
    RBE_SHATTER,
    RBE_DISEASE,
    RBE_VAMP,

    /* non-damaging effects (in progress) */
    RBE_EAT_GOLD,
    RBE_EAT_ITEM,
    RBE_EAT_FOOD,
    RBE_EAT_LIGHT,
    RBE_LOSE_STR,
    RBE_LOSE_INT,
    RBE_LOSE_WIS,
    RBE_LOSE_DEX,
    RBE_LOSE_CON,
    RBE_LOSE_CHR,
    RBE_LOSE_ALL,
    RBE_DRAIN_CHARGES,
    RBE_DRAIN_EXP,
    RBE_CUT,
    RBE_STUN_MALE,
    RBE_SLOW_ANKLE,
};

/*** Monster flag values (hard-coded) ***/


/* Extra flags valid for a single game only (saved and loaded)
 * N.B. Out of extreme stinginess, mon_race->flagsx is a byte for packing purposes */
#define RFX_QUESTOR         0x01  /* Unique Monster tagged for a random quest */
#define RFX_SUPPRESS        0x02  /* Unique Monster won't generate this game */
#define RFX_WANTED          0x04  /* Wanted monster (bounty at hunters guild) */
#define RFX_BOUNTY          0x08  /* Player turned in wanted corpse at hunters guild) */
#define RFX_GUARDIAN        0x10  /* Monster guards a dungeon (1) */

#define is_aware(A) \
     (bool)(((A)->mflag2 & MFLAG2_AWARE) ? TRUE : FALSE)

/* Hack -- Determine monster race appearance index is same as race index */
#define is_original_ap(A) \
     (bool)(((A)->apparent_race == (A)->race) ? TRUE : FALSE)

#define is_original_ap_and_seen(A) \
     (bool)((A)->ml && !plr_tim_find(T_HALLUCINATE) && ((A)->apparent_race == (A)->race))



/*** Option Definitions ***/


#define OPT_PAGE_INPUT          1
#define OPT_PAGE_MAPSCREEN      2
#define OPT_PAGE_TEXT           3
#define OPT_PAGE_GAMEPLAY       4
#define OPT_PAGE_DISTURBANCE    5
#define OPT_PAGE_BIRTH          6
#define OPT_PAGE_AUTODESTROY    7
#define OPT_PAGE_PLAYRECORD    10

#define OPT_PAGE_JAPANESE_ONLY 99


/*** Macro Definitions ***/


/*
 * Hack -- The main "screen"
 */
#define term_screen     (angband_term[0])


/*
 * Return the "attr" for a given item.
 * Use "flavor" if available.
 * Default to user definitions.
 */
#define object_attr(T) \
    ((k_info[(T)->k_idx].flavor) ? \
     (k_info[k_info[(T)->k_idx].flavor].x_attr) : \
     ((!(T)->k_idx || ((T)->tval != TV_CORPSE) || ((T)->sval != SV_CORPSE) || \
       (k_info[(T)->k_idx].x_attr != TERM_DARK)) ? \
      (k_info[(T)->k_idx].x_attr) : (mon_race_visual(mon_race_lookup((T)->race_id)).a)))

/*
 * Return the "char" for a given item.
 * Use "flavor" if available.
 * Default to user definitions.
 */
#define object_char(T) \
    ((k_info[(T)->k_idx].flavor) ? \
     (k_info[k_info[(T)->k_idx].flavor].x_char) : \
     (k_info[(T)->k_idx].x_char))

/*
 * Convert an "attr"/"char" pair into a "pict" (P)
 */
#define PICT(A,C) \
    ((((u16b)(A)) << 8) | ((byte)(C)))

/*
 * Convert a "pict" (P) into an "attr" (A)
 */
#define PICT_A(P) \
    ((byte)((P) >> 8))

/*
 * Convert a "pict" (P) into an "char" (C)
 */
#define PICT_C(P) \
    ((char)((byte)(P)))



/*
 * Hack -- Prepare to use the "Secure" routines
 */
#if defined(SET_UID) && defined(SECURE)
extern int PlayerUID;
# define getuid() PlayerUID
# define geteuid() PlayerUID
#endif



/*** Color constants ***/


/*
 * Angband "attributes" (with symbols, and base (R,G,B) codes)
 *
 * The "(R,G,B)" codes are given in "fourths" of the "maximal" value,
 * and should "gamma corrected" on most (non-Macintosh) machines.
 */


/*** Sound constants ***/


/*
 * Mega-Hack -- some primitive sound support (see "main-win.c")
 *
 * Some "sound" constants for "Term_xtra(TERM_XTRA_SOUND, val)"
 */
#define SOUND_HIT        1
#define SOUND_MISS       2
#define SOUND_FLEE       3
#define SOUND_DROP       4
#define SOUND_KILL       5
#define SOUND_LEVEL      6
#define SOUND_DEATH      7
#define SOUND_STUDY      8
#define SOUND_TELEPORT   9
#define SOUND_SHOOT     10
#define SOUND_QUAFF     11
#define SOUND_ZAP       12
#define SOUND_WALK      13
#define SOUND_TPOTHER   14
#define SOUND_HITWALL   15
#define SOUND_EAT       16
#define SOUND_STORE1    17
#define SOUND_STORE2    18
#define SOUND_STORE3    19
#define SOUND_STORE4    20
#define SOUND_DIG       21
#define SOUND_OPENDOOR  22
#define SOUND_SHUTDOOR  23
#define SOUND_TPLEVEL   24
#define SOUND_SCROLL    25
#define SOUND_BUY        26
#define SOUND_SELL        27
#define SOUND_WARN        28
#define SOUND_ROCKET    29 /* Somebody's shooting rockets */
#define SOUND_N_KILL    30 /* The player kills a non-living/undead monster */
#define SOUND_U_KILL    31 /* The player kills a unique */
#define SOUND_QUEST     32 /* The player has just completed a quest */
#define SOUND_HEAL      33 /* The player was healed a little bit */
#define SOUND_X_HEAL    34 /* The player was healed full health */
#define SOUND_BITE      35 /* A monster bites you */
#define SOUND_CLAW      36 /* A monster claws you */
#define SOUND_M_SPELL   37 /* A monster casts a miscellaneous spell */
#define SOUND_SUMMON    38 /* A monster casts a summoning spell  */
#define SOUND_BREATH    39 /* A monster breathes */
#define SOUND_BALL      40 /* A monster casts a ball / bolt spell */
#define SOUND_M_HEAL    41 /* A monster heals itself somehow */
#define SOUND_ATK_SPELL 42 /* A monster casts a misc. offensive spell */
#define SOUND_EVIL      43 /* Something nasty has just happened! */
#define SOUND_TOUCH     44 /* A monster touches you */
#define SOUND_STING     45 /* A monster stings you */
#define SOUND_CRUSH     46 /* A monster crushes / envelopes you */
#define SOUND_SLIME     47 /* A monster drools/spits/etc on you */
#define SOUND_WAIL      48 /* A monster wails */
#define SOUND_WINNER    49 /* Just won the game! */
#define SOUND_FIRE      50 /* An item was burned  */
#define SOUND_ACID      51 /* An item was destroyed by acid */
#define SOUND_ELEC      52 /* An item was destroyed by electricity */
#define SOUND_COLD      53 /* An item was shattered */
#define SOUND_ILLEGAL   54 /* Illegal command attempted */
#define SOUND_FAIL      55 /* Fail to get a spell off / activate an item */
#define SOUND_WAKEUP    56 /* A monster wakes up */
#define SOUND_INVULN    57 /* Invulnerability! */
#define SOUND_FALL      58 /* Falling through a trapdoor... */
#define SOUND_PAIN      59 /* A monster is in pain! */
#define SOUND_DESTITEM  60 /* An item was destroyed by misc. means */
#define SOUND_MOAN      61 /* A monster makes a moan/beg/insult attack */
#define SOUND_SHOW      62 /* A monster makes a "show" attack */
#define SOUND_UNUSED    63 /* (no sound for gaze attacks) */
#define SOUND_EXPLODE   64 /* Something (or somebody) explodes */
#define SOUND_GLASS     65 /* A glass feature was crashed */

/*
 * Mega-Hack -- maximum known sounds
 */
#define SOUND_MAX 66

/* Virtues: Note, using V_FOO style macros collides with windows.h */

#define VIRTUE_NONE          0
#define VIRTUE_COMPASSION    1
#define VIRTUE_HONOUR        2
#define VIRTUE_JUSTICE       3
#define VIRTUE_SACRIFICE     4
#define VIRTUE_KNOWLEDGE     5
#define VIRTUE_FAITH         6
#define VIRTUE_ENLIGHTENMENT 7
#define VIRTUE_ENCHANTMENT   8
#define VIRTUE_CHANCE        9
#define VIRTUE_NATURE        10
#define VIRTUE_HARMONY       11
#define VIRTUE_VITALITY      12
#define VIRTUE_UNLIFE        13
#define VIRTUE_PATIENCE      14
#define VIRTUE_TEMPERANCE    15
#define VIRTUE_DILIGENCE     16
#define VIRTUE_VALOUR        17
#define VIRTUE_INDIVIDUALISM 18
#define VIRTUE_MAX           19

/*** Hack ***/


/*
 * Hack -- attempt to reduce various values
 */
#ifdef ANGBAND_LITE
# undef MACRO_MAX
# define MACRO_MAX      128
# undef QUARK_MAX
# define QUARK_MAX      128
#endif


/*
 * Initialization flags
 */
#define INIT_XXXXXX1            0x01
#define INIT_XXXXXX2            0x02
#define INIT_XXXXXX3            0x04
#define INIT_XXXXXX4            0x08
#define INIT_XXXXXX5            0x10
#define INIT_DEBUG              0x20 /* error checking on dungeon files */

/*
 * Available graphic modes
 */
#define GRAPHICS_NONE       0
#define GRAPHICS_ORIGINAL   1

/*
 * Modes for the tokenizer
 */
#define TOKENIZE_CHECKQUOTE 0x01  /* Special handling of single quotes */
#define TOKENIZE_NO_SLASH   0x02  /* e.g. G:/:g can't uses tokenize for display char of '/' */
#define TOKENIZE_NO_ESCAPE  0x04  /* e.g. G:\\:g is weird if you want display char of '\' */

/*
 * Parse errors
 */
#define PARSE_ERROR_GENERIC                  1
#define PARSE_ERROR_OBSOLETE_FILE            2
#define PARSE_ERROR_MISSING_RECORD_HEADER    3
#define PARSE_ERROR_NON_SEQUENTIAL_RECORDS   4
#define PARSE_ERROR_INVALID_FLAG             5
#define PARSE_ERROR_UNDEFINED_DIRECTIVE      6
#define PARSE_ERROR_OUT_OF_MEMORY            7
#define PARSE_ERROR_OUT_OF_BOUNDS            8
#define PARSE_ERROR_TOO_FEW_ARGUMENTS        9
#define PARSE_ERROR_UNDEFINED_TERRAIN_TAG   10
#define PARSE_ERROR_MAX                     11

#define SKILL_MARTIAL_ARTS  0
#define SKILL_DUAL_WIELDING 1
#define SKILL_RIDING        2

/* Proficiency level */
#define EXP_LEVEL_UNSKILLED 0
#define EXP_LEVEL_BEGINNER  1
#define EXP_LEVEL_SKILLED   2
#define EXP_LEVEL_EXPERT    3
#define EXP_LEVEL_MASTER    4

/* Proficiency of weapons and misc. skills (except riding) */
#define WEAPON_EXP_UNSKILLED     0
#define WEAPON_EXP_BEGINNER   4000
#define WEAPON_EXP_SKILLED    6000
#define WEAPON_EXP_EXPERT     7000
#define WEAPON_EXP_MASTER     8000

/* Proficiency of riding */
#define RIDING_EXP_UNSKILLED     0
#define RIDING_EXP_BEGINNER    500
#define RIDING_EXP_SKILLED    2000
#define RIDING_EXP_EXPERT     5000
#define RIDING_EXP_MASTER     8000

/* Proficiency of spells */
#define SPELL_EXP_UNSKILLED      0
#define SPELL_EXP_BEGINNER     900
#define SPELL_EXP_SKILLED     1200
#define SPELL_EXP_EXPERT      1400
#define SPELL_EXP_MASTER      1600


#define MAX_MAGIC_NUM 108

#define MAX_KUBI 20

#define DETECT_RAD_DEFAULT 30
#define DETECT_RAD_MAP     30
#define DETECT_RAD_ALL     255

#define DO_AUTOPICK       0x01
#define DO_AUTODESTROY    0x02
#define DO_DISPLAY        0x04
#define DONT_AUTOPICK     0x08
#define ITEM_DISPLAY      0x10
#define DO_QUERY_AUTOPICK 0x20
#define DO_AUTO_ID        0x40

#define MAGIC_GAIN_EXP    0x0004

#define KNOW_STAT   0x01
#define KNOW_HPRATE 0x02

/* XXX I'm refactoring ... these should all be removed
 * or made private. cf PLR_HIT_* codes in plr_attack.h.
 * private codes typically begin at PLR_HIT_CUSTOM (5000) */
enum {
    _DEAD_ATTACK_MODES = 7000,
    HISSATSU_IAI,  /* XXX mon_attack needs player hooks */
};

/* Multishadow effects is determined by turn */
#define CHECK_MULTISHADOW() (plr_tim_find(T_MULTISHADOW) && (dun_mgr()->turn & 1))

/*
 * Max numbers of macro trigger names
 */
#define MAX_MACRO_MOD 12
#define MAX_MACRO_TRIG 200

/* Max size of screen dump buffer */
#define SCREEN_BUF_SIZE 65536


/*
 * Special key code used for inkey_special()
 */
#define SKEY_MOD_MASK     0x0f00
#define SKEY_MOD_SHIFT    0x0100
#define SKEY_MOD_CONTROL  0x0200

#define SKEY_MASK         0xf000
#define SKEY_DOWN         0xf001
#define SKEY_LEFT         0xf002
#define SKEY_RIGHT        0xf003
#define SKEY_UP           0xf004
#define SKEY_PGUP         0xf005
#define SKEY_PGDOWN       0xf006
#define SKEY_TOP          0xf007
#define SKEY_BOTTOM       0xf008

/*
 * Bit flags for move_player_effect()
 */
#define MPE_STAYING       0x00000001
#define MPE_FORGET_FLOW   0x00000002
#define MPE_HANDLE_STUFF  0x00000004
#define MPE_ENERGY_USE    0x00000008
#define MPE_DONT_PICKUP   0x00000010
#define MPE_DO_PICKUP     0x00000020
#define MPE_BREAK_TRAP    0x00000040
#define MPE_DONT_SWAP_MON 0x00000080


#define CONCENT_RADAR_THRESHOLD 2
#define CONCENT_TELE_THRESHOLD  5

/* object_type.rune */
#define RUNE_ABSORPTION           1
#define RUNE_PROTECTION           2
#define RUNE_REGENERATION         3
#define RUNE_FIRE                 4
#define RUNE_AIR                  5
#define RUNE_WATER                6
#define RUNE_LIGHT                7
#define RUNE_SHADOW               8
#define RUNE_EARTH                9
#define RUNE_UNDERSTANDING        10
#define RUNE_ELEMENTAL_PROTECTION 11
#define RUNE_HASTE                12
#define RUNE_SEEING               13
#define RUNE_SACRIFICE            14
#define RUNE_LIFE                 15
#define RUNE_STABILITY            16
#define RUNE_REFLECTION           17
#define RUNE_DEATH                18
#define RUNE_MIND                 19
#define RUNE_MIGHT                20
#define RUNE_DESTRUCTION          21
#define RUNE_GOOD_FORTUNE         22
#define RUNE_IMMORTALITY          23

/* Weaponmaster et. al. toggle modes. These
 * values are written to savefiles, so do not
 * alter! (plr->magic_num1[0]) */
enum {
    TOGGLE_NONE = 0,

    /* Slingmaster */
    TOGGLE_SHOT_ON_THE_RUN = 1,
    TOGGLE_RAPID_SHOT,

    /* Daggermaster */
    TOGGLE_FLYING_DAGGER_STANCE = 10,
    TOGGLE_SHADOW_STANCE,
    TOGGLE_FRENZY_STANCE,

    /* Clubmaster */
    TOGGLE_COMBAT_EXPERTISE = 20,
    TOGGLE_TRADE_BLOWS,

    /* Axemaster */
    TOGGLE_POWER_ATTACK = 30,

    /* Swordmaster */
    TOGGLE_BURNING_BLADE = 40,
    TOGGLE_ICE_BLADE,
    TOGGLE_THUNDER_BLADE,
    TOGGLE_SHARD_BLADE,
    TOGGLE_CHAOS_BLADE,
    TOGGLE_DEATH_BLADE,
    TOGGLE_DEMON_BLADE,
    TOGGLE_PATTERN_BLADE,
    TOGGLE_HOLY_BLADE,
    TOGGLE_ARMAGEDDON_BLADE,

    /* Scythemaster */
    TOGGLE_MANY_STRIKE = 60,
    TOGGLE_PIERCING_STRIKE,
    TOGGLE_TRIP,

    /* Pickmaster */
    TOGGLE_STRENGTH_OF_THE_UNDERTAKER = 70,
    TOGGLE_STOICISM,
    TOGGLE_INDUSTRIOUS_MORTICIAN,

    /* Shieldmaster */
    TOGGLE_SHIELD_BASH = 80,
    TOGGLE_BULWARK,
    TOGGLE_SHIELD_REVENGE,

    /* Bowmaster */
    TOGGLE_READIED_SHOT = 90,
    TOGGLE_PIERCING_ARROW,

    /* Crossbowmaster */
    TOGGLE_RAPID_RELOAD = 100,
    TOGGLE_EXPLODING_BOLT,
    TOGGLE_OVERDRAW,
    TOGGLE_CAREFUL_AIM,

    MAULER_TOGGLE_BLOCK = 110,
    MAULER_TOGGLE_SHATTER,
    MAULER_TOGGLE_TUNNEL,
    MAULER_TOGGLE_DRAIN,
    MAULER_TOGGLE_MAUL,
    MAULER_TOGGLE_SPLATTER,

    MYSTIC_TOGGLE_STEALTH = 120,
    MYSTIC_TOGGLE_FAST,
    MYSTIC_TOGGLE_RETALIATE,
    MYSTIC_TOGGLE_OFFENSE,
    MYSTIC_TOGGLE_DEFENSE,

    LEPRECHAUN_TOGGLE_BLINK = 130,
    LEPRECHAUN_TOGGLE_HOARDING,

    WARLOCK_DRAGON_TOGGLE_BLESS = 140,
    WARLOCK_DRAGON_TOGGLE_CANTER,
    WARLOCK_DRAGON_TOGGLE_GALLOP,
    WARLOCK_DRAGON_TOGGLE_HEALING,
    WARLOCK_DRAGON_TOGGLE_HEROIC_CHARGE,
};

/* All of the following enumeration values are persisted in
   savefiles, so should not be changed. Add new options to
   the bottom of the respective enum (just before the _MAX
   entry, if present)
*/

/* Many ego types are now shared across mutliple kinds of equipment */
enum ego_e {
    EGO_TYPE_NONE         = 0,

    /* Melee Weapons */
    EGO_TYPE_WEAPON       = 0x00000001,
    EGO_TYPE_DIGGER       = 0x00000002,

    /* Armor */
    EGO_TYPE_SHIELD       = 0x00000004,
    EGO_TYPE_BODY_ARMOR   = 0x00000008,
    EGO_TYPE_ROBE         = 0x00000010,
    EGO_TYPE_DRAGON_ARMOR = 0x00000020,
    EGO_TYPE_CLOAK        = 0x00000040,
    EGO_TYPE_HELMET       = 0x00000080,
    EGO_TYPE_CROWN        = 0x00000100,
    EGO_TYPE_GLOVES       = 0x00000200,
    EGO_TYPE_BOOTS        = 0x00000400,

    /* Missile Weapons */
    EGO_TYPE_BOW          = 0x00000800,
    EGO_TYPE_AMMO         = 0x00001000,
    EGO_TYPE_HARP         = 0x00002000,
    EGO_TYPE_QUIVER       = 0x00004000,

    /* Jewelry, Lights, Devices */
    EGO_TYPE_RING         = 0x00008000,
    EGO_TYPE_AMULET       = 0x00010000,
    EGO_TYPE_LIGHT        = 0x00020000,
    EGO_TYPE_DEVICE       = 0x00040000,

    /* (Blasted) */
    EGO_TYPE_SPECIAL      = 0x00080000,

    /* New */
    EGO_TYPE_DARK         = 0x00100000,
};

enum ego_type_e {
    /* Melee Weapons */
    EGO_WEAPON_SLAYING = 1,
    EGO_WEAPON_SHARPNESS,
    EGO_WEAPON_FORCE,
    EGO_WEAPON_BLESSED,
    EGO_WEAPON_EXTRA_ATTACKS = 5,
    EGO_WEAPON_ARCANE,
    EGO_WEAPON_ARMAGEDDON,
    EGO_WEAPON_CHAOS,
    EGO_WEAPON_CRAFT,
    EGO_WEAPON_CRUSADE = 10,
    EGO_WEAPON_DAEMON,
    EGO_WEAPON_DEATH,
    EGO_WEAPON_LIFE,
    EGO_WEAPON_NATURE,
    EGO_WEAPON_TRUMP = 15,
    EGO_WEAPON_DEFENDER = 18,
    EGO_WEAPON_WESTERNESSE,
    EGO_WEAPON_GONDOLIN = 20,
    EGO_WEAPON_MORGUL,
    EGO_WEAPON_PATTERN,
    EGO_WEAPON_NOLDOR,
    EGO_WEAPON_JOUSTING,
    EGO_WEAPON_HELL_LANCE = 25,
    EGO_WEAPON_HOLY_LANCE,

    EGO_DIGGER_DIGGING = 40,
    EGO_DIGGER_DISSOLVING,
    EGO_DIGGER_DISRUPTION,

    /* Armor */
    EGO_ARMOR_PROTECTION = 50,
    EGO_ARMOR_ELEMENTAL_PROTECTION,
    EGO_ARMOR_CELESTIAL_PROTECTION,
    EGO_ARMOR_ELVENKIND,
    EGO_ARMOR_STEALTH,
    EGO_ARMOR_FREE_ACTION = 55,
    EGO_ARMOR_SEEING,

    EGO_SHIELD_DWARVEN = 60,
    EGO_SHIELD_ORCISH,
    EGO_SHIELD_REFLECTION,
    EGO_SHIELD_NIGHT_AND_DAY,
    EGO_SHIELD_ENDURANCE,

    EGO_BODY_DWARVEN = 70,
    EGO_BODY_URUK_HAI,
    EGO_BODY_OLOG_HAI,
    EGO_BODY_DEMON,
    EGO_BODY_DEMON_LORD,

    EGO_ROBE_PERMANENCE = 80,
    EGO_ROBE_TWILIGHT,
    EGO_ROBE_SORCERER,

    EGO_DRAGON_LORE = 85,
    EGO_DRAGON_BREATH,
    EGO_DRAGON_ATTACK,
    EGO_DRAGON_CRAFT,
    EGO_DRAGON_ARMOR,
    EGO_DRAGON_DOMINATION = 90,
    EGO_DRAGON_CRUSADE,
    EGO_DRAGON_DEATH,

    EGO_CLOAK_COWARDICE = 95,
    EGO_CLOAK_IMMOLATION,
    EGO_CLOAK_ELECTRICITY,
    EGO_CLOAK_FREEZING,
    EGO_CLOAK_RETRIBUTION,
    EGO_CLOAK_SHADOWS = 100,
    EGO_CLOAK_AMAN,
    EGO_CLOAK_BAT,
    EGO_CLOAK_NAZGUL,

    EGO_HELMET_KNOWLEDGE = 110,
    EGO_HELMET_PIETY,
    EGO_HELMET_DOMINATION,
    EGO_HELMET_FORTITUDE,
    EGO_HELMET_KOBOLD,
    EGO_HELMET_TROLL = 115,
    EGO_HELMET_VAMPIRE,
    EGO_HELMET_SUNLIGHT,
    EGO_HELMET_DWARVEN,
    EGO_HELMET_VALKYRIE,
    EGO_HELMET_RAGE = 120,

    EGO_CROWN_TELEPATHY = 125,
    EGO_CROWN_MAGI,
    EGO_CROWN_MIGHT,
    EGO_CROWN_LORDLINESS,
    EGO_CROWN_ANGMAR,
    EGO_CROWN_UNBELIEVER = 130,

    EGO_GLOVES_SLAYING = 135,
    EGO_GLOVES_THIEF,
    EGO_GLOVES_GIANT,
    EGO_GLOVES_WIZARD,
    EGO_GLOVES_YEEK,
    EGO_GLOVES_GENJI = 140,
    EGO_GLOVES_SNIPER,
    EGO_GLOVES_BERSERKER,

    EGO_BOOTS_LEVITATION = 145,
    EGO_BOOTS_GNOMISH,
    EGO_BOOTS_DWARVEN,
    EGO_BOOTS_SPEED,
    EGO_BOOTS_ELVENKIND,
    EGO_BOOTS_SPRITE = 151,
    EGO_BOOTS_GOLEM,

    /* Missile Weapons */
    EGO_BOW_ACCURACY = 160,
    EGO_BOW_VELOCITY,
    EGO_BOW_EXTRA_MIGHT,
    EGO_BOW_EXTRA_SHOTS,
    EGO_BOW_LOTHLORIEN,
    EGO_BOW_HARADRIM = 165,
    EGO_BOW_BUCKLAND,
    EGO_BOW_HUNTER,

    EGO_AMMO_SLAYING = 180,
    EGO_AMMO_ELEMENTAL,
    EGO_AMMO_HOLY_MIGHT,
    EGO_AMMO_RETURNING,
    EGO_AMMO_ENDURANCE,
    EGO_AMMO_EXPLODING = 185,
    EGO_AMMO_PIERCING,
    EGO_AMMO_DAEMON,

    EGO_HARP_VANYAR = 195,
    EGO_HARP_EREBOR,

    /* Jewelry */
    EGO_JEWELRY_DEFENDER = 200,
    EGO_JEWELRY_ELEMENTAL,

    EGO_RING_PROTECTION = 205,
    EGO_RING_COMBAT,
    EGO_RING_ARCHERY,
    EGO_RING_WIZARDRY,
    EGO_RING_SPEED,
    EGO_RING_NAZGUL = 210,
    EGO_RING_DWARVES,

    EGO_AMULET_BARBARIAN = 220,
    EGO_AMULET_SACRED,
    EGO_AMULET_HELL,
    EGO_AMULET_DWARVEN,
    EGO_AMULET_MAGI,
    EGO_AMULET_HERO = 225,
    EGO_AMULET_DEVOTION,
    EGO_AMULET_TRICKERY,

    /* Lites */
    EGO_LIGHT_EXTRA_LIGHT = 235,
    EGO_LIGHT_ILLUMINATION,
    EGO_LIGHT_DURATION,
    EGO_LIGHT_INFRAVISION,
    EGO_LIGHT_IMMOLATION,
    EGO_LIGHT_DARKNESS = 240,
    EGO_LIGHT_IMMORTAL_EYE,
    EGO_LIGHT_VALINOR,
    EGO_LIGHT_SCRYING,
    EGO_DARK_DARKNESS,
    EGO_DARK_NETHERWORLD,

    /* Devices */
    EGO_DEVICE_RESISTANCE = 250,
    EGO_DEVICE_CAPACITY,
    EGO_DEVICE_REGENERATION,
    EGO_DEVICE_SIMPLICITY,
    EGO_DEVICE_POWER,
    EGO_DEVICE_HOLDING = 255,
    EGO_DEVICE_QUICKNESS,

    /* Special */
    EGO_SPECIAL_BLASTED = 260,

    /* Quivers */
    EGO_QUIVER_HOLDING = 265,
    EGO_QUIVER_PROTECTION,
    EGO_QUIVER_ENDLESS,
    EGO_QUIVER_PHASE,
    EGO_QUIVER_REGEN,
};


enum effect_e
{
    EFFECT_NONE = 0,

    /* Detection */
    EFFECT_LIGHT_AREA = 1,
    EFFECT_LIGHT_MAP_AREA,
    EFFECT_ENLIGHTENMENT,
    EFFECT_CLAIRVOYANCE,

    EFFECT_DETECT_TRAPS,
    EFFECT_DETECT_MONSTERS,
    EFFECT_DETECT_OBJECTS,
    EFFECT_DETECT_ALL,
    EFFECT_DETECT_GOLD,
    EFFECT_DETECT_INVISIBLE = 10,
    EFFECT_DETECT_DOOR_STAIRS,
    EFFECT_DETECT_EVIL,

    /* Utility */
    EFFECT_PHASE_DOOR = 50,
    EFFECT_TELEPORT,
    EFFECT_TELEPORT_AWAY,
    EFFECT_STRAFING,
    EFFECT_DIMENSION_DOOR,
    EFFECT_ESCAPE,
    EFFECT_RECALL,

    EFFECT_STONE_TO_MUD,
    EFFECT_EARTHQUAKE,
    EFFECT_DESTRUCTION,
    EFFECT_GENOCIDE,         /*60*/
    EFFECT_MASS_GENOCIDE,

    EFFECT_RECHARGE_FROM_DEVICE,
    EFFECT_ENCHANTMENT,
    EFFECT_IDENTIFY,
    EFFECT_IDENTIFY_FULL,
    EFFECT_PROBING,
    EFFECT_RUNE_EXPLOSIVE,
    EFFECT_RUNE_PROTECTION,

    EFFECT_SATISFY_HUNGER,
    EFFECT_DESTROY_TRAP,    /*70*/
    EFFECT_DESTROY_TRAPS,
    EFFECT_WHIRLWIND_ATTACK,
    EFFECT_LIST_UNIQUES,
    EFFECT_LIST_ARTIFACTS,
    EFFECT_BANISH_EVIL,
    EFFECT_BANISH_ALL,
    EFFECT_TELEKINESIS,
    EFFECT_ALCHEMY,
    EFFECT_SELF_KNOWLEDGE,

    EFFECT_GENOCIDE_ONE,   /*80*/
    EFFECT_RECHARGE_FROM_PLAYER,

    /* Timed Buffs */
    EFFECT_STONE_SKIN = 100,
    EFFECT_RESIST_ACID,
    EFFECT_RESIST_ELEC,
    EFFECT_RESIST_FIRE,
    EFFECT_RESIST_COLD,
    EFFECT_RESIST_POIS,
    EFFECT_RESISTANCE,
    EFFECT_PROT_EVIL,
    EFFECT_HOLY_GRAIL,
    EFFECT_BLESS,
    EFFECT_HEROISM,
    EFFECT_BERSERK,
    EFFECT_SPEED,
    EFFECT_SPEED_HERO,
    EFFECT_SPEED_HERO_BLESS,
    EFFECT_LIGHT_SPEED,
    EFFECT_ENLARGE_WEAPON,
    EFFECT_TELEPATHY,
    EFFECT_WRAITHFORM,
    EFFECT_INVULNERABILITY,

    /* Pets */
    EFFECT_SUMMON_MONSTERS = 150,
    EFFECT_SUMMON_HOUNDS,
    EFFECT_SUMMON_ANTS,
    EFFECT_SUMMON_HYDRAS,
    EFFECT_SUMMON_OCTOPUS,
    EFFECT_SUMMON_DAWN,
    EFFECT_SUMMON_PHANTASMAL,
    EFFECT_SUMMON_ELEMENTAL,
    EFFECT_SUMMON_DRAGON,
    EFFECT_SUMMON_UNDEAD,
    EFFECT_SUMMON_DEMON,
    EFFECT_SUMMON_CYBERDEMON,
    EFFECT_SUMMON_ANGEL,
    EFFECT_SUMMON_KRAKEN,

    EFFECT_CHARM_ANIMAL = 175,
    EFFECT_CHARM_DEMON,
    EFFECT_CHARM_UNDEAD,
    EFFECT_CHARM_MONSTER,

    EFFECT_RETURN_PETS = 190,
    EFFECT_CAPTURE_PET,

    /* Healing and Recovery */
    EFFECT_RESTORE_STATS = 200,
    EFFECT_RESTORE_EXP,
    EFFECT_RESTORING,
    EFFECT_HEAL,
    EFFECT_CURING,
    EFFECT_HEAL_CURING,
    EFFECT_HEAL_CURING_HERO,
    EFFECT_RESTORE_MANA,
    EFFECT_CURE_POIS,
    EFFECT_CURE_FEAR,
    EFFECT_CURE_FEAR_POIS,
    EFFECT_REMOVE_CURSE,
    EFFECT_REMOVE_ALL_CURSE,
    EFFECT_CLARITY,
    EFFECT_GREAT_CLARITY,

    /* Offense: Bolts */
    EFFECT_BOLT_MISSILE = 300,
    EFFECT_BOLT_ACID,
    EFFECT_BOLT_ELEC,
    EFFECT_BOLT_FIRE,
    EFFECT_BOLT_COLD,
    EFFECT_BOLT_POIS,
    EFFECT_BOLT_LIGHT,
    EFFECT_BOLT_DARK,
    EFFECT_BOLT_CONF,
    EFFECT_BOLT_NETHER,
    EFFECT_BOLT_NEXUS = 310,
    EFFECT_BOLT_SOUND,
    EFFECT_BOLT_SHARDS,
    EFFECT_BOLT_CHAOS,
    EFFECT_BOLT_DISEN,
    EFFECT_BOLT_TIME,
    EFFECT_BOLT_WATER,
    EFFECT_BOLT_MANA,
    EFFECT_BOLT_ICE,
    EFFECT_BOLT_PLASMA = 319,

    /* Offense: Beams */
    EFFECT_BEAM_LIGHT_WEAK = 350,
    EFFECT_BEAM_LIGHT,
    EFFECT_BEAM_GRAVITY,
    EFFECT_BEAM_DISINTEGRATE,
    EFFECT_BEAM_ACID,
    EFFECT_BEAM_ELEC,
    EFFECT_BEAM_FIRE,
    EFFECT_BEAM_COLD,
    EFFECT_BEAM_SOUND,
    EFFECT_BEAM_CHAOS = 359,

    /* Offense: Balls */
    EFFECT_BALL_ACID = 400,
    EFFECT_BALL_ELEC,
    EFFECT_BALL_FIRE,
    EFFECT_BALL_COLD,
    EFFECT_BALL_POIS,
    EFFECT_BALL_LIGHT,
    EFFECT_BALL_DARK,
    EFFECT_BALL_CONF,
    EFFECT_BALL_NETHER,
    EFFECT_BALL_NEXUS,
    EFFECT_BALL_SOUND = 410,
    EFFECT_BALL_SHARDS,
    EFFECT_BALL_CHAOS,
    EFFECT_BALL_DISEN,
    EFFECT_BALL_TIME,
    EFFECT_BALL_WATER,
    EFFECT_BALL_MANA,
    EFFECT_BALL_DISINTEGRATE,

    /* Offense: Breaths */
    EFFECT_BREATHE_ACID = 450,
    EFFECT_BREATHE_ELEC,
    EFFECT_BREATHE_FIRE,
    EFFECT_BREATHE_COLD,
    EFFECT_BREATHE_POIS,
    EFFECT_BREATHE_LIGHT,
    EFFECT_BREATHE_DARK,
    EFFECT_BREATHE_CONF,
    EFFECT_BREATHE_NETHER,
    EFFECT_BREATHE_NEXUS,
    EFFECT_BREATHE_SOUND,
    EFFECT_BREATHE_SHARDS,
    EFFECT_BREATHE_CHAOS,
    EFFECT_BREATHE_DISEN,
    EFFECT_BREATHE_TIME,
    EFFECT_BREATHE_ONE_MULTIHUED, /* DSM with random breath types ... */
    EFFECT_BREATHE_ONE_CHAOS,
    EFFECT_BREATHE_ONE_LAW,
    EFFECT_BREATHE_ONE_BALANCE,
    EFFECT_BREATHE_ONE_SHINING,
    EFFECT_BREATHE_ELEMENTS,
    EFFECT_BREATHE_HOLY_FIRE,
    EFFECT_BREATHE_HELL_FIRE,

    /* Offense: Other */
    EFFECT_DISPEL_EVIL = 550,
    EFFECT_DISPEL_EVIL_HERO,
    EFFECT_DISPEL_GOOD,
    EFFECT_DISPEL_LIFE,
    EFFECT_DISPEL_DEMON,
    EFFECT_DISPEL_UNDEAD,
    EFFECT_DISPEL_MONSTERS,
    EFFECT_DRAIN_LIFE,
    EFFECT_STAR_BALL,
    EFFECT_ROCKET,
    EFFECT_MANA_STORM = 560,    /* Centered on player. cf EFFECT_BALL_MANA */
    EFFECT_CONFUSING_LIGHT,
    EFFECT_ARROW,
    EFFECT_WRATH_OF_GOD,
    EFFECT_METEOR,
    EFFECT_HOLINESS,
    EFFECT_STARBURST,      /* Centered on player. cf EFFECT_BALL_LIGHT */
    EFFECT_DARKNESS_STORM, /* Centered on player. cf EFFECT_BALL_DARK */
    EFFECT_PESTICIDE,

    /* Misc */
    EFFECT_POLY_SELF = 600,
    EFFECT_ANIMATE_DEAD,

    EFFECT_SCARE_MONSTER,
    EFFECT_SCARE_MONSTERS,
    EFFECT_SLEEP_MONSTER,
    EFFECT_SLEEP_MONSTERS,
    EFFECT_SLOW_MONSTER,
    EFFECT_SLOW_MONSTERS,
    EFFECT_STASIS_MONSTERS,
    EFFECT_CONFUSE_MONSTER,
    EFFECT_CONFUSE_MONSTERS,

    EFFECT_PIERCING_SHOT,
    EFFECT_CHARGE,
    EFFECT_WALL_BUILDING,
    EFFECT_POLYMORPH,
    EFFECT_STARLIGHT,
    EFFECT_NOTHING,      /* Food for undead players */
    EFFECT_ENDLESS_QUIVER,

    /* Bad Effects */
    EFFECT_AGGRAVATE = 900,
    EFFECT_HEAL_MONSTER,
    EFFECT_HASTE_MONSTER,
    EFFECT_HASTE_MONSTERS,
    EFFECT_CLONE_MONSTER,
    EFFECT_DARKNESS,
    EFFECT_SUMMON_ANGRY_MONSTERS,
    EFFECT_SLOWNESS,


    /* Specific Artifacts ... Try to minimize! */
    EFFECT_JEWEL = 1000,
    EFFECT_HERMES,
    EFFECT_ARTEMIS,
    EFFECT_DEMETER,
    EFFECT_EYE_VECNA,
    EFFECT_ONE_RING,
    EFFECT_BLADETURNER,
    EFFECT_BLOODY_MOON,
    EFFECT_SACRED_KNIGHTS,
    EFFECT_GONG,
    EFFECT_MURAMASA,

    EFFECT_MAX
};

/* "Biases" for random artifact generation */
#define BIAS_ELEC            0x00000001
#define BIAS_POIS            0x00000002
#define BIAS_FIRE            0x00000004
#define BIAS_COLD            0x00000008
#define BIAS_ACID            0x00000010
#define BIAS_ELEMENTAL      (BIAS_ELEC | BIAS_POIS | BIAS_FIRE | BIAS_COLD | BIAS_ACID)
#define BIAS_STR             0x00000020
#define BIAS_INT             0x00000040
#define BIAS_WIS             0x00000080
#define BIAS_DEX             0x00000100
#define BIAS_CON             0x00000200
#define BIAS_CHR             0x00000400
#define BIAS_CHAOS           0x00000800
#define BIAS_PRIESTLY        0x00001000
#define BIAS_NECROMANTIC     0x00002000
#define BIAS_LAW             0x00004000
#define BIAS_ROGUE           0x00008000
#define BIAS_MAGE            0x00010000
#define BIAS_WARRIOR         0x00020000
#define BIAS_RANGER          0x00040000
#define BIAS_DEMON           0x00080000
#define BIAS_PROTECTION      0x00100000
#define BIAS_ARCHER          0x00200000

enum dragon_realm_e
{
    DRAGON_REALM_NONE, /* Steel dragons and upgrading old savefiles */
    DRAGON_REALM_LORE,
    DRAGON_REALM_BREATH,
    DRAGON_REALM_ATTACK,
    DRAGON_REALM_CRAFT,
    DRAGON_REALM_ARMOR,
    DRAGON_REALM_DOMINATION,
    DRAGON_REALM_CRUSADE,
    DRAGON_REALM_DEATH,
    DRAGON_REALM_MAX
};

enum ui_result_e
{
    UI_NONE = 0,
    UI_OK,
    UI_CANCEL,
    UI_QUIT,
    UI_RESTART,
    UI_UNWIND,
    UI_ERROR
};

