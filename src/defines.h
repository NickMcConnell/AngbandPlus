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


#define VER_MAJOR 5
#define VER_MINOR 0
#define VER_PATCH 1
#define VER_EXTRA 1

#define GAME_MODE_BEGINNER  0
#define GAME_MODE_NORMAL    1
#define GAME_MODE_REAL_LIFE 2
#define GAME_MODE_MONSTER   3
#define GAME_MODE_MAX       4

/*
 * Number of grids in each block (vertically)
 * Probably hard-coded to 11, see "generate.c"
 */
#define BLOCK_HGT    11

/*
 * Number of grids in each block (horizontally)
 * Probably hard-coded to 11, see "generate.c"
 */
#define BLOCK_WID    11


/*
 * Number of grids in each panel (vertically)
 * Must be a multiple of BLOCK_HGT
 */
#define PANEL_HGT    11

/*
 * Number of grids in each panel (horizontally)
 * Must be a multiple of BLOCK_WID
 */
#define PANEL_WID    33


/*
 * Number of grids used to display the dungeon (vertically).
 * Must be a multiple of 11, probably hard-coded to 22.
 */
#define SCREEN_HGT      22

/*
 * Number of grids used to display the dungeon (horizontally).
 * Must be a multiple of 33, probably hard-coded to 66.
 */
#define SCREEN_WID      66


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
 * Implement continuous wilderness scrolling (as a huge hack) by dividing
 * each wilderness square into a 3x3 grid (excluding the boundary sqaures).
 */
#define WILD_SCROLL_CY  (MAX_HGT/3)
#define WILD_SCROLL_CX  (MAX_WID/3)

/*
 * Quest constants
 */
#define MIN_RANDOM_QUEST    40
/*#define MAX_RANDOM_QUEST    (MIN_RANDOM_QUEST + num_random_quests - 1)*/

/* Check is the quest index is "fixed" */
#define is_fixed_quest_idx(Q_IDX) (((Q_IDX) < MIN_RANDOM_QUEST) || ((Q_IDX) >= MIN_RANDOM_QUEST + num_random_quests))

#define QUEST_OBERON         8
#define QUEST_SERPENT        9

/*
 * Arena constants
 */
#define MAX_ARENA_MONS        38    /* -KMW- */
#define ARENA_DEFEATED_OLD_VER (-(MAX_SHORT))


/*
 * Total number of stores (see "store.c", etc)
 */
#define MAX_STORES      11

/*
 * Number of buildings
 */
#define MAX_BLDG        32

/*
 * Store types
 */
#define STORE_GENERAL   0
#define STORE_ARMOURY   1
#define STORE_WEAPON    2
#define STORE_TEMPLE    3
#define STORE_ALCHEMIST 4
#define STORE_MAGIC     5
#define STORE_BLACK     6
#define STORE_HOME      7
#define STORE_BOOK      8
#define STORE_MUSEUM    9
#define STORE_JEWELER  10


#define BUILDING_NON_MEMBER 0
#define BUILDING_MEMBER     1
#define BUILDING_OWNER      2

/*
 * Total number of owners per store (see "store.c", etc)
 */
#define MAX_OWNERS      32

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

/* Monk martial arts... */
# define MAX_MA 17
# define MA_KNEE 1
# define MA_SLOW 2

/* Hallucination stuff */
#define MAX_SILLY_ATTACK 29

#define MAX_SPELLS            70 /* Possessing a GCAWDL gives 62 spells (and about 25 powers)! */

/* Spellcasters that don't use spell books */
#define MAX_MIND_POWERS  21

#define MIND_BERSERKER      0
#define MIND_MIRROR_MASTER  1
#define MIND_NINJUTSU       2

#define MIND_MAX_CLASSES    3


/*
 * Size of memory reserved for initialization of some arrays
 */
#define FAKE_NAME_SIZE  40 * 1024
#define FAKE_TEXT_SIZE 250 * 1024
#define FAKE_TAG_SIZE   10 * 1024


/*
 * Maximum number of high scores in the high score file
 */
#define MAX_HISCORES    999


/*
 * Maximum dungeon level.  The player can never reach this level
 * in the dungeon, and this value is used for various calculations
 * involving object and monster creation.  It must be at least 100.
 * Setting it below 128 may prevent the creation of some objects.
 */
#define MAX_DEPTH       128


/*
 * Maximum number of saved floors.
 */
#define MAX_SAVED_FLOORS       20


/*
 * Maximum size of the "lite" array (see "cave.c")
 * Note that the "lite radius" will NEVER exceed 14, and we would
 * never require more than 581 entries in the array for circular "lite".
 */
#define LITE_MAX 600

/*
 * Maximum size of the "mon_lite" array (see "cave.c")
 * Note that the "view radius" will NEVER exceed 20, monster illumination
 * flags are dependent on CAVE_VIEW, and even if the "view" was octagonal,
 * we would never require more than 1520 entries in the array.
 */
#define MON_LITE_MAX 1536

/*
 * Maximum size of the "view" array (see "cave.c")
 * Note that the "view radius" will NEVER exceed 20, and even if the "view"
 * was octagonal, we would never require more than 1520 entries in the array.
 */
#define VIEW_MAX 1536

/*
 * Maximum size of the "temp" array (see "cave.c")
 * We must be as large as "VIEW_MAX" and "LITE_MAX" for proper functioning
 * of "update_view()" and "update_lite()".  We must also be as large as the
 * largest illuminatable room, but no room is larger than 800 grids.  We
 * must also be large enough to allow "good enough" use as a circular queue,
 * to calculate monster flow, but note that the flow code is "paranoid".
 */
#define TEMP_MAX 2298

/*
 * Maximum size of the "redraw" array (see "cave.c")
 * We must be large for proper functioning of delayed redrawing.
 * We must also be as large as two times of the largest view area.
 * Note that maximum view grids are 1149 entries.
 */
#define REDRAW_MAX 2298


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
 * OPTION: Maximum number of messages to remember (see "io.c")
 * Default: assume maximal memorization of 2048 total messages
 */
#define MESSAGE_MAX     2048

/*
 * OPTION: Maximum space for the message text buffer (see "io.c")
 * Default: assume that each of the 2048 messages is repeated an
 * average of three times, and has an average length of 48
 */
#define MESSAGE_BUF     32768


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

/*
 * Special internal key
 */
#define SPECIAL_KEY_QUEST    255
#define SPECIAL_KEY_BUILDING 254
#define SPECIAL_KEY_STORE    253
#define SPECIAL_KEY_QUIT     252

/*
 * Store constants
 */
#define STORE_INVEN_MAX 24              /* Max number of discrete objs in inven */
#define STORE_CHOICES   48              /* Number of items to choose stock from */
#define STORE_OBJ_LEVEL 5               /* Magic Level for normal stores */
#define STORE_TURNOVER  9               /* Normal shop turnover, per day */
#define STORE_MIN_KEEP  6               /* Min slots to "always" keep full */
#define STORE_MAX_KEEP  18              /* Max slots to "always" keep full */
#define STORE_SHUFFLE   21              /* 1/Chance (per day) of an owner changing */
#define STORE_TICKS     1000            /* Number of ticks between turnovers */


/*
 * Random energy
 */
#define ENERGY_NEED() (randnor(100, 25))


/*
 * Extract energy from speed (Assumes that SPEED is unsigned)
 */
#define SPEED_TO_ENERGY(SPEED) \
    (((SPEED) > 199) ? 49 : extract_energy[(SPEED)])


/*
 * Misc constants
 */
#define TOWN_DAWN         10000    /* Number of ticks from dawn to dawn XXX */
#define TURNS_PER_TICK    10       /* Number of energy-gain-turns per ticks */
#define MAX_DAYS          20000    /* Maximum days */
#define BREAK_GLYPH       550      /* Rune of protection resistance */
#define BREAK_MON_TRAP    299      /* For explosive runes */
#define BTH_PLUS_ADJ       3       /* Adjust BTH per plus-to-hit */
#define MON_MULT_ADJ         8       /* High value slows multiplication */
#define MON_SUMMON_ADJ       2       /* Adjust level of summoned creatures */
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


#define CAN_TWO_HANDS_WIELDING() (!p_ptr->riding || (p_ptr->pet_extra_flags & PF_RYOUTE))


/*
 * There is a 1/20 (5%) chance of inflating the requested object_level
 * during the creation of an object (see "get_obj_num()" in "object.c").
 * Lower values yield better objects more often.
 */
#define GREAT_OBJ       10

/*
 * There is a 1/50 (2%) chance of inflating the requested monster_level
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
#define MAX_RANGE       (p_ptr->inside_battle ? 36 : 18)      /* Maximum range (spells, etc) */
#define AAF_LIMIT       100     /* Limit of sensing radius */
#define AAF_LIMIT_RING  255



/*
 * The town starts out with 4 residents during the day
 */
#define MIN_M_ALLOC_TD          4

/*
 * The town starts out with 8 residents during the night
 */
#define MIN_M_ALLOC_TN          8


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
#define CH_MUSIC        0x008000    /* This is 16th bit */
#define CH_HISSATSU     0x010000
#define CH_HEX          0x020000
#define CH_RAGE         0x040000
#define CH_BURGLARY     0x080000



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
#define REALM_CRAFT           8
#define REALM_DAEMON       9
#define REALM_CRUSADE      10
#define REALM_NECROMANCY   11
#define REALM_ARMAGEDDON   12
#define MAX_MAGIC          12
#define MIN_TECHNIC        16
#define REALM_MUSIC        16
#define REALM_HISSATSU     17
#define REALM_HEX          18
#define REALM_RAGE         19
#define REALM_BURGLARY     20
#define MAX_REALM          20

#define VALID_REALM        (MAX_REALM + MAX_MAGIC - MIN_TECHNIC + 1)
#define NUM_TECHNIC        (MAX_REALM - MIN_TECHNIC + 1)

#define is_magic(A) ((((A) > REALM_NONE) && ((A) < MAX_MAGIC + 1)) ? TRUE : FALSE)
#define tval2realm(A) ((A) - TV_LIFE_BOOK + 1)
#define realm2tval(A) ((A) + TV_LIFE_BOOK - 1)
#define technic2magic(A)      (is_magic(A) ? (A) : (A) - MIN_TECHNIC + 1 + MAX_MAGIC)
#define is_good_realm(REALM)   ((REALM) == REALM_LIFE || (REALM) == REALM_CRUSADE)
#define is_evil_realm(REALM)   ((REALM) == REALM_DEATH || (REALM) == REALM_DAEMON)

/*
 * Magic-books for the realms
 */
#define REALM1_BOOK     (p_ptr->realm1 + TV_LIFE_BOOK - 1)
#define REALM2_BOOK     (p_ptr->realm2 + TV_LIFE_BOOK - 1)

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

/*
 * Maximum number of "normal" pack slots, and the index of the "overflow"
 * slot, which can hold an item, but only temporarily, since it causes the
 * pack to "overflow", dropping the "last" item onto the ground.
 */
#define INVEN_PACK      26
#define EQUIP_BEGIN     (INVEN_PACK + 1)
#define INVEN_TOTAL     50
#define EQUIP_MAX_SLOTS (INVEN_TOTAL - EQUIP_BEGIN)

/* If you are looking for old INVEN_* crap, look in equip.h instead ... */

/*
 * Fake inventory slot for selecting force (hard-coded).
 */
#define INVEN_FORCE               1111
#define INVEN_UNLIMITED_QUIVER    1112
#define INVEN_ALL                 1113

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


/*
 * Player race constants (hard-coded by save-files, arrays, etc)
 */
#define RACE_HUMAN               0
#define RACE_TONBERRY            1
#define RACE_DEMIGOD             2
#define RACE_HOBBIT              3
#define RACE_GNOME               4
#define RACE_DWARF               5
#define RACE_SNOTLING            6
#define RACE_HALF_TROLL          7
#define RACE_AMBERITE            8
#define RACE_HIGH_ELF            9
#define RACE_BARBARIAN          10
#define RACE_HALF_OGRE          11
#define RACE_HALF_GIANT         12
#define RACE_HALF_TITAN         13
#define RACE_CYCLOPS            14
#define RACE_YEEK               15
#define RACE_KLACKON            16
#define RACE_KOBOLD             17
#define RACE_NIBELUNG           18
#define RACE_DARK_ELF           19
#define RACE_DRACONIAN          20
#define RACE_MIND_FLAYER        21
#define RACE_IMP                22
#define RACE_GOLEM              23
#define RACE_SKELETON           24
#define RACE_ZOMBIE             25
#define RACE_VAMPIRE            26
#define RACE_SPECTRE            27
#define RACE_SPRITE             28
#define RACE_BEASTMAN           29
#define RACE_ENT                30
#define RACE_ARCHON             31
#define RACE_BALROG             32
#define RACE_DUNADAN            33
#define RACE_SHADOW_FAIRY       34
#define RACE_KUTAR              35
#define RACE_ANDROID            36
#define RACE_DOPPELGANGER       37
#define RACE_MON_JELLY          38
#define RACE_MON_SPIDER         39
#define RACE_MON_DRAGON         40
#define RACE_MON_LICH           41
#define RACE_MON_XORN           42
#define RACE_MON_ANGEL          43
#define RACE_MON_HOUND          44
#define RACE_MON_GIANT          45
#define RACE_MON_BEHOLDER       46
#define RACE_MON_DEMON          47
#define RACE_MON_HYDRA          48
#define RACE_MON_LEPRECHAUN     49
#define RACE_MON_TROLL          50
#define RACE_CENTAUR            51
#define RACE_MON_ELEMENTAL      52
#define RACE_MON_SWORD          53
#define RACE_MON_GOLEM          54
#define RACE_MON_QUYLTHULG      55
#define RACE_MON_POSSESSOR      56
#define RACE_MON_VAMPIRE        57
#define RACE_MON_RING           58
#define RACE_MON_MIMIC          59
#define RACE_WOOD_ELF           60
#define RACE_MON_CENTIPEDE      61
/* TODO: New races are in progress! ... */
    #define RACE_MON_GHOST          62
    #define RACE_MON_ZOMBIE         63
    #define MAX_RACES               62 /* TODO: Bump me!! */

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

#define MAX_DEMIGOD_TYPES        13
#define MAX_DEMIGOD_POWERS        5

#define SPIDER_PHASE  0
#define SPIDER_ARANEA 1
#define SPIDER_MAX    2

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
#define GOLEM_SPELLWARP 2
#define GOLEM_MAX       3

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

/* Mimicry uses races too ... Its just that players
   cannot choose these races during birth. */
#define MIMIC_NONE            -1                  /* RACE_HUMAN is 0 and Dopplegangers can mimic humans! */

enum _mimic_types {
    MIMIC_DEMON = 1000,
    MIMIC_DEMON_LORD,
    MIMIC_VAMPIRE,
    MIMIC_CLAY_GOLEM,
    MIMIC_IRON_GOLEM,
    MIMIC_MITHRIL_GOLEM,
    MIMIC_COLOSSUS,
    MIMIC_SMALL_KOBOLD,
    MIMIC_MANGY_LEPER,
    /* Vampires only, please :) */
    MIMIC_BAT,
    MIMIC_MIST,
    MIMIC_WOLF,
    MIMIC_MAX
};

/*#define prace_is_(A) (p_ptr->mimic_form == (A) || (p_ptr->mimic_form == MIMIC_NONE && (A) < MAX_RACES && p_ptr->prace == (A)))*/
#define psubclass_is_(A, B) (p_ptr->pclass == (A) && p_ptr->psubclass == (B))
#define weaponmaster_is_(B) (p_ptr->pclass == CLASS_WEAPONMASTER && p_ptr->psubclass == (B))
#define warlock_is_(B) (p_ptr->pclass == CLASS_WARLOCK && p_ptr->psubclass == (B))
#define devicemaster_is_(B) (p_ptr->pclass == CLASS_DEVICEMASTER && p_ptr->psubclass == (B))
#define demigod_is_(B) (prace_is_(RACE_DEMIGOD) && p_ptr->psubrace == (B))
#define dragon_is_(B) (prace_is_(RACE_MON_DRAGON) && p_ptr->psubrace == (B))
#define giant_is_(B) (prace_is_(RACE_MON_GIANT) && p_ptr->psubrace == (B))
#define demon_is_(B) (prace_is_(RACE_MON_DEMON) && p_ptr->psubrace == (B))
#define elemental_is_(B) (prace_is_(RACE_MON_ELEMENTAL) && p_ptr->psubrace == (B))
#define draconian_is_(B) (prace_is_(RACE_DRACONIAN) && p_ptr->psubrace == (B))


/*
 * Player class constants (hard-coded by save-files, arrays, etc)
 */
#define CLASS_WARRIOR            0
#define CLASS_MAGE               1
#define CLASS_PRIEST             2
#define CLASS_ROGUE              3
#define CLASS_RANGER             4
#define CLASS_PALADIN            5
#define CLASS_WARRIOR_MAGE       6
#define CLASS_CHAOS_WARRIOR      7
#define CLASS_MONK               8
#define CLASS_MINDCRAFTER        9
#define CLASS_HIGH_MAGE         10
#define CLASS_TOURIST           11
#define CLASS_IMITATOR          12
#define CLASS_BEASTMASTER       13
#define CLASS_SORCERER          14
#define CLASS_ARCHER            15
#define CLASS_MAGIC_EATER       16
#define CLASS_BARD              17
#define CLASS_RED_MAGE          18
#define CLASS_SAMURAI           19
#define CLASS_FORCETRAINER      20
#define CLASS_BLUE_MAGE         21
#define CLASS_CAVALRY           22
#define CLASS_BERSERKER         23
#define CLASS_WEAPONSMITH       24
#define CLASS_MIRROR_MASTER     25
#define CLASS_NINJA             26
#define CLASS_SNIPER            27
#define CLASS_TIME_LORD         28
#define CLASS_BLOOD_KNIGHT        29
#define CLASS_WARLOCK            30
#define CLASS_ARCHAEOLOGIST        31
#define CLASS_DUELIST            32
#define CLASS_WILD_TALENT        33
#define CLASS_RUNE_KNIGHT        34
#define CLASS_WEAPONMASTER        35
#define CLASS_BLOOD_MAGE        36
#define CLASS_NECROMANCER        37
#define CLASS_PSION                38
#define CLASS_RAGE_MAGE            39
#define CLASS_SCOUT             40
#define CLASS_MAULER            41
#define CLASS_MONSTER           42
#define CLASS_MYSTIC            43
#define CLASS_DEVICEMASTER      44

#define MAX_CLASS               45

/* Warlock Pacts ... stored in p_ptr->psubclass */
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

/* Weaponmaster Specialities ... stored in p_ptr->psubclass */
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


#define PERS_ORDINARY      0
#define PERS_MIGHTY        1
#define PERS_SHREWD        2
#define PERS_PIOUS         3
#define PERS_NIMBLE        4
#define PERS_FEARLESS      5
#define PERS_COMBAT        6
#define PERS_LAZY          7
#define PERS_SEXY          8
#define PERS_LUCKY         9
#define PERS_PATIENT      10
#define PERS_MUNCHKIN     11
#define PERS_CRAVEN       12
#define PERS_HASTY        13
#define MAX_PERSONALITIES 14

/*
 * Number of feats we change to (Excluding default). Used in f_info.txt.
 */
#define MAX_FEAT_STATES     8


/*
 * Wilderness terrains
 * Note: parse_v_info in init1.c relies on the order below for parsing external files!
 * Note: room_template_t subtype uses these terrain values for generating appropriate wilderness encounters.
 */
#define TERRAIN_EDGE             0 /* Edge of the World */
#define TERRAIN_TOWN             1
#define TERRAIN_DEEP_WATER       2
#define TERRAIN_SHALLOW_WATER    3
#define TERRAIN_SWAMP            4
#define TERRAIN_DIRT             5
#define TERRAIN_GRASS            6
#define TERRAIN_TREES            7
#define TERRAIN_DESERT           8
#define TERRAIN_SHALLOW_LAVA     9
#define TERRAIN_DEEP_LAVA       10
#define TERRAIN_MOUNTAIN        11

#define MAX_WILDERNESS          12 /* Maximum wilderness index */


/*
 * Feature flags - should be used instead of feature indexes unless generating.
 * Originally from UnAngband, and modified into TR-like style in Hengband
 */


#define FF_LOS           0
#define FF_PROJECT       1
#define FF_MOVE          2
#define FF_PLACE         3
#define FF_DROP          4
#define FF_SECRET        5
#define FF_NOTICE        6
#define FF_REMEMBER      7
#define FF_OPEN          8
#define FF_CLOSE         9
#define FF_BASH          10
#define FF_SPIKE         11
#define FF_DISARM        12
#define FF_STORE         13
#define FF_TUNNEL        14
#define FF_MAY_HAVE_GOLD 15
#define FF_HAS_GOLD      16
#define FF_HAS_ITEM      17
#define FF_DOOR          18
#define FF_TRAP          19
#define FF_STAIRS        20
#define FF_GLYPH         21
#define FF_LESS          22
#define FF_MORE          23
#define FF_AVOID_RUN     24
#define FF_FLOOR         25
#define FF_WALL          26
#define FF_PERMANENT     27
/* #define FF_XXX00         28 */
/* #define FF_XXX01         29 */
/* #define FF_XXX02         30 */
#define FF_HIT_TRAP      31

/* #define FF_BRIDGE        32 */
/* #define FF_RIVER         33 */
/* #define FF_LAKE          34 */
/* #define FF_BRIDGED       35 */
/* #define FF_COVERED       36 */
#define FF_GLOW          37
#define FF_ENSECRET      38
#define FF_WATER         39
#define FF_LAVA          40
#define FF_SHALLOW       41
#define FF_DEEP          42
/* #define FF_FILLED        43 */
#define FF_HURT_ROCK     44
/* #define FF_HURT_FIRE     45 */
/* #define FF_HURT_COLD     46 */
/* #define FF_HURT_ACID     47 */
/* #define FF_ICE           48 */
/* #define FF_ACID          49 */
/* #define FF_OIL           50 */
/* #define FF_XXX04      51 */
/* #define FF_CAN_CLIMB     52 */
#define FF_CAN_FLY       53
#define FF_CAN_SWIM      54
#define FF_CAN_PASS      55
/* #define FF_CAN_OOZE      56 */
#define FF_CAN_DIG       57
/* #define FF_HIDE_ITEM     58 */
/* #define FF_HIDE_SNEAK    59 */
/* #define FF_HIDE_SWIM     60 */
/* #define FF_HIDE_DIG      61 */
/* #define FF_KILL_HUGE     62 */
/* #define FF_KILL_MOVE     63 */

/* #define FF_PICK_TRAP     64 */
/* #define FF_PICK_DOOR     65 */
/* #define FF_ALLOC         66 */
/* #define FF_CHEST         67 */
/* #define FF_DROP_1D2      68 */
/* #define FF_DROP_2D2      69 */
/* #define FF_DROP_GOOD     70 */
/* #define FF_DROP_GREAT    71 */
/* #define FF_HURT_POIS     72 */
/* #define FF_HURT_ELEC     73 */
/* #define FF_HURT_WATER    74 */
/* #define FF_HURT_BWATER   75 */
/* #define FF_USE_FEAT      76 */
/* #define FF_GET_FEAT      77 */
/* #define FF_GROUND        78 */
/* #define FF_OUTSIDE       79 */
/* #define FF_EASY_HIDE     80 */
/* #define FF_EASY_CLIMB    81 */
/* #define FF_MUST_CLIMB    82 */
#define FF_TREE          83
/* #define FF_NEED_TREE     84 */
/* #define FF_BLOOD         85 */
/* #define FF_DUST          86 */
/* #define FF_SLIME         87 */
#define FF_PLANT         88
/* #define FF_XXX2          89 */
/* #define FF_INSTANT       90 */
/* #define FF_EXPLODE       91 */
/* #define FF_TIMED         92 */
/* #define FF_ERUPT         93 */
/* #define FF_STRIKE        94 */
/* #define FF_SPREAD        95 */

#define FF_SPECIAL       96
#define FF_HURT_DISI     97
#define FF_QUEST_ENTER   98
#define FF_QUEST_EXIT    99
#define FF_QUEST         100
#define FF_SHAFT         101
#define FF_MOUNTAIN      102
#define FF_BLDG          103
#define FF_MON_TRAP      104
#define FF_PATTERN       105
#define FF_TOWN          106
#define FF_ENTRANCE      107
#define FF_MIRROR        108
#define FF_UNPERM        109
#define FF_TELEPORTABLE  110
#define FF_CONVERT       111
#define FF_GLASS         112
#define FF_ROGUE_TRAP_1  113
#define FF_ROGUE_TRAP_2  114
#define FF_ROGUE_TRAP_3  115
#define FF_WEB           116
#define FF_FLAG_MAX      117
#define FF_FLAG_SIZE     (1 + ((FF_FLAG_MAX - 1) / 32))

/* Which features are dynamic */
#define have_dynamic_flags(ARRAY) \
    (!!((ARRAY)[(FF_INSTANT / 32)] & \
        ((1UL << (FF_INSTANT % 32)) | \
         (1UL << (FF_EXPLODE % 32)) | \
         (1UL << (FF_TIMED % 32)) | \
         (1UL << (FF_ERUPT % 32)) | \
         (1UL << (FF_STRIKE % 32)) | \
         (1UL << (FF_SPREAD % 32)))))


/*
 * Feature action flags
 */
#define FAF_DESTROY     0x01
#define FAF_NO_DROP     0x02
#define FAF_CRASH_GLASS 0x04


/*
 * Bit flags for teleportation
 */
#define TELEPORT_NONMAGICAL    0x00000001
#define TELEPORT_PASSIVE       0x00000002
#define TELEPORT_DEC_VALOUR    0x00000004
#define TELEPORT_LINE_OF_SIGHT 0x00000008
#define TELEPORT_DISENGAGE       0x00000010


/* Types of doors */
#define DOOR_DEFAULT    -1
#define DOOR_DOOR        0
#define DOOR_GLASS_DOOR  1
#define DOOR_CURTAIN     2

#define MAX_DOOR_TYPES   3

#define feat_locked_door_random(DOOR_TYPE) \
    (feat_door[(DOOR_TYPE)].num_locked ? \
     feat_door[(DOOR_TYPE)].locked[randint0(feat_door[(DOOR_TYPE)].num_locked)] : feat_none)

#define feat_jammed_door_random(DOOR_TYPE) \
    (feat_door[(DOOR_TYPE)].num_jammed ? \
     feat_door[(DOOR_TYPE)].jammed[randint0(feat_door[(DOOR_TYPE)].num_jammed)] : feat_none)


/* Types of normal traps */
#define NOT_TRAP        -1
#define TRAP_TRAPDOOR    0
#define TRAP_PIT         1
#define TRAP_SPIKED_PIT  2
#define TRAP_POISON_PIT  3
#define TRAP_TY_CURSE    4
#define TRAP_TELEPORT    5
#define TRAP_FIRE        6
#define TRAP_ACID        7
#define TRAP_SLOW        8
#define TRAP_LOSE_STR    9
#define TRAP_LOSE_DEX   10
#define TRAP_LOSE_CON   11
#define TRAP_BLIND      12
#define TRAP_CONFUSE    13
#define TRAP_POISON     14
#define TRAP_SLEEP      15
#define TRAP_TRAPS      16
#define TRAP_ALARM      17


/* Types of special traps */
#define TRAP_OPEN       18
#define TRAP_ARMAGEDDON 19
#define TRAP_PIRANHA    20


/* Maximum locked/jammed doors */
#define MAX_LJ_DOORS 8


/* Types of pattern tiles */
#define NOT_PATTERN_TILE      -1
#define PATTERN_TILE_START    0
#define PATTERN_TILE_1        1
#define PATTERN_TILE_2        2
#define PATTERN_TILE_3        3
#define PATTERN_TILE_4        4
#define PATTERN_TILE_END      5
#define PATTERN_TILE_OLD      6
#define PATTERN_TILE_TELEPORT 7
#define PATTERN_TILE_WRECKED  8


/* Types of conversions */
#define CONVERT_TYPE_FLOOR   0
#define CONVERT_TYPE_WALL    1
#define CONVERT_TYPE_INNER   2
#define CONVERT_TYPE_OUTER   3
#define CONVERT_TYPE_SOLID   4
#define CONVERT_TYPE_STREAM1 5
#define CONVERT_TYPE_STREAM2 6


/*
 * Bit flags for the *_can_enter() and monster_can_cross_terrain()
 */
#define CEM_RIDING              0x0001
#define CEM_P_CAN_ENTER_PATTERN 0x0002


/* Lighting levels of features' attr and char */

#define F_LIT_STANDARD 0 /* Standard */
#define F_LIT_LITE     1 /* Brightly lit */
#define F_LIT_DARK     2 /* Darkened */

#define F_LIT_NS_BEGIN 1 /* Nonstandard */
#define F_LIT_MAX      3


/*** Artifact indexes (see "lib/edit/a_info.txt") ***/

/* Lites */
#define ART_GALADRIEL            1
#define ART_ELENDIL              2
#define ART_JUDGE                3
#define ART_EDISON               7
#define ART_PALANTIR             15
#define ART_STONE_LORE           17
#define ART_FLY_STONE            147

/* Amulets */
#define ART_CARLAMMAS            4
#define ART_INGWE                5
#define ART_DWARVES              6
#define ART_FARAMIR              18
#define ART_BOROMIR              143
#define ART_MAGATAMA             149
#define ART_INROU                166
#define ART_NIGHT                215
#define ART_SACRED_KNIGHTS       217
#define ART_HELL                 218
#define ART_CHARMED              219
#define ART_GOGO                 220

/* Rings */
#define ART_FRAKIR               8
#define ART_TULKAS               9
#define ART_NARYA               10
#define ART_NENYA               11
#define ART_VILYA               12
#define ART_POWER               13
#define ART_AHO                 14

/* Dragon Scale */
#define ART_RAZORBACK           129
#define ART_BLADETURNER         130
#define ART_SEIRYU              201

/* Hard Armour */
#define ART_SOULKEEPER          19
#define ART_ISILDUR             20
#define ART_ROHIRRIM            21
#define ART_LOHENGRIN           22
#define ART_JULIAN              23
#define ART_ARVEDUI             24
#define ART_CASPANION           25
#define ART_GILES               168
#define ART_MORLOK              203
#define ART_VETERAN             206

/* Soft Armour */
#define ART_SHIVA_JACKET        26
#define ART_HITHLOMIR           27
#define ART_THALKETTOTH         28
#define ART_HIMRING             127
#define ART_INCANUS             131
#define ART_NAMAKE_ARMOR        183
#define ART_GHB                 192
#define ART_DASAI               200
#define ART_KESHO               204

/* Shields */
#define ART_THORIN              30
#define ART_CELEGORM            31
#define ART_ANARION             32
#define ART_GIL_GALAD           138
#define ART_YENDOR              141
#define ART_YATA                151
#define ART_EARENDIL            186
#define ART_PERSEUS             197

/* Helms and Crowns */
#define ART_INDRA               33
#define ART_CHAOS               34
#define ART_BERUTHIEL           35
#define ART_THRANDUIL           36
#define ART_THENGEL             37
#define ART_HAMMERHAND          38
#define ART_DOR                 39
#define ART_HOLHENNETH          40
#define ART_TERROR              41
#define ART_AMBER               42
#define ART_NUMENOR             132
#define ART_STONEMASK           146

/* Cloaks */
#define ART_JACK                43
#define ART_COLLUIN             44
#define ART_HOLCOLLETH          45
#define ART_THINGOL             46
#define ART_THORONGIL           47
#define ART_COLANNON            48
#define ART_LUTHIEN             49
#define ART_TUOR                50
#define ART_MOOK                205
#define ART_HEAVENLY_MAIDEN     233

/* Gloves */
#define ART_CAMBELEG            52
#define ART_CAMMITHRIM          53
#define ART_PAURHACH            54
#define ART_CORWIN              55
#define ART_PAURAEGEN           56
#define ART_PAURNEN             57
#define ART_THANOS              58
#define ART_FINGOLFIN           59
#define ART_PAURNIMMEN          185

/* Boots */
#define ART_FEANOR              60
#define ART_FLORA               61
#define ART_THROR               62
#define ART_SHIVA_BOOTS         63
#define ART_GLASS               165
#define ART_GETA                210

/* Digging */
#define ART_NAIN                211

/* Swords */
#define ART_MAEDHROS            64
#define ART_CAINE               65
#define ART_NARTHANC            66
#define ART_NIMTHANC            67
#define ART_DETHANC             68
#define ART_RILIA               69
#define ART_FIONA               70
#define ART_CALRIS              71
#define ART_GRAYSWANDIR         72
#define ART_GLAMDRING           73
#define ART_NOTHUNG             74
#define ART_ORCRIST             75
#define ART_GURTHANG            76
#define ART_ZARCUTHRA           77
#define ART_TWILIGHT            78
#define ART_GONDRICAM           79
#define ART_CRISDURIAN          80
#define ART_AGLARANG            81
#define ART_RINGIL              82
#define ART_ANDURIL             83
#define ART_WEREWINDLE          84
#define ART_CHAINSWORD          85
#define ART_FORASGIL            86
#define ART_CARETH              87
#define ART_STING               88
#define ART_SOULSWORD           89
#define ART_MERLIN              90
#define ART_DOOMCALLER          91
#define ART_VORPAL_BLADE        92
#define ART_SLAYER              123
#define ART_KUSANAGI            128
#define ART_HURIN               133
#define ART_AZAGHAL             135
#define ART_NOVA                137
#define ART_CHARIOT             140
#define ART_WORPAL_BLADE        142
#define ART_MURAMASA            144
#define ART_ZANTETSU            150
#define ART_SOULCRUSH           154
#define ART_FALIS               155
#define ART_HRUNTING            156
#define ART_ANUBIS              158
#define ART_GURENKI             160
#define ART_DR_JONES            162
#define ART_TAILBITER           167
#define ART_MUSASI_KATANA       171
#define ART_MUSASI_WAKIZASI     172
#define ART_QUICKTHORN          174
#define ART_TINYTHORN           175
#define ART_EXCALIBUR           176
#define ART_EXCALIPUR           177
#define ART_EXCALIBUR_J         179
#define ART_ARUNRUTH            184
#define ART_HAKABUSA            189
#define ART_STORMBRINGER        190
#define ART_NARSIL              191
#define ART_KANNU               193
#define ART_GRIMTOOTH           196
#define ART_KAMUI               198
#define ART_GOURYU              207
#define ART_EOWYN               216
#define ART_SPECTRAL_DSM        226
#define ART_BLOODRIP            243
#define ART_MAGLOR                245
#define ART_DAERON                246
#define ART_MASTER_TONBERRY        247
#define ART_DUELIST                248
#define ART_ETERNAL_BLADE       294

/* Polearms */
#define ART_THEODEN             93
#define ART_PAIN                94
#define ART_OSONDIR             95
#define ART_TIL                 96
#define ART_RUNESPEAR           97
#define ART_DESTINY             98
#define ART_HAGEN               99
#define ART_EORLINGAS           100
#define ART_DURIN               101
#define ART_EONWE               102
#define ART_BALLI               103
#define ART_LOTHARANG           104
#define ART_DWARVES_AXE         105
#define ART_BARUKKHELED         106
#define ART_WRATH               107
#define ART_ULMO                108
#define ART_AVAVIR              109
#define ART_BENKEI              152
#define ART_TAIKOBO             159
#define ART_TONBO               161
#define ART_GAEBOLG             163
#define ART_ARRYU               164
#define ART_AEGLOS              187
#define ART_BLOOD               199
#define ART_NUMAHOKO            202
#define ART_DRAGONLANCE         322

/* The sword of the Dawn */
#define ART_DAWN                110

/* Hafted */
#define ART_GROND               111
#define ART_TOTILA              112
#define ART_THUNDERFIST         113
#define ART_BLOODSPIKE          114
#define ART_FIRESTAR            115
#define ART_TARATOL             116
#define ART_AULE                117
#define ART_NAR                 118
#define ART_ERIRIL              119
#define ART_GANDALF             120
#define ART_SARUMAN             249
#define ART_DEATHWREAKER        121
#define ART_TURMIL              122
#define ART_MJOLLNIR            136
#define ART_WINBLOWS            139
#define ART_XIAOLONG            145
#define ART_NYOIBOU             157
#define ART_JONES               162
#define ART_HYOUSIGI            169
#define ART_MATOI               170
#define ART_IRON_BALL           173
#define ART_SAMSON              178
#define ART_NAMAKE_HAMMER       181
#define ART_BOLISHOI            188
#define ART_SHUTEN_DOJI         194
#define ART_G_HAMMER            195
#define ART_AEGISFANG           208
#define ART_HERMIT              209
#define ART_GOTHMOG             212
#define ART_JIZO                213
#define ART_FUNDIN              214
#define ART_AESCULAPIUS         225
#define ART_AEGIR                251
#define ART_DEFENDER_OF_THE_CROWN 252
#define ART_MONKEY_KING            255
#define ART_MAUL_OF_VICE        279

/* Bows */
#define ART_BELTHRONDING        124
#define ART_BARD                125
#define ART_BRAND               126
#define ART_CRIMSON             16
#define ART_BUCKLAND            134
#define ART_YOICHI              148
#define ART_HARAD               180
#define ART_NAMAKE_BOW          182
#define ART_ROBIN_HOOD          221
#define ART_HELLFIRE            222

/* Arrows */
#define ART_BARD_ARROW          153

#define ART_ETERNITY            244
#define ART_ZEUS            256
#define ART_POSEIDON        257
#define ART_HADES            258
#define ART_ATHENA            259
#define ART_ARES            260
#define ART_HERMES            261
#define ART_APOLLO            262
#define ART_ARTEMIS            263
#define ART_HEPHAESTUS        264
#define ART_HERA            265
#define ART_DEMETER            266
#define ART_APHRODITE        267
#define ART_HAND_OF_VECNA    268
#define ART_EYE_OF_VECNA    269
#define ART_CUPIDS_ARROW    270
#define ART_BALLISTA        271
#define ART_KAMIKAZE_ROBE   272
#define ART_RAILGUN         273
#define ART_ASSASSINATOR    275
#define ART_STOMPER         277
#define ART_GONG            278

#define ART_STONE_OF_NATURE     282
#define ART_STONE_OF_LIFE       283
#define ART_STONE_OF_SORCERY    284
#define ART_STONE_OF_CHAOS      285
#define ART_STONE_OF_DEATH      286
#define ART_STONE_OF_TRUMP      287
#define ART_STONE_OF_DAEMON     288
#define ART_STONE_OF_CRUSADE    289
#define ART_STONE_OF_CRAFT      290
#define ART_STONE_OF_WAR        291
#define ART_STONE_OF_ARMAGEDDON 297
#define ART_STONE_OF_MIND       328

#define ART_HOLY_GRAIL      293

#define ART_UBBO_SATHLA     298
#define ART_UNGOLIANT       299
#define ART_GLAURUNG        300
#define ART_VECNA           301
#define ART_CARCHAROTH      302
#define ART_YMIR            303
#define ART_TYPHOEUS        304
#define ART_KRONOS          305
#define ART_OMARAX          306
#define ART_LERNEAN         307
#define ART_FANG            308
#define ART_WOLF            309
#define ART_GRIP            310
#define ART_OREMORJ         311
#define ART_KHORNE          312
#define ART_MEPHISTOPHELES  313
#define ART_ULIK            314
#define ART_QUAKER          315
#define ART_ARIEL           316
#define ART_MOIRE           317
#define ART_LOGE            318
#define ART_EMPEROR_QUYLTHULG 319
#define ART_DESTROYER       320
#define ART_ATLAS           321
#define ART_MULTIHUED_CENTIPEDE 327

/*** Object "tval" and "sval" codes ***/


/*
 * The values for the "tval" field of various objects.
 *
 * This value is the primary means by which items are sorted in the
 * player inventory, followed by "sval" and "cost".
 */

#define TV_SKELETON      1      /* Skeletons ('s'), not specified */
#define TV_BOTTLE        2
#define TV_JUNK          3      /* Sticks, Pottery, etc ('~') */
#define TV_WHISTLE       4
#define TV_SPIKE         5
#define TV_CHEST         7
#define TV_FIGURINE      8
#define TV_STATUE        9
#define TV_CORPSE       10      /* Corpses and Skeletons, specific */
#define TV_CAPTURE      11      /* Monster ball */
#define TV_NO_AMMO      15      /* Ammo for crimson?? */
#define TV_SHOT         16
#define TV_ARROW        17
#define TV_BOLT         18
#define TV_BOW          19
#define TV_DIGGING      20
#define TV_HAFTED       21      /* Priest Weapons */
#define TV_POLEARM      22      /* Axes and Pikes */
#define TV_SWORD        23      /* Edged Weapons */
#define TV_BOOTS        30
#define TV_GLOVES       31
#define TV_HELM         32
#define TV_CROWN        33
#define TV_SHIELD       34
#define TV_CLOAK        35
#define TV_SOFT_ARMOR   36
#define TV_HARD_ARMOR   37
#define TV_DRAG_ARMOR   38
#define TV_LITE         39
#define TV_AMULET       40
#define TV_RING         45
#define TV_CARD         50
#define TV_STAFF        55
#define TV_WAND         65
#define TV_ROD          66
#define TV_PARCHMENT    69
#define TV_SCROLL       70
#define TV_POTION       75
#define TV_FLASK        77
#define TV_FOOD         80
#define TV_RUNE            81
#define TV_LIFE_BOOK    90
#define TV_SORCERY_BOOK 91
#define TV_NATURE_BOOK  92
#define TV_CHAOS_BOOK   93
#define TV_DEATH_BOOK   94
#define TV_TRUMP_BOOK   95
#define TV_ARCANE_BOOK  96
#define TV_CRAFT_BOOK   97
#define TV_DAEMON_BOOK  98
#define TV_CRUSADE_BOOK 99
#define TV_NECROMANCY_BOOK 100
#define TV_ARMAGEDDON_BOOK 101
#define TV_MUSIC_BOOK   105
#define TV_HISSATSU_BOOK 106
#define TV_HEX_BOOK     107
#define TV_RAGE_BOOK    108
#define TV_BURGLARY_BOOK 109
#define TV_GOLD         127     /* Gold can only be picked up by players */

#define TV_EQUIP_BEGIN    TV_SHOT
#define TV_EQUIP_END      TV_CARD
#define TV_MISSILE_BEGIN  TV_SHOT
#define TV_MISSILE_END    TV_BOLT
#define TV_WEARABLE_BEGIN TV_BOW
#define TV_WEARABLE_END   TV_CARD
#define TV_WEAPON_BEGIN   TV_BOW
#define TV_WEAPON_END     TV_SWORD
#define TV_ARMOR_BEGIN    TV_BOOTS
#define TV_ARMOR_END      TV_DRAG_ARMOR

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
#define SV_PHOTO            50

/* The "sval" codes for TV_CORPSE */
#define SV_SKELETON             0
#define SV_CORPSE            1

/* The "sval" codes for TV_SHOT/TV_ARROW/TV_BOLT */
#define SV_AMMO_LIGHT                    0    /* pebbles */
#define SV_AMMO_NORMAL                   1    /* shots, arrows, bolts */
#define SV_AMMO_HEAVY                    2    /* seeker arrows and bolts, mithril shots */

/* The "sval" codes for TV_BOW. The weird sequencing is historic (Previously,
   sval%10 gave the bow multiplier, but this is now specified in k_info, etc). */
#define SV_SLING                         2
#define SV_SHORT_BOW                    12
#define SV_LONG_BOW                     13
#define SV_LIGHT_XBOW                   23
#define SV_HEAVY_XBOW                   24
#define SV_CRIMSON                      50
#define SV_RAILGUN                      51
#define SV_NAMAKE_BOW                   63
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
#define SV_NAMAKE_HAMMER                63    /* 1d77 */

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
#define SV_TSURIZAO                     40    /* 1d1 */
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
#define SV_RUNESWORD                    34  /* 0d0 */
#define SV_DRAGON_FANG                  35  /* 1d8 */

#define SV_FALCON_SWORD                    33
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
#define SV_WIZARD_CLOAK                  8

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

/* The sval codes for TV_LITE */
#define SV_LITE_TORCH                    0
#define SV_LITE_LANTERN                  1
#define SV_LITE_FEANOR                   2
#define SV_LITE_EDISON                   3
#define SV_LITE_GALADRIEL                4
#define SV_LITE_ELENDIL                  5
#define SV_LITE_JUDGE                    6
#define SV_LITE_LORE                     7
#define SV_LITE_PALANTIR                 8
#define SV_LITE_FLY_STONE                9
#define SV_LITE_EYE                        10
#define SV_LITE_NATURE                  11
#define SV_LITE_LIFE                    12
#define SV_LITE_SORCERY                 13
#define SV_LITE_CHAOS                   14
#define SV_LITE_DEATH                   15
#define SV_LITE_TRUMP                   16
#define SV_LITE_DAEMON                  17
#define SV_LITE_CRUSADE                 18
#define SV_LITE_CRAFT                   19
#define SV_LITE_WAR                     20
#define SV_LITE_ARMAGEDDON              21
#define SV_LITE_HYDRA                   22
#define SV_LITE_MIND                    23

#define SV_AMULT                         0
#define SV_RING                          0
#define SV_EXPRESS_CARD                  0

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
#define SV_SCROLL_MADNESS               56
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
#define SV_POTION_NEO_TSUYOSHI          64
#define SV_POTION_TSUYOSHI              65
    #define SV_POTION_POLYMORPH             66
#define SV_POTION_BLOOD                    67
#define SV_POTION_GIANT_STRENGTH        68
#define SV_POTION_STONE_SKIN            69
#define SV_POTION_CLARITY               70
#define SV_POTION_GREAT_CLARITY         71

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


#define OBJ_GOLD_LIST   480     /* First "gold" entry */
#define MAX_GOLD        18      /* Number of "gold" entries */


/*** General flag values ***/


/*
 * Special cave grid flags
 */
#define CAVE_MARK       0x0001    /* memorized feature */
#define CAVE_GLOW       0x0002    /* self-illuminating */
#define CAVE_ICKY       0x0004    /* part of a vault */
#define CAVE_ROOM       0x0008    /* part of a room */
#define CAVE_LITE       0x0010    /* lite flag  */
#define CAVE_VIEW       0x0020    /* view flag */
#define CAVE_TEMP       0x0040    /* temp flag */
#define CAVE_XTRA       0x0080    /* misc flag */
#define CAVE_MNLT       0x0100    /* Illuminated by monster */
#define CAVE_MNDK       0x8000    /* Darken by monster */

/* Used only while cave generation */
#define CAVE_FLOOR      0x0200
#define CAVE_EXTRA      0x0400
#define CAVE_INNER      0x0800
#define CAVE_OUTER      0x1000
#define CAVE_SOLID      0x2000
#define CAVE_VAULT      0x4000
#define CAVE_MASK (CAVE_FLOOR | CAVE_EXTRA | CAVE_INNER | CAVE_OUTER | CAVE_SOLID | CAVE_VAULT)

/* Used only after cave generation */
#define CAVE_NOTE       0x0400    /* Flag for delayed visual update (needs note_spot()) */
#define CAVE_REDRAW     0x0800    /* Flag for delayed visual update (needs lite_spot()) */
#define CAVE_OBJECT     0x1000    /* Mirror, glyph, etc. */
#define CAVE_UNSAFE     0x2000    /* Might have trap */
#define CAVE_IN_DETECT  0x4000    /* trap detected area (inner circle only) */
/* NOTE: 0x8000 is used above, listed out of sequence!!!! Also note we are now out of bits ... :( */

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
 *   REFLECTABLE: Refrectable spell attacks (used for "bolts")
 *   NO_HANGEKI: Avoid counter attacks of monsters
 *   PATH: Only used for printing project path
 *   FAST: Hide "visual" of flying bolts until blast
 */
#define PROJECT_JUMP        0x01
#define PROJECT_BEAM        0x02
#define PROJECT_THRU        0x04
#define PROJECT_STOP        0x08
#define PROJECT_GRID        0x10
#define PROJECT_ITEM        0x20
#define PROJECT_KILL        0x40
#define PROJECT_HIDE        0x80
#define PROJECT_DISI        0x100
#define PROJECT_PLAYER      0x200
#define PROJECT_AIMED       0x400
#define PROJECT_REFLECTABLE 0x800
#define PROJECT_NO_HANGEKI  0x1000
#define PROJECT_PATH        0x2000
#define PROJECT_FAST        0x4000
#define PROJECT_LOS         0x8000
#define PROJECT_FULL_DAM    0x10000
#define PROJECT_NO_PAIN     0x20000  /* Omit the pain messages. Note: Mon vs Mon melee is implemented with project()! */


/*
 * Special caster ID for project()
 */
#define PROJECT_WHO_PLAYER        0
#define PROJECT_WHO_UNCTRL_POWER -1
#define PROJECT_WHO_GLASS_SHARDS -2


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
 */
#define TARGET_KILL        0x01
#define TARGET_LOOK        0x02
#define TARGET_XTRA        0x04
#define TARGET_GRID        0x08
#define TARGET_MARK        0x10
#define TARGET_DISI        0x20


/*
 * Bit flags for control of get_check_strict()
 */
#define CHECK_OKAY_CANCEL 0x01
#define CHECK_NO_ESCAPE   0x02
#define CHECK_NO_HISTORY  0x04
#define CHECK_DEFAULT_Y   0x08


/*
 * Some bit-flags for the "smart" field
 */
#define SM_RES_ACID             0x00000001
#define SM_RES_ELEC             0x00000002
#define SM_RES_FIRE             0x00000004
#define SM_RES_COLD             0x00000008
#define SM_RES_POIS             0x00000010
#define SM_RES_NETH             0x00000020
#define SM_RES_LITE             0x00000040
#define SM_RES_DARK             0x00000080
#define SM_RES_FEAR             0x00000100
#define SM_RES_CONF             0x00000200
#define SM_RES_CHAOS            0x00000400
#define SM_RES_DISEN            0x00000800
#define SM_RES_BLIND            0x00001000
#define SM_RES_NEXUS            0x00002000
#define SM_RES_SOUND            0x00004000
#define SM_RES_SHARD            0x00008000
#define SM_OPP_ACID             0x00010000
#define SM_OPP_ELEC             0x00020000
#define SM_OPP_FIRE             0x00040000
#define SM_OPP_COLD             0x00080000
#define SM_OPP_POIS             0x00100000
#define SM_GUARDIAN             0x00200000
#define SM_CLONED               0x00400000
#define SM_PET                  0x00800000
#define SM_IMM_ACID             0x01000000
#define SM_IMM_ELEC             0x02000000
#define SM_IMM_FIRE             0x04000000
#define SM_IMM_COLD             0x08000000
#define SM_FRIENDLY             0x10000000
#define SM_IMM_REFLECT          0x20000000
#define SM_IMM_FREE             0x40000000
#define SM_IMM_MANA             0x80000000


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
 * Bit flags for the "p_ptr->notice" variable
 */
#define PN_COMBINE      0x00000001     /* Combine the pack */
#define PN_REORDER      0x00000002     /* Reorder the pack */
#define PN_AUTODESTROY  0x00000004     /* Auto-destroy marked item */
/* xxx (many) */


/*
 * Bit flags for the "p_ptr->update" variable
 */
#define PU_BONUS        0x00000001     /* Calculate bonuses */
#define PU_TORCH        0x00000002     /* Calculate torch radius */
/* xxx (many) */
#define PU_HP           0x00000010     /* Calculate chp and mhp */
#define PU_MANA         0x00000020     /* Calculate csp and msp */
#define PU_SPELLS       0x00000040     /* Calculate spells */
/* xxx (many) */
/* xxx (many) */
#define PU_UN_VIEW      0x00010000     /* Forget view */
#define PU_UN_LITE      0x00020000     /* Forget lite */
/* xxx (many) */
#define PU_VIEW         0x00100000     /* Update view */
#define PU_LITE         0x00200000     /* Update lite */
#define PU_MON_LITE     0x00400000     /* Monster illumination */
#define PU_DELAY_VIS    0x00800000     /* Mega-Hack -- Delayed visual update */
#define PU_MONSTERS     0x01000000     /* Update monsters */
#define PU_DISTANCE     0x02000000     /* Update distances */
/* xxx */
#define PU_FLOW         0x10000000     /* Update flow */
/* xxx (many) */


/*
 * Bit flags for the "p_ptr->redraw" variable
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

/* xxx */
/* xxx */
/* xxx */
/* xxx */

/*
 * Bit flags for the "p_ptr->window" variable (etc)
 */
#define PW_INVEN        0x00000001     /* Display inven/equip */
#define PW_EQUIP        0x00000002     /* Display equip/inven */
#define PW_SPELL        0x00000004     /* Display spell list */
#define PW_XXX          0x00000008
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

#define OD_LORE (OD_NAME_ONLY | OD_OMIT_PREFIX | OD_COLOR_CODED)

/*
 * Bit flags for the "p_ptr->special_attack" variable. -LM-
 *
 * Note:  The elemental and poison attacks should be managed using the
 * function "set_ele_attack", in spell2.c.  This provides for timeouts and
 * prevents the player from getting more than one at a time.
 */
#define ATTACK_CONFUSE        0x00000001
#define ATTACK_XXX1        0x00000002
#define ATTACK_XXX2        0x00000004
#define ATTACK_XXX3            0x00000008
#define ATTACK_ACID        0x00000010
#define ATTACK_ELEC        0x00000020
#define ATTACK_FIRE        0x00000040
#define ATTACK_COLD        0x00000080
#define ATTACK_POIS        0x00000100
#define ATTACK_HOLY        0x00000200
#define ATTACK_SUIKEN        0x00000400


#define DEFENSE_ACID        0x00000001
#define DEFENSE_ELEC        0x00000002
#define DEFENSE_FIRE        0x00000004
#define DEFENSE_COLD        0x00000008
#define DEFENSE_POIS        0x00000010
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
#define DEFENSE_SANCTUARY        0x00008000

#define MAX_KAMAE 4
#define KAMAE_MASK (KAMAE_GENBU | KAMAE_BYAKKO | KAMAE_SEIRYU | KAMAE_SUZAKU)

#define MAX_KATA 4
#define KATA_MASK (KATA_IAI | KATA_FUUJIN | KATA_KOUKIJIN | KATA_MUSOU)


#define ACTION_NONE       0
#define ACTION_SEARCH     1
#define ACTION_REST       2
#define ACTION_LEARN      3
#define ACTION_FISH       4
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
 * Exact numeric values are saved in v_info.raw so don't change/remove/reorder.
 */
enum summon_specific_e {
    SUMMON_ANT = 1,
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
};

/*
 * Spell types used by project(), and related functions.
 */
#define GF_ELEC         1
#define GF_POIS         2
#define GF_ACID         3
#define GF_COLD         4
#define GF_FIRE         5
#define GF_PSY_SPEAR    9
#define GF_MISSILE      10
#define GF_ARROW        11
#define GF_PLASMA       12
/* Replaced with GF_HOLY_FIRE and GF_HELL_FIRE */
/* #define GF_HOLY_ORB     13 */
#define GF_WATER        14
#define GF_LITE         15
#define GF_DARK         16
#define GF_LITE_WEAK    17  /* Only hurts monsters vulnerable to light ... */
#define GF_DARK_WEAK    18  /* Only hurts monsters vulnerable to dark ... */
#define GF_SHARDS       20
#define GF_SOUND        21
#define GF_CONFUSION    22
#define GF_FORCE        23
#define GF_INERT        24  /* M$ now uses GF_INERTIA for mouse gestures */
#define GF_MANA         26
#define GF_METEOR       27
#define GF_ICE          28
#define GF_CHAOS        30
#define GF_NETHER       31
#define GF_DISENCHANT   32
#define GF_NEXUS        33
#define GF_TIME         34
#define GF_GRAVITY      35
#define GF_KILL_WALL    40
#define GF_KILL_DOOR    41
#define GF_KILL_TRAP    42
#define GF_MAKE_WALL    45
#define GF_MAKE_DOOR    46
#define GF_MAKE_TRAP    47
#define GF_MAKE_TREE    48
#define GF_OLD_CLONE    51
#define GF_OLD_POLY             52
#define GF_OLD_HEAL             53
#define GF_OLD_SPEED    54
#define GF_OLD_SLOW             55
#define GF_OLD_CONF             56
#define GF_OLD_SLEEP    57
#define GF_OLD_DRAIN    58
#define GF_AWAY_UNDEAD  61
#define GF_AWAY_EVIL    62
#define GF_AWAY_ALL     63
#define GF_TURN_UNDEAD  64
#define GF_TURN_EVIL    65
#define GF_TURN_ALL     66
#define GF_DISP_UNDEAD  67
#define GF_DISP_EVIL    68
#define GF_DISP_ALL 69
#define GF_DISP_DEMON   70      /* New types for Zangband begin here... */
#define GF_DISP_LIVING  71
#define GF_ROCKET       72
#define GF_NUKE         73
#define GF_MAKE_GLYPH   74
#define GF_STASIS       75
#define GF_STONE_WALL   76
#define GF_DEATH_RAY    77
#define GF_STUN         78
#define GF_HOLY_FIRE    79
#define GF_HELL_FIRE    80
#define GF_DISINTEGRATE 81
#define GF_CHARM        82
#define GF_CONTROL_UNDEAD   83
#define GF_CONTROL_ANIMAL   84
#define GF_PSI         85
#define GF_PSI_DRAIN   86
#define GF_TELEKINESIS  87
#define GF_JAM_DOOR     88
#define GF_DOMINATION   89
#define GF_DISP_GOOD    90
#define GF_DRAIN_MANA   91
#define GF_MIND_BLAST   92
#define GF_BRAIN_SMASH  93
#define GF_CAUSE_1      94
#define GF_CAUSE_2      95
#define GF_CAUSE_3      96
#define GF_CAUSE_4      97
#define GF_HAND_DOOM    98
#define GF_CAPTURE      99
#define GF_ANIM_DEAD   100
#define GF_CONTROL_LIVING   101
#define GF_IDENTIFY    102
#define GF_ATTACK      103
#define GF_ENGETSU     104
#define GF_GENOCIDE    105
#define GF_PHOTO       106
#define GF_CONTROL_DEMON   107
#define GF_LAVA_FLOW   108
#define GF_BLOOD_CURSE 109
#define GF_SEEKER 110
#define GF_SUPER_RAY 111
#define GF_STAR_HEAL 112
#define GF_WATER_FLOW   113
#define GF_CRUSADE     114
#define GF_STASIS_EVIL 115
#define GF_WOUNDS      116
#define GF_BLOOD       117
#define GF_ELDRITCH            118
#define GF_ELDRITCH_STUN    119
#define GF_ELDRITCH_DRAIN    120
#define GF_ELDRITCH_DISPEL    121
#define GF_ELDRITCH_CONFUSE    122
#define GF_REMOVE_OBSTACLE    123
#define GF_PHARAOHS_CURSE   124
#define GF_ISOLATION        125
#define GF_ELDRITCH_HOWL 126
#define GF_ENTOMB    127
#define GF_UNHOLY_WORD  128
#define GF_DRAINING_TOUCH 129
#define GF_DEATH_TOUCH 130
#define GF_PSI_EGO_WHIP 131
#define GF_PSI_BRAIN_SMASH 132
#define GF_PSI_STORM 133
#define GF_MANA_CLASH 134
#define GF_ANTIMAGIC 135
#define GF_ROCK      136
#define GF_WEB      137
#define GF_AMNESIA  138
#define GF_STEAL    139
#define GF_WATER2   140
#define GF_STORM    141
#define GF_QUAKE    142
#define GF_CHARM_RING_BEARER  143
#define GF_SUBJUGATION 144
#define GF_PARALYSIS 145
#define GF_CONTROL_PACT_MONSTER  146

#define MAX_GF                147

/*
 * Some things which induce learning
 */
#define DRS_ACID         1
#define DRS_ELEC         2
#define DRS_FIRE         3
#define DRS_COLD         4
#define DRS_POIS         5
#define DRS_NETH         6
#define DRS_LITE         7
#define DRS_DARK         8
#define DRS_FEAR         9
#define DRS_CONF        10
#define DRS_CHAOS       11
#define DRS_DISEN       12
#define DRS_BLIND       13
#define DRS_NEXUS       14
#define DRS_SOUND       15
#define DRS_SHARD       16
#define DRS_FREE        30
#define DRS_MANA        31
#define DRS_REFLECT     32



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
#define OM_NOMSG           0x0002    /* temporary flag to suppress messages */
#define OM_NO_QUERY        0x0004    /* Query for auto-pick was already answered as 'No' */
#define OM_AUTODESTROY     0x0008    /* Destroy later to avoid illegal inventry shift */
#define OM_TOUCHED         0x0010    /* Object was touched by player */
#define OM_RESERVED        0x0020    /* Object reserved in the shop */
#define OM_WORN            0x0040    /* Object was previously being worn but is possibly no longer a legal piece of equipment (Mimics) */
#define OM_COUNTED         0x0080    /* Stats */
#define OM_EGO_COUNTED     0x0100    /* Stats */
#define OM_ART_COUNTED     0x0200    /* Stats */
#define OM_EFFECT_COUNTED  0x0400    /* Stats */


/*
 * Special Monster Flags (all temporary)
 */
#define MFLAG_VIEW      0x01    /* Monster is in line of sight */
#define MFLAG_TEMP      0x02    /* Monster is marked for project_hack() */
#define MFLAG_XXX2      0x04    /* (unused) */
#define MFLAG_XXX3      0x08    /* (unused) */
#define MFLAG_BORN      0x10    /* Monster is still being born */
#define MFLAG_NICE      0x20    /* Monster is still being nice */

#define MFLAG2_KAGE             0x00000001    /* Monster is kage */
#define MFLAG2_NOPET            0x00000002    /* Cannot make monster pet */
#define MFLAG2_NOGENO           0x00000004    /* Cannot genocide */
#define MFLAG2_CHAMELEON        0x00000008    /* Monster is chameleon */
#define MFLAG2_NOFLOW           0x00000010    /* Monster is in no_flow_by_smell mode */
#define MFLAG2_SHOW             0x00000020    /* Monster is recently memorized */
#define MFLAG2_MARK             0x00000040    /* Monster is currently memorized */
#define MFLAG2_TRIPPED          0x00000080
#define MFLAG2_XXXXXXXX         0x00000100
#define MFLAG2_NODESTRUCT       0x00000200
#define MFLAG2_AWARE            0x00000400
#define MFLAG2_DROP_BASIC       0x00000800
#define MFLAG2_DROP_UTILITY     0x00001000
#define MFLAG2_DROP_PRIZE       0X00002000
#define MFLAG2_DROP_MASK        (MFLAG2_DROP_BASIC | MFLAG2_DROP_UTILITY | MFLAG2_DROP_PRIZE)


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

/*
 * Object Flags (OF_*)
 *
 * Do not delete or reorder, and only add new flags to the end unless
 * you are ready to break savefiles.
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

    /* Stats (Code often assumes order here: e.g. OF_STR + loop_variable) */
    OF_STR,
    OF_INT,
    OF_WIS,
    OF_DEX,
    OF_CON,
    OF_CHR,
    OF_DEC_STR,
    OF_DEC_INT,
    OF_DEC_WIS,
    OF_DEC_DEX,
    OF_DEC_CON,
    OF_DEC_CHR,
    OF_SUST_STR,
    OF_SUST_INT,
    OF_SUST_WIS,
    OF_SUST_DEX,
    OF_SUST_CON,
    OF_SUST_CHR,

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

    /* Resists */
    OF_RES_ACID,
    OF_RES_ELEC,
    OF_RES_FIRE,
    OF_RES_COLD,
    OF_RES_POIS,
    OF_RES_LITE,
    OF_RES_DARK,
    OF_RES_CONF,
    OF_RES_NETHER,
    OF_RES_NEXUS,
    OF_RES_SOUND,
    OF_RES_SHARDS,
    OF_RES_CHAOS,
    OF_RES_DISEN,
    OF_RES_TIME,
    OF_RES_BLIND,
    OF_RES_FEAR,

    OF_IM_ACID,
    OF_IM_ELEC,
    OF_IM_FIRE,
    OF_IM_COLD,
    OF_IM_POIS,
    OF_IM_LITE,
    OF_IM_DARK,
    OF_IM_NETHER,
    OF_IM_BLIND,
    OF_IM_FEAR,

    OF_VULN_ACID,
    OF_VULN_ELEC,
    OF_VULN_FIRE,
    OF_VULN_COLD,
    OF_VULN_POIS,
    OF_VULN_LITE,
    OF_VULN_DARK,
    OF_VULN_CONF,
    OF_VULN_NETHER,
    OF_VULN_NEXUS,
    OF_VULN_SOUND,
    OF_VULN_SHARDS,
    OF_VULN_CHAOS,
    OF_VULN_DISEN,
    OF_VULN_BLIND,
    OF_VULN_FEAR,

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
    OF_LITE,
    OF_DARKNESS,
    OF_LORE1,
    OF_LORE2,

    OF_ACTIVATE, /* Present, but not required to Activate (obj_has_effect() suffices).
                    This is a very useful crutch for object lore, though. */

    OF_IGNORE_ACID,
    OF_IGNORE_ELEC,
    OF_IGNORE_FIRE,
    OF_IGNORE_COLD,

    /* Auras */
    OF_AURA_ELEC,
    OF_AURA_FIRE,
    OF_AURA_COLD,
    OF_AURA_SHARDS,
    OF_AURA_REVENGE,
    OF_AURA_FEAR,

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
    OF_BRAND_WILD,
    OF_BRAND_ORDER,
    OF_BRAND_MANA,
    OF_VORPAL,
    OF_VORPAL2,
    OF_IMPACT,
    OF_STUN,

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

    /* A few places loop from 0 <= i < OF_COUNT ... (init1, race_sword and race_ring) */
    OF_COUNT,
};
#define OF_ARRAY_SIZE          6
/* u32b flgs[OF_ARRAY_SIZE];
   assert((OF_COUNT + 31)/32 == OF_ARRAY_SIZE); is checked during initialization */

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
 * Bit flags for apply_magic() (etc)
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
#define AM_GUARDIAN     0x00000800


/*** Monster blow constants ***/


/*
 * New monster blow methods
 */
#define RBM_HIT          1
#define RBM_TOUCH        2
#define RBM_PUNCH        3
#define RBM_KICK         4
#define RBM_CLAW         5
#define RBM_BITE         6
#define RBM_STING        7
#define RBM_SLASH        8
#define RBM_BUTT         9
#define RBM_CRUSH       10
#define RBM_ENGULF      11
#define RBM_CHARGE      12
#define RBM_CRAWL       13
#define RBM_DROOL       14
#define RBM_SPIT        15
#define RBM_EXPLODE     16
#define RBM_GAZE        17
#define RBM_WAIL        18
#define RBM_SPORE       19
#define RBM_XXX4        20
#define RBM_BEG         21
#define RBM_INSULT      22
#define RBM_MOAN        23
#define RBM_SHOW        24
#define RBM_SHOOT       25


/*
 * New monster blow effects
 */
#define RBE_HURT         1
#define RBE_POISON       2
#define RBE_UN_BONUS     3
#define RBE_UN_POWER     4
#define RBE_EAT_GOLD     5
#define RBE_EAT_ITEM     6
#define RBE_EAT_FOOD     7
#define RBE_EAT_LITE     8
#define RBE_ACID         9
#define RBE_ELEC        10
#define RBE_FIRE        11
#define RBE_COLD        12
#define RBE_BLIND       13
#define RBE_CONFUSE     14
#define RBE_TERRIFY     15
#define RBE_PARALYZE    16
#define RBE_LOSE_STR    17
#define RBE_LOSE_INT    18
#define RBE_LOSE_WIS    19
#define RBE_LOSE_DEX    20
#define RBE_LOSE_CON    21
#define RBE_LOSE_CHR    22
#define RBE_LOSE_ALL    23
#define RBE_SHATTER     24
#define RBE_EXP_10      25
#define RBE_EXP_20      26
#define RBE_EXP_40      27
#define RBE_EXP_80      28
#define RBE_DISEASE     29
#define RBE_TIME        30
#define RBE_EXP_VAMP    31
#define RBE_DR_MANA     32
#define RBE_SUPERHURT   33


/*** Monster flag values (hard-coded) ***/


/*
 * New monster race bit flags
 */
#define RF1_UNIQUE              0x00000001  /* Unique Monster */
#define RF1_QUESTOR             0x00000002  /* Quest Monster Remark: See dungeon() for setting/unsetting for quests.*/
#define RF1_MALE                0x00000004  /* Male gender */
#define RF1_FEMALE              0x00000008  /* Female gender */
#define RF1_CHAR_CLEAR          0x00000010  /* Absorbs symbol */
#define RF1_SHAPECHANGER        0x00000020  /* TY: shapechanger */
#define RF1_ATTR_CLEAR          0x00000040  /* Absorbs color */
#define RF1_ATTR_MULTI          0x00000080  /* Changes color */
#define RF1_FORCE_DEPTH         0x00000100  /* Start at "correct" depth */
#define RF1_FORCE_MAXHP         0x00000200  /* Start with max hitpoints */
#define RF1_FORCE_SLEEP         0x00000400  /* Start out sleeping */
#define RF1_FORCE_EXTRA         0x00000800  /* Start out something */
#define RF1_ATTR_SEMIRAND       0x00001000  /* Color is determined semi-randomly */
#define RF1_FRIENDS             0x00002000  /* Arrive with some friends */
#define RF1_ESCORT              0x00004000  /* Arrive with an escort */
#define RF1_ESCORTS             0x00008000  /* Arrive with some escorts */
#define RF1_NEVER_BLOW          0x00010000  /* Never make physical blow */
#define RF1_NEVER_MOVE          0x00020000  /* Never make physical move */
#define RF1_RAND_25             0x00040000  /* Moves randomly (25%) */
#define RF1_RAND_50             0x00080000  /* Moves randomly (50%) */
#define RF1_ONLY_GOLD           0x00100000  /* Drop only gold */
#define RF1_ONLY_ITEM           0x00200000  /* Drop only items */
#define RF1_DROP_60             0x00400000  /* Drop an item/gold (60%) */
#define RF1_DROP_90             0x00800000  /* Drop an item/gold (90%) */
#define RF1_DROP_1D2            0x01000000  /* Drop 1d2 items/gold */
#define RF1_DROP_2D2            0x02000000  /* Drop 2d2 items/gold */
#define RF1_DROP_3D2            0x04000000  /* Drop 3d2 items/gold */
#define RF1_DROP_4D2            0x08000000  /* Drop 4d2 items/gold */
#define RF1_DROP_GOOD           0x10000000  /* Drop good items */
#define RF1_DROP_GREAT          0x20000000  /* Drop great items */
#define RF1_TRUMP               0x40000000  /* Free teleport every turn */
#define RF1_XXX3                0x80000000  /* XXX */

/*
 * New monster race bit flags
 */
#define RF2_STUPID          0x00000001  /* Monster is stupid */
#define RF2_SMART           0x00000002  /* Monster is smart */
#define RF2_CAN_SPEAK       0x00000004  /* TY: can speak */
#define RF2_REFLECTING      0x00000008  /* Reflects bolts */
#define RF2_INVISIBLE       0x00000010  /* Monster avoids vision */
#define RF2_COLD_BLOOD      0x00000020  /* Monster avoids infra */
#define RF2_EMPTY_MIND      0x00000040  /* Monster avoids telepathy */
#define RF2_WEIRD_MIND      0x00000080  /* Monster avoids telepathy? */
#define RF2_MULTIPLY        0x00000100  /* Monster reproduces */
#define RF2_REGENERATE      0x00000200  /* Monster regenerates */
#define RF2_CHAR_MULTI      0x00000400  /* (Not implemented) */
#define RF2_ATTR_ANY        0x00000800  /* TY: Attr_any */
#define RF2_POWERFUL        0x00001000  /* Monster has strong breath */
#define RF2_ELDRITCH_HORROR 0x00002000  /* Sanity-blasting horror    */
#define RF2_AURA_FIRE       0x00004000  /* Burns in melee */
#define RF2_AURA_ELEC       0x00008000  /* Shocks in melee */
#define RF2_OPEN_DOOR       0x00010000  /* Monster can open doors */
#define RF2_BASH_DOOR       0x00020000  /* Monster can bash doors */
#define RF2_PASS_WALL       0x00040000  /* Monster can pass walls */
#define RF2_KILL_WALL       0x00080000  /* Monster can destroy walls */
#define RF2_MOVE_BODY       0x00100000  /* Monster can move monsters */
#define RF2_KILL_BODY       0x00200000  /* Monster can kill monsters */
#define RF2_TAKE_ITEM       0x00400000  /* Monster can pick up items */
#define RF2_KILL_ITEM       0x00800000  /* Monster can crush items */
#define RF2_AURA_REVENGE    0x01000000
#define RF2_THIEF           0x02000000
#define RF2_AURA_FEAR       0x04000000
#define RF2_CAMELOT         0x08000000
#define RF2_KNIGHT          0x10000000
#define RF2_SOUTHERING      0x20000000
#define RF2_HUMAN           0x40000000  /* Human */
#define RF2_QUANTUM         0x80000000  /* Monster has quantum behavior */

/*
 * New monster race bit flags
 */
#define RF3_ORC             0x00000001  /* Orc */
#define RF3_TROLL           0x00000002  /* Troll */
#define RF3_GIANT           0x00000004  /* Giant */
#define RF3_DRAGON          0x00000008  /* Dragon */
#define RF3_DEMON           0x00000010  /* Demon */
#define RF3_UNDEAD          0x00000020  /* Undead */
#define RF3_EVIL            0x00000040  /* Evil */
#define RF3_ANIMAL          0x00000080  /* Animal */
#define RF3_AMBERITE        0x00000100  /* TY: Amberite */
#define RF3_GOOD            0x00000200  /* Good */
#define RF3_AURA_COLD       0x00000400  /* Freezes in melee */
#define RF3_NONLIVING       0x00000800  /* TY: Non-Living (?) */
#define RF3_HURT_LITE       0x00001000  /* Hurt by lite */
#define RF3_HURT_ROCK       0x00002000  /* Hurt by rock remover */
#define RF3_HURT_FIRE       0x00004000  /* Hurt badly by fire */
#define RF3_HURT_COLD       0x00008000  /* Hurt badly by cold */
#define RF3_OLYMPIAN        0x00010000
#define RF3_XXX17           0x00020000
#define RF3_XXX18           0x00040000
#define RF3_XXX19           0x00080000
#define RF3_XXX20           0x00100000
#define RF3_XXX21           0x00200000
#define RF3_XXX22           0x00400000
#define RF3_XXX23           0x00800000
#define RF3_XXX24           0x01000000
#define RF3_XXX25           0x02000000
#define RF3_XXX26           0x04000000
#define RF3_XXX27           0x08000000
#define RF3_NO_FEAR         0x10000000  /* Cannot be scared */
#define RF3_NO_STUN         0x20000000  /* Cannot be stunned */
#define RF3_NO_CONF         0x40000000  /* Cannot be confused and resist confusion */
#define RF3_NO_SLEEP        0x80000000  /* Cannot be slept */

/*
 * New monster race bit flags
 */
#define RF4_SHRIEK          0x00000001  /* Shriek for help */
#define RF4_THROW           0x00000002  /* Throw a Boulder */
#define RF4_DISPEL          0x00000004  /* Dispel magic */
#define RF4_ROCKET          0x00000008  /* TY: Rocket */
#define RF4_SHOOT           0x00000010  /* Fire missiles */
#define RF4_ANTI_MAGIC      0x00000020
#define RF4_POLY            0x00000040
#define RF4_BR_STORM        0x00000080
#define RF4_BR_ACID         0x00000100  /* Breathe Acid */
#define RF4_BR_ELEC         0x00000200  /* Breathe Elec */
#define RF4_BR_FIRE         0x00000400  /* Breathe Fire */
#define RF4_BR_COLD         0x00000800  /* Breathe Cold */
#define RF4_BR_POIS         0x00001000  /* Breathe Poison */
#define RF4_BR_NETH         0x00002000  /* Breathe Nether */
#define RF4_BR_LITE         0x00004000  /* Breathe Lite */
#define RF4_BR_DARK         0x00008000  /* Breathe Dark */
#define RF4_BR_CONF         0x00010000  /* Breathe Confusion */
#define RF4_BR_SOUN         0x00020000  /* Breathe Sound */
#define RF4_BR_CHAO         0x00040000  /* Breathe Chaos */
#define RF4_BR_DISE         0x00080000  /* Breathe Disenchant */
#define RF4_BR_NEXU         0x00100000  /* Breathe Nexus */
#define RF4_BR_TIME         0x00200000  /* Breathe Time */
#define RF4_BR_INER         0x00400000  /* Breathe Inertia */
#define RF4_BR_GRAV         0x00800000  /* Breathe Gravity */
#define RF4_BR_SHAR         0x01000000  /* Breathe Shards */
#define RF4_BR_PLAS         0x02000000  /* Breathe Plasma */
#define RF4_BR_WALL         0x04000000  /* Breathe Force */
#define RF4_BR_MANA         0x08000000  /* Breathe Mana */
#define RF4_BA_NUKE         0x10000000  /* TY: Nuke Ball */
#define RF4_BR_NUKE         0x20000000  /* TY: Toxic Breath */
#define RF4_BA_CHAO         0x40000000  /* TY: Logrus Ball */
#define RF4_BR_DISI         0x80000000  /* Breathe Disintegration */

/*
 * New monster race bit flags
 */
#define RF5_BA_ACID         0x00000001  /* Acid Ball */
#define RF5_BA_ELEC         0x00000002  /* Elec Ball */
#define RF5_BA_FIRE         0x00000004  /* Fire Ball */
#define RF5_BA_COLD         0x00000008  /* Cold Ball */
#define RF5_BA_POIS         0x00000010  /* Poison Ball */
#define RF5_BA_NETH         0x00000020  /* Nether Ball */
#define RF5_BA_WATE         0x00000040  /* Water Ball */
#define RF5_BA_MANA         0x00000080  /* Mana Storm */
#define RF5_BA_DARK         0x00000100  /* Darkness Storm */
#define RF5_DRAIN_MANA      0x00000200  /* Drain Mana */
#define RF5_MIND_BLAST      0x00000400  /* Blast Mind */
#define RF5_BRAIN_SMASH     0x00000800  /* Smash Brain */
#define RF5_CAUSE_1         0x00001000  /* Cause Light Wound */
#define RF5_CAUSE_2         0x00002000  /* Cause Serious Wound */
#define RF5_CAUSE_3         0x00004000  /* Cause Critical Wound */
#define RF5_CAUSE_4         0x00008000  /* Cause Mortal Wound */
#define RF5_BO_ACID         0x00010000  /* Acid Bolt */
#define RF5_BO_ELEC         0x00020000  /* Elec Bolt (unused) */
#define RF5_BO_FIRE         0x00040000  /* Fire Bolt */
#define RF5_BO_COLD         0x00080000  /* Cold Bolt */
#define RF5_BA_LITE         0x00100000  /* StarBurst */
#define RF5_BO_NETH         0x00200000  /* Nether Bolt */
#define RF5_BO_WATE         0x00400000  /* Water Bolt */
#define RF5_BO_MANA         0x00800000  /* Mana Bolt */
#define RF5_BO_PLAS         0x01000000  /* Plasma Bolt */
#define RF5_BO_ICEE         0x02000000  /* Ice Bolt */
#define RF5_MISSILE         0x04000000  /* Magic Missile */
#define RF5_SCARE           0x08000000  /* Frighten Player */
#define RF5_BLIND           0x10000000  /* Blind Player */
#define RF5_CONF            0x20000000  /* Confuse Player */
#define RF5_SLOW            0x40000000  /* Slow Player */
#define RF5_HOLD            0x80000000  /* Paralyze Player */

/*
 * New monster race bit flags
 */
#define RF6_HASTE           0x00000001  /* Speed self */
#define RF6_HAND_DOOM       0x00000002  /* Hand of Doom */
#define RF6_HEAL            0x00000004  /* Heal self */
#define RF6_INVULNER        0x00000008  /* INVULNERABILITY! */
#define RF6_BLINK           0x00000010  /* Teleport Short */
#define RF6_TPORT           0x00000020  /* Teleport Long */
#define RF6_WORLD           0x00000040  /* world */
#define RF6_SPECIAL         0x00000080  /* Special Attack */
#define RF6_TELE_TO         0x00000100  /* Move player to monster */
#define RF6_TELE_AWAY       0x00000200  /* Move player far away */
#define RF6_TELE_LEVEL      0x00000400  /* Move player vertically */
#define RF6_PSY_SPEAR       0x00000800  /* Psyco-spear */
#define RF6_DARKNESS        0x00001000  /* Create Darkness */
#define RF6_TRAPS           0x00002000  /* Create Traps */
#define RF6_FORGET          0x00004000  /* Cause amnesia */
#define RF6_RAISE_DEAD      0x00008000  /* Raise Dead */
#define RF6_S_KIN           0x00010000  /* Summon "kin" */
#define RF6_S_CYBER         0x00020000  /* Summon Cyberdemons! */
#define RF6_S_MONSTER       0x00040000  /* Summon Monster */
#define RF6_S_MONSTERS      0x00080000  /* Summon Monsters */
#define RF6_S_ANT           0x00100000  /* Summon Ants */
#define RF6_S_SPIDER        0x00200000  /* Summon Spiders */
#define RF6_S_HOUND         0x00400000  /* Summon Hounds */
#define RF6_S_HYDRA         0x00800000  /* Summon Hydras */
#define RF6_S_ANGEL         0x01000000  /* Summon Angel */
#define RF6_S_DEMON         0x02000000  /* Summon Demon */
#define RF6_S_UNDEAD        0x04000000  /* Summon Undead */
#define RF6_S_DRAGON        0x08000000  /* Summon Dragon */
#define RF6_S_HI_UNDEAD     0x10000000  /* Summon Greater Undead */
#define RF6_S_HI_DRAGON     0x20000000  /* Summon Ancient Dragon */
#define RF6_S_AMBERITES     0x40000000  /* Summon Amberites */
#define RF6_S_UNIQUE        0x80000000  /* Summon Unique Monster */

/*
 * New monster race bit flags
 */
#define RF7_AQUATIC             0x00000001  /* Aquatic monster */
#define RF7_CAN_SWIM            0x00000002  /* Monster can swim */
#define RF7_CAN_FLY             0x00000004  /* Monster can fly */
#define RF7_FRIENDLY            0x00000008  /* Monster is friendly */
#define RF7_NAZGUL              0x00000010  /* Is a "Nazgul" unique */
#define RF7_UNIQUE2             0x00000020  /* Fake unique */
#define RF7_RIDING              0x00000040  /* Good for riding */
#define RF7_KAGE                0x00000080  /* Is kage */
#define RF7_HAS_LITE_1          0x00000100  /* Monster carries light */
#define RF7_SELF_LITE_1         0x00000200  /* Monster lights itself */
#define RF7_HAS_LITE_2          0x00000400  /* Monster carries light */
#define RF7_SELF_LITE_2         0x00000800  /* Monster lights itself */
#define RF7_GUARDIAN            0x00001000  /* Guardian of a dungeon */
#define RF7_CHAMELEON           0x00002000  /* Chameleon can change */
#define RF7_KILL_EXP            0x00004000  /* No exp until you kill it */
#define RF7_TANUKI              0x00008000  /* Tanuki disguise */
#define RF7_HAS_DARK_1          0x00010000  /* Monster carries darkness */
#define RF7_SELF_DARK_1         0x00020000  /* Monster darkens itself */
#define RF7_HAS_DARK_2          0x00040000  /* Monster carries darkness */
#define RF7_SELF_DARK_2         0x00080000  /* Monster darkens itself */

/*
 * Monster race flags
 */
#define RF8_WILD_ONLY           0x00000001
#define RF8_WILD_TOWN           0x00000002
#define RF8_XXX8X02             0x00000004
#define RF8_WILD_SHORE          0x00000008
#define RF8_WILD_OCEAN          0x00000010
#define RF8_WILD_WASTE          0x00000020
#define RF8_WILD_WOOD           0x00000040
#define RF8_WILD_VOLCANO        0x00000080
#define RF8_XXX8X08             0x00000100
#define RF8_WILD_MOUNTAIN       0x00000200
#define RF8_WILD_GRASS          0x00000400
#define RF8_WILD_ALL            0x80000000

/*
 * Monster Corspe Info, including a bunch of stuff for The Possessor
 */
#define RF9_DROP_CORPSE         0x00000001
#define RF9_DROP_SKELETON       0x00000002
#define RF9_POS_GAIN_AC         0x00000004
#define RF9_POS_TELEPATHY       0x00000008
#define RF9_POS_SEE_INVIS       0x00000010
#define RF9_POS_HOLD_LIFE       0x00000020
#define RF9_POS_SUST_STR        0x00000040
#define RF9_POS_SUST_INT        0x00000080
#define RF9_POS_SUST_WIS        0x00000100
#define RF9_POS_SUST_DEX        0x00000200
#define RF9_POS_SUST_CON        0x00000400
#define RF9_POS_SUST_CHR        0x00000800
#define RF9_XXX13               0x00001000
#define RF9_XXX14               0x00002000
#define RF9_POS_DETECT_TRAPS    0x00004000
#define RF9_POS_DETECT_EVIL     0x00008000
#define RF9_POS_DETECT_MONSTERS 0x00010000
#define RF9_POS_DETECT_OBJECTS  0x00020000
#define RF9_POS_MAPPING         0x00040000
#define RF9_POS_IDENTIFY        0x00080000
#define RF9_POS_HEROISM         0x00100000
#define RF9_POS_BLESSING        0x00200000
#define RF9_POS_BERSERK         0x00400000
#define RF9_POS_CLAIRVOYANCE    0x00800000
#define RF9_POS_BACKSTAB        0x01000000
#define RF9_XXX26               0x02000000
#define RF9_XXX27               0x04000000
#define RF9_XXX28               0x08000000
#define RF9_XXX29               0x01000000
#define RF9_XXX30               0x20000000
#define RF9_XXX31               0x40000000
#define RF9_XXX32               0x80000000

/* Themed drops ... r_info[].drop_theme
   Note: If you reorder these, you'll need to touch r_info.txt as well.
   See:  obj_drop_theme for implementation
   See:  r_drop_themes in init1.c for parsing r_info.txt */
enum r_drop_e
{
    R_DROP_NONE = 0,

    /* Class Themes */
    R_DROP_WARRIOR,
    R_DROP_WARRIOR_SHOOT,
    R_DROP_ARCHER,
    R_DROP_MAGE,
    R_DROP_PRIEST,
    R_DROP_PRIEST_EVIL,
    R_DROP_PALADIN,
    R_DROP_PALADIN_EVIL,
    R_DROP_SAMURAI,
    R_DROP_NINJA,
    R_DROP_ROGUE,

    /* Racial Themes */
    R_DROP_HOBBIT,
    R_DROP_DWARF,

    R_DROP_JUNK,
    R_DROP_MAX
};

/*
 * Monster bit flags of racial resistances
 * Note: Resist confusion was merged to RFR_NO_CONF
 */
#define RFR_IM_ACID         0x00000001  /* Resist acid */
#define RFR_IM_ELEC         0x00000002  /* Resist elec */
#define RFR_IM_FIRE         0x00000004  /* Resist fire */
#define RFR_IM_COLD         0x00000008  /* Resist cold */
#define RFR_IM_POIS         0x00000010  /* Resist poison */
#define RFR_RES_LITE        0x00000020  /* Resist lite */
#define RFR_RES_DARK        0x00000040  /* Resist dark */
#define RFR_RES_NETH        0x00000080  /* Resist nether */
#define RFR_RES_WATE        0x00000100  /* Resist water */
#define RFR_RES_PLAS        0x00000200  /* Resist plasma */
#define RFR_RES_SHAR        0x00000400  /* Resist shards */
#define RFR_RES_SOUN        0x00000800  /* Resist sound */
#define RFR_RES_CHAO        0x00001000  /* Resist chaos */
#define RFR_RES_NEXU        0x00002000  /* Resist nexus */
#define RFR_RES_DISE        0x00004000  /* Resist disenchantment */
#define RFR_RES_WALL        0x00008000  /* Resist force */
#define RFR_RES_INER        0x00010000  /* Resist inertia */
#define RFR_RES_TIME        0x00020000  /* Resist time */
#define RFR_RES_GRAV        0x00040000  /* Resist gravity */
#define RFR_RES_ALL         0x00080000  /* Resist all */
#define RFR_RES_TELE        0x00100000  /* Resist teleportation */
#define RFR_PACT_MONSTER    0x00200000  /* Resists damage due to pact alliance ... s/b reset on new characters! */
#define RFR_RES_ACID        0x00400000
#define RFR_RES_ELEC        0x00800000
#define RFR_RES_FIRE        0x01000000
#define RFR_RES_COLD        0x02000000
#define RFR_RES_POIS        0x04000000
#define RFR_XXX27           0x08000000
#define RFR_XXX28           0x10000000
#define RFR_XXX29           0x20000000
#define RFR_XXX30           0x40000000
#define RFR_XXX31           0x80000000


/*
 * Hack -- choose "intelligent" spells when desperate
 * Including "summon" spells
 */
#define RF4_INT_MASK \
    (RF4_SUMMON_MASK | RF4_DISPEL | RF4_ANTI_MAGIC | RF4_POLY)

#define RF5_INT_MASK \
    (RF5_SUMMON_MASK | \
     RF5_HOLD | RF5_SLOW | RF5_CONF | RF5_BLIND | RF5_SCARE)

#define RF6_INT_MASK \
    (RF6_SUMMON_MASK | \
     RF6_BLINK | RF6_TPORT | RF6_TELE_LEVEL | RF6_TELE_AWAY | \
     RF6_HEAL | RF6_INVULNER | RF6_HASTE | RF6_TRAPS)

#define RF6_PANIC_MASK (RF6_TPORT | RF6_TELE_LEVEL | RF6_TELE_AWAY | RF6_HEAL)

/*
 * Hack -- spells that cannot be used while player riding on the monster
 */
#define RF4_RIDING_MASK \
    (RF4_SHRIEK)

#define RF5_RIDING_MASK 0

#define RF6_RIDING_MASK \
    (RF6_BLINK | RF6_TPORT | RF6_TRAPS | RF6_DARKNESS | RF6_SPECIAL)

/*
 * Hack -- "bolt" spells that may hurt fellow monsters
 * Currently "bolt" spells are included in "attack"
 */
#define RF4_BOLT_MASK \
    (RF4_ROCKET | RF4_SHOOT)

#define RF5_BOLT_MASK \
    (RF5_BO_ACID | RF5_BO_ELEC | RF5_BO_FIRE | RF5_BO_COLD | \
     RF5_BO_NETH | RF5_BO_WATE | RF5_BO_MANA | RF5_BO_PLAS | \
     RF5_BO_ICEE | RF5_MISSILE)

#define RF6_BOLT_MASK 0

/*
 * Hack -- "beam" spells that may hurt fellow monsters
 * Currently "beam" spells are included in "attack"
 */
#define RF4_BEAM_MASK 0

#define RF5_BEAM_MASK 0

#define RF6_BEAM_MASK (RF6_PSY_SPEAR)

/*
 * Hack -- "ball" spells that may hurt friends
 * Including "radius 4 ball" and "breath" spells
 * Currently "ball" spells are included in "attack"
 */
#define RF4_BALL_MASK \
    (RF4_BIG_BALL_MASK | RF4_BREATH_MASK | \
     RF4_ROCKET | RF4_BA_NUKE | RF4_THROW)

#define RF5_BALL_MASK \
    (RF5_BIG_BALL_MASK | RF5_BREATH_MASK | \
     RF5_BA_ACID | RF5_BA_ELEC | RF5_BA_FIRE | RF5_BA_COLD | \
     RF5_BA_POIS | RF5_BA_NETH)

#define RF6_BALL_MASK \
    (RF6_BIG_BALL_MASK | RF6_BREATH_MASK)

/*
 * Hack -- "ball" spells with radius 4 that may hurt friends
 * Currently "radius 4 ball" spells are included in "ball"
 */
#define RF4_BIG_BALL_MASK \
    (RF4_BA_CHAO)

#define RF5_BIG_BALL_MASK \
    (RF5_BA_LITE | RF5_BA_DARK | RF5_BA_WATE | RF5_BA_MANA)

#define RF6_BIG_BALL_MASK 0

/*
 * Hack -- "breath" spells that may hurt friends
 * Currently "breath" spells are included in "ball" and "non-magic"
 */
#define RF4_BREATH_MASK \
    (RF4_BR_ACID | RF4_BR_ELEC | RF4_BR_FIRE | RF4_BR_COLD | \
     RF4_BR_POIS | RF4_BR_NETH | RF4_BR_LITE | RF4_BR_DARK | \
     RF4_BR_CONF | RF4_BR_SOUN | RF4_BR_CHAO | RF4_BR_DISE | \
     RF4_BR_NEXU | RF4_BR_SHAR | RF4_BR_TIME | RF4_BR_INER | \
     RF4_BR_GRAV | RF4_BR_PLAS | RF4_BR_WALL | RF4_BR_MANA | \
     RF4_BR_NUKE | RF4_BR_DISI | RF4_BR_STORM)

#define RF5_BREATH_MASK 0

#define RF6_BREATH_MASK 0

/*
 * Hack -- "summon" spells
 * Currently "summon" spells are included in "intelligent" and "indirect"
 */
#define RF4_SUMMON_MASK 0

#define RF5_SUMMON_MASK 0

#define RF6_SUMMON_MASK \
    (RF6_S_KIN | RF6_S_CYBER | RF6_S_MONSTER | RF6_S_MONSTERS | RF6_S_ANT | \
     RF6_S_SPIDER | RF6_S_HOUND | RF6_S_HYDRA | RF6_S_ANGEL | RF6_S_DEMON | \
     RF6_S_UNDEAD | RF6_S_DRAGON | RF6_S_HI_UNDEAD | RF6_S_HI_DRAGON | \
     RF6_S_AMBERITES | RF6_S_UNIQUE)

#define RF6_WORTHY_SUMMON_MASK \
    (RF6_S_CYBER | RF6_S_HI_UNDEAD | RF6_S_HI_DRAGON | \
     RF6_S_AMBERITES | RF6_S_UNIQUE)

/*
 * Hack -- "attack" spells
 * Including "bolt", "beam" and "ball" spells
 */
#define RF4_ATTACK_MASK \
    (RF4_BOLT_MASK | RF4_BEAM_MASK | RF4_BALL_MASK | RF4_DISPEL | RF4_ANTI_MAGIC | RF4_POLY)

#define RF5_ATTACK_MASK \
    (RF5_BOLT_MASK | RF5_BEAM_MASK | RF5_BALL_MASK | \
     RF5_DRAIN_MANA | RF5_MIND_BLAST | RF5_BRAIN_SMASH | \
     RF5_CAUSE_1 | RF5_CAUSE_2 | RF5_CAUSE_3 | RF5_CAUSE_4 | \
     RF5_SCARE | RF5_BLIND | RF5_CONF | RF5_SLOW | RF5_HOLD)

#define RF5_WORTHY_ATTACK_MASK \
    (RF5_BOLT_MASK | RF5_BEAM_MASK | RF5_BALL_MASK | \
     RF5_MIND_BLAST | RF5_BRAIN_SMASH | RF5_SCARE)

#define RF6_ATTACK_MASK \
    (RF6_BOLT_MASK | RF6_BEAM_MASK | RF6_BALL_MASK | \
     RF6_HAND_DOOM | RF6_TELE_TO | RF6_TELE_AWAY | RF6_TELE_LEVEL | \
     RF6_DARKNESS | RF6_TRAPS | RF6_FORGET)

#define RF6_WORTHY_ATTACK_MASK  (RF6_BOLT_MASK | RF6_BEAM_MASK | RF6_BALL_MASK | RF6_HAND_DOOM | RF6_TELE_TO)

/*
 * Hack -- "indirect" spells
 * Including "summon" spells
 */
#define RF4_INDIRECT_MASK \
    (RF4_SUMMON_MASK | RF4_SHRIEK)

#define RF5_INDIRECT_MASK \
    (RF5_SUMMON_MASK)

#define RF6_INDIRECT_MASK \
    (RF6_SUMMON_MASK | \
     RF6_HASTE | RF6_HEAL | RF6_INVULNER | RF6_BLINK | RF6_WORLD | \
     RF6_TPORT | RF6_RAISE_DEAD)

/*
 * When players are hiding inside the walls, we need to flush 'em out!
 * Note: Summoning will target on the caster and tend to bring forth
 * wall breakers, disintegraters and passwall monsters!
 */
#define RF4_NO_FLOW_MASK_EASY RF4_SHRIEK

#define RF5_NO_FLOW_MASK_EASY 0

#define RF6_NO_FLOW_MASK_EASY \
    (RF6_HASTE | RF6_HEAL | RF6_RAISE_DEAD)

#define RF4_NO_FLOW_MASK_HARD \
    (RF4_SUMMON_MASK | RF4_SHRIEK)

#define RF5_NO_FLOW_MASK_HARD \
    (RF5_SUMMON_MASK)

#define RF6_NO_FLOW_MASK_HARD \
    (RF6_SUMMON_MASK | \
     RF6_HASTE | RF6_HEAL | RF6_INVULNER | RF6_BLINK | RF6_WORLD | \
     RF6_TPORT | RF6_RAISE_DEAD)

/*
 * Hack -- "non-magic" spells
 * Including "breath" spells
 */
#define RF4_NOMAGIC_MASK \
    (RF4_BREATH_MASK | RF4_SHRIEK | RF4_ROCKET | RF4_SHOOT)

#define RF5_NOMAGIC_MASK \
    (RF5_BREATH_MASK)

#define RF6_NOMAGIC_MASK \
    (RF6_BREATH_MASK | RF6_SPECIAL)

/*
 * Hack -- "torch" masks
 */
#define RF7_LITE_MASK \
    (RF7_HAS_LITE_1 | RF7_SELF_LITE_1 | RF7_HAS_LITE_2 | RF7_SELF_LITE_2)

#define RF7_DARK_MASK \
    (RF7_HAS_DARK_1 | RF7_SELF_DARK_1 | RF7_HAS_DARK_2 | RF7_SELF_DARK_2)

#define RF7_HAS_LD_MASK \
    (RF7_HAS_LITE_1 | RF7_HAS_LITE_2 | RF7_HAS_DARK_1 | RF7_HAS_DARK_2)

#define RF7_SELF_LD_MASK \
    (RF7_SELF_LITE_1 | RF7_SELF_LITE_2 | RF7_SELF_DARK_1 | RF7_SELF_DARK_2)

/*
 * Hack -- effective elemental and poison immunity mask
 * Note: These flags are currently used for slays (monk, melee, shots)
 * and auras, so lumping immunity and resistance together is probably OK for now.
 */
#define RFR_EFF_IM_ACID_MASK  (RFR_IM_ACID | RFR_RES_ACID | RFR_RES_ALL)
#define RFR_EFF_IM_ELEC_MASK  (RFR_IM_ELEC | RFR_RES_ELEC | RFR_RES_ALL)
#define RFR_EFF_IM_FIRE_MASK  (RFR_IM_FIRE | RFR_RES_FIRE | RFR_RES_ALL)
#define RFR_EFF_IM_COLD_MASK  (RFR_IM_COLD | RFR_RES_COLD | RFR_RES_ALL)
#define RFR_EFF_IM_POIS_MASK  (RFR_IM_POIS | RFR_RES_POIS | RFR_RES_ALL)
#define RFR_EFF_RES_SHAR_MASK (RFR_RES_SHAR | RFR_RES_ALL)
#define RFR_EFF_RES_CHAO_MASK (RFR_RES_CHAO | RFR_RES_ALL)
#define RFR_EFF_RES_NEXU_MASK (RFR_RES_NEXU | RFR_RES_ALL)
#define RFR_EFF_RES_TIME_MASK (RFR_RES_TIME | RFR_RES_ALL)


#define MR1_SINKA      0x01  /* Evolution */
#define MR1_POSSESSOR  0x02  /* Body Type */
#define MR1_LORE       0x04  /* Probed */


#define is_friendly(A) \
     (bool)(((A)->smart & SM_FRIENDLY) ? TRUE : FALSE)

#define is_friendly_idx(IDX) \
     (bool)((IDX) > 0 && is_friendly(&m_list[(IDX)]))

#define is_pet(A) \
     (bool)(((A)->smart & SM_PET) ? TRUE : FALSE)

#define is_aware(A) \
     (bool)(((A)->mflag2 & MFLAG2_AWARE) ? TRUE : FALSE)

#define is_hostile(A) \
     (bool)((is_friendly(A) || is_pet(A)) ? FALSE : TRUE)

/* Hack -- Determine monster race appearance index is same as race index */
#define is_original_ap(A) \
     (bool)(((A)->ap_r_idx == (A)->r_idx) ? TRUE : FALSE)

#define is_original_ap_and_seen(A) \
     (bool)((A)->ml && !p_ptr->image && ((A)->ap_r_idx == (A)->r_idx))

/*
 * Is the monster seen by the player?
 */
#define is_seen(A) \
    ((bool)((A)->ml && (!ignore_unview || p_ptr->inside_battle || \
     (player_can_see_bold((A)->fy, (A)->fx) && projectable(py, px, (A)->fy, (A)->fx)))))


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
 * Determine if a given inventory item is "known"
 * Test One -- Check for special "known" tag
 * Test Two -- Check for "Easy Know" + "Aware"
 */
#define object_is_known(T) \
    (((T)->ident & (IDENT_KNOWN)) || \
     (k_info[(T)->k_idx].easy_know && k_info[(T)->k_idx].aware))


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
      (k_info[(T)->k_idx].x_attr) : (r_info[(T)->pval].x_attr)))

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
 * Artifacts use the "name1" field
 */
#define object_is_fixed_artifact(T) \
    ((T)->name1 ? TRUE : FALSE)

/*
 * Ego-Items use the "name2" field
 */
#define object_is_ego(T) \
    ((T)->name2 ? TRUE : FALSE)


/*
 * Broken items.
 */
#define object_is_broken(T) \
    ((T)->ident & (IDENT_BROKEN))

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
 * Convert a "location" (Y,X) into a "grid" (G)
 */
#define GRID(Y,X) \
    (256 * (Y) + (X))

/*
 * Convert a "grid" (G) into a "location" (Y)
 */
#define GRID_Y(G) \
    ((int)((G) / 256U))

/*
 * Convert a "grid" (G) into a "location" (X)
 */
#define GRID_X(G) \
    ((int)((G) % 256U))


/*
 * Determines if a map location is fully inside the outer walls
 */
#define in_bounds(Y,X) \
   (((Y) > 0) && ((X) > 0) && ((Y) < cur_hgt-1) && ((X) < cur_wid-1))

/*
 * Determines if a map location is on or inside the outer walls
 */
#define in_bounds2(Y,X) \
   (((Y) >= 0) && ((X) >= 0) && ((Y) < cur_hgt) && ((X) < cur_wid))

/*
 * Determines if a map location is on or inside the outer walls
 * (unsigned version)
 */
#define in_bounds2u(Y,X) \
   (((Y) < cur_hgt) && ((X) < cur_wid))

/*
 * Determines if a map location is currently "on screen" -RAK-
 * Note that "panel_contains(Y,X)" always implies "in_bounds2(Y,X)".
 */
#define panel_contains(Y,X) \
    (cave_xy_is_visible((X), (Y)))

/*
 * Determine if player is on this grid
 */
#define player_bold(Y,X) \
    (((Y) == py) && ((X) == px))


/*
 * Grid based version of "player_bold()"
 */
#define player_grid(C) \
    ((C) == &cave[py][px])


#define cave_have_flag_bold(Y,X,INDEX) \
    (have_flag(f_info[cave[(Y)][(X)].feat].flags, (INDEX)))


#define cave_have_flag_grid(C,INDEX) \
    (have_flag(f_info[(C)->feat].flags, (INDEX)))


/*
 * Determine if a "feature" supports "los"
 */
#define feat_supports_los(F) \
    (have_flag(f_info[(F)].flags, FF_LOS))


/*
 * Determine if a "legal" grid supports "los"
 */
#define cave_los_bold(Y,X) \
    (feat_supports_los(cave[(Y)][(X)].feat))

#define cave_los_grid(C) \
    (feat_supports_los((C)->feat))


/*
 * Determine if a "legal" grid is a "clean" floor grid
 * Determine if terrain-change spells are allowed in a grid.
 *
 * Line 1 -- forbid non-floors
 * Line 2 -- forbid object terrains
 * Line 3 -- forbid normal objects
 */
#define cave_clean_bold(Y,X) \
    (cave_have_flag_bold((Y), (X), FF_FLOOR) && \
     !(cave[Y][X].info & CAVE_OBJECT) && \
      (cave[Y][X].o_idx == 0))


/*
 * Determine if an object can be dropped on a "legal" grid
 *
 * Line 1 -- forbid non-drops
 * Line 2 -- forbid object terrains
 */
#define cave_drop_bold(Y,X) \
    (cave_have_flag_bold((Y), (X), FF_DROP) && \
     !(cave[Y][X].info & CAVE_OBJECT))


/*
 * Determine if a "legal" grid is an "empty" floor grid
 * Determine if monsters are allowed to move into a grid
 *
 * Line 1 -- forbid non-placement grids
 * Line 2 -- forbid normal monsters
 * Line 3 -- forbid the player
 */
#define cave_empty_bold(Y,X) \
    (cave_have_flag_bold((Y), (X), FF_PLACE) && \
     !(cave[Y][X].m_idx) && \
     !player_bold(Y,X))


/*
 * Determine if a "legal" grid is an "empty" floor grid
 * Determine if monster generation is allowed in a grid
 *
 * Line 1 -- forbid non-empty grids
 * Line 2 -- forbid trees while dungeon generation
 */
#define cave_empty_bold2(Y,X) \
    (cave_empty_bold(Y,X) && \
     (character_dungeon || !cave_have_flag_bold((Y), (X), FF_TREE)))


/*
 * Determine if a "legal" grid is an "naked" floor grid
 *
 * Line 1 -- forbid non-clean gird
 * Line 2 -- forbid monsters
 * Line 3 -- forbid the player
 */
#define cave_naked_bold(Y,X) \
    (cave_clean_bold(Y,X) && \
     !(cave[Y][X].m_idx) && \
     !player_bold(Y,X))


/*
 * Determine if a "legal" grid is "permanent"
 *
 * Line 1 -- permanent flag
 */
#define cave_perma_bold(Y,X) \
    (cave_have_flag_bold((Y), (X), FF_PERMANENT))


/*
 * Grid based version of "cave_empty_bold()"
 */
#define cave_empty_grid(C) \
    (cave_have_flag_grid((C), FF_PLACE) && \
     !((C)->m_idx) && \
     !player_grid(C))


/*
 * Grid based version of "cave_perma_bold()"
 */
#define cave_perma_grid(C) \
    (cave_have_flag_grid((C), FF_PERMANENT))


#define pattern_tile(Y,X) \
    (cave_have_flag_bold((Y), (X), FF_PATTERN))

/*
 * Does the grid stop disintegration?
 */
#define cave_stop_disintegration(Y,X) \
    (!cave_have_flag_bold((Y), (X), FF_PROJECT) && \
     (!cave_have_flag_bold((Y), (X), FF_HURT_DISI) || \
      cave_have_flag_bold((Y), (X), FF_PERMANENT)))


/*
 * Determine if a "legal" grid is within "los" of the player
 *
 * Note the use of comparison to zero to force a "boolean" result
 */
#define player_has_los_grid(C) \
    (((C)->info & (CAVE_VIEW)) != 0)

/*
 * Determine if a "legal" grid is within "los" of the player
 *
 * Note the use of comparison to zero to force a "boolean" result
 */
#define player_has_los_bold(Y,X) \
    (((cave[Y][X].info & (CAVE_VIEW)) != 0) || p_ptr->inside_battle)


/*
 * Determine if a "feature" is "permanent wall"
 */
#define permanent_wall(F) \
    (have_flag((F)->flags, FF_WALL) && \
     have_flag((F)->flags, FF_PERMANENT))

/*
 * Get feature mimic from f_info[] (applying "mimic" field)
 */
#define get_feat_mimic(C) \
    (f_info[(C)->mimic ? (C)->mimic : (C)->feat].mimic)

#define update_playtime() \
{\
    time_t tmp;\
    tmp = time(NULL);\
    if (start_time)\
        playtime += (u32b)(tmp - start_time);\
    start_time = tmp;\
}

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
#define TERM_DARK                0  /* 'd' */   /* 0,0,0 */
#define TERM_WHITE               1  /* 'w' */   /* 4,4,4 */
#define TERM_SLATE               2  /* 's' */   /* 2,2,2 */
#define TERM_ORANGE              3  /* 'o' */   /* 4,2,0 */
#define TERM_RED                 4  /* 'r' */   /* 3,0,0 */
#define TERM_GREEN               5  /* 'g' */   /* 0,2,1 */
#define TERM_BLUE                6  /* 'b' */   /* 0,0,4 */
#define TERM_UMBER               7  /* 'u' */   /* 2,1,0 */
#define TERM_L_DARK              8  /* 'D' */   /* 1,1,1 */
#define TERM_L_WHITE             9  /* 'W' */   /* 3,3,3 */
#define TERM_VIOLET             10  /* 'v' */   /* 4,0,4 */
#define TERM_YELLOW             11  /* 'y' */   /* 4,4,0 */
#define TERM_L_RED              12  /* 'R' */   /* 4,0,0 */
#define TERM_L_GREEN            13  /* 'G' */   /* 0,4,0 */
#define TERM_L_BLUE             14  /* 'B' */   /* 0,4,4 */
#define TERM_L_UMBER            15  /* 'U' */   /* 3,2,1 */


/*
 * Not using graphical tiles for this feature?
 */
#define is_ascii_graphics(A) (!((A) & 0x80))


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
# undef MESSAGE_MAX
# define MESSAGE_MAX    128
# undef MESSAGE_BUF
# define MESSAGE_BUF    4096
#endif


/*
 * Buildings actions
 */
#define BACT_NOTHING                 0
#define BACT_RESEARCH_ITEM           1
#define BACT_TOWN_HISTORY            2
#define BACT_RACE_LEGENDS            3
#define BACT_GREET_KING              4
#define BACT_KING_LEGENDS            5
#define BACT_QUEST                   6
#define BACT_XXX_UNUSED              7
#define BACT_POSTER                  8
#define BACT_ARENA_RULES             9
#define BACT_ARENA                  10
#define BACT_ARENA_LEGENDS          11
#define BACT_IN_BETWEEN             12
#define BACT_GAMBLE_RULES           13
#define BACT_CRAPS                  14
#define BACT_SPIN_WHEEL             15
#define BACT_DICE_SLOTS             16
#define BACT_REST                   17
#define BACT_FOOD                   18
#define BACT_RUMORS                 19
#define BACT_RESEARCH_MONSTER       20
#define BACT_COMPARE_WEAPONS        21
#define BACT_LEGENDS                22
#define BACT_ENCHANT_WEAPON         23
#define BACT_ENCHANT_ARMOR          24
#define BACT_RECHARGE               25
#define BACT_IDENTS                 26
#define BACT_LEARN                  27
#define BACT_HEALING                28
#define BACT_RESTORE                29
#define BACT_ENCHANT_ARROWS         30
#define BACT_ENCHANT_BOW            31
#define BACT_GREET                  32
#define BACT_RECALL                 33
#define BACT_TELEPORT_LEVEL         34
#define BACT_LOSE_MUTATION          35
#define BACT_BATTLE                 36
#define BACT_TSUCHINOKO             37
#define BACT_TARGET                 38
#define BACT_KUBI                   39
#define BACT_KANKIN                 40
#define BACT_HEIKOUKA               41
#define BACT_TELE_TOWN              42
#define BACT_POKER                  43
#define BACT_IDENT_ONE              44
#define BACT_RECHARGE_ALL           45
#define BACT_EVAL_AC                46
#define BACT_GAMBLE_SHOP_POTION     47
#define BACT_GAMBLE_SHOP_SCROLL     48
#define BACT_GAMBLE_SHOP_STAFF      49
#define BACT_GAMBLE_SHOP_WAND       50
#define BACT_GAMBLE_SHOP_ROD        51
#define BACT_GAMBLE_SHOP_ARMOR      52
#define BACT_GAMBLE_SHOP_WEAPON     53
#define BACT_GAMBLE_SHOP_ARTIFACT   54
#define BACT_REPUTATION             55
#define BACT_REFORGE_ARTIFACT       56
#define BACT_CHANGE_NAME            57

/*
 * Quest status
 */
#define QUEST_STATUS_UNTAKEN         0
#define QUEST_STATUS_TAKEN           1
#define QUEST_STATUS_COMPLETED       2
#define QUEST_STATUS_REWARDED        3
#define QUEST_STATUS_FINISHED        4
#define QUEST_STATUS_FAILED          5
#define QUEST_STATUS_FAILED_DONE     6

/*
 * Quest type
 */
#define QUEST_TYPE_KILL_LEVEL                1
#define QUEST_TYPE_KILL_ANY_LEVEL            2
#define QUEST_TYPE_FIND_ARTIFACT             3
#define QUEST_TYPE_FIND_EXIT                 4
#define QUEST_TYPE_KILL_NUMBER               5
#define QUEST_TYPE_KILL_ALL                  6
#define QUEST_TYPE_RANDOM                    7

/*
 * Initialization flags
 */
#define INIT_SHOW_TEXT          0x01
#define INIT_ASSIGN             0x02
#define INIT_CREATE_DUNGEON     0x04
#define INIT_SCROLL_WILDERNESS  0x08
#define INIT_ONLY_BUILDINGS     0x10

/*
 * Quest flags
 */
#define QUEST_FLAG_SILENT  0x01 /* no messages fro completion */
#define QUEST_FLAG_PRESET  0x02 /* quest is outside the main dungeon */
#define QUEST_FLAG_ONCE    0x04 /* quest is marked finished after leaving */

/*
 * Available graphic modes
 */
#define GRAPHICS_NONE       0
#define GRAPHICS_ORIGINAL   1
#define GRAPHICS_ADAM_BOLT  2

/*
 * Modes for the random name generator
 */
#define NAME_DWARF  1
#define NAME_ELF    2
#define NAME_GNOME  3
#define NAME_HOBBIT 4
#define NAME_HUMAN  5
#define NAME_ORC    6


/*
 * Modes for the tokenizer
 */
#define TOKENIZE_CHECKQUOTE 0x01  /* Special handling of single quotes */

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

#define NO_TOWN 6
#define SECRET_TOWN 5

#define NIKKI_HIGAWARI     0
#define NIKKI_BUNSHOU      1
#define NIKKI_ART          2
#define NIKKI_UNIQUE       3
#define NIKKI_FIX_QUEST_C  4
#define NIKKI_FIX_QUEST_F  5
#define NIKKI_RAND_QUEST_C 6
#define NIKKI_RAND_QUEST_F 7
#define NIKKI_MAXDEAPTH    8
#define NIKKI_TRUMP        9
#define NIKKI_STAIR       10
#define NIKKI_RECALL      11
#define NIKKI_TO_QUEST    12
#define NIKKI_TELE_LEV    13
#define NIKKI_BUY         14
#define NIKKI_SELL        15
#define NIKKI_ARENA       16
#define NIKKI_HANMEI      17
#define NIKKI_LEVELUP     18
#define NIKKI_GAMESTART   19
#define NIKKI_WIZ_TELE    20
#define NIKKI_NAMED_PET   21
#define NIKKI_PAT_TELE    22

#define RECORD_NAMED_PET_NAME        0
#define RECORD_NAMED_PET_UNNAME      1
#define RECORD_NAMED_PET_DISMISS     2
#define RECORD_NAMED_PET_DEATH       3
#define RECORD_NAMED_PET_MOVED       4
#define RECORD_NAMED_PET_LOST_SIGHT  5
#define RECORD_NAMED_PET_DESTROY     6
#define RECORD_NAMED_PET_EARTHQUAKE  7
#define RECORD_NAMED_PET_GENOCIDE    8
#define RECORD_NAMED_PET_WIZ_ZAP     9
#define RECORD_NAMED_PET_TELE_LEVEL  10
#define RECORD_NAMED_PET_BLAST       11
#define RECORD_NAMED_PET_HEAL_LEPER  12
#define RECORD_NAMED_PET_COMPACT     13
#define RECORD_NAMED_PET_LOSE_PARENT 14

#define MAX_MANE 16
#define MAX_MAGIC_NUM 108
#define MAX_MONSPELLS 96
#define MONSPELL_TYPE_BOLT 1
#define MONSPELL_TYPE_BALL 2
#define MONSPELL_TYPE_BREATH 3
#define MONSPELL_TYPE_SUMMON 4
#define MONSPELL_TYPE_OTHER 5

#define MAX_KUBI 20

#define DETECT_RAD_DEFAULT 30
#define DETECT_RAD_MAP     30
#define DETECT_RAD_ALL     255

/* Monster Spells */
#define MS_SHRIEK         0
#define MS_XXX1           1
#define MS_DISPEL         2
#define MS_ROCKET         3
#define MS_SHOOT          4
#define MS_XXX2           5
#define MS_XXX3           6
#define MS_BR_STORM       7
#define MS_BR_ACID        8
#define MS_BR_ELEC        9
#define MS_BR_FIRE        10
#define MS_BR_COLD        11
#define MS_BR_POIS        12
#define MS_BR_NETHER      13
#define MS_BR_LITE        14
#define MS_BR_DARK        15
#define MS_BR_CONF        16
#define MS_BR_SOUND       17
#define MS_BR_CHAOS       18
#define MS_BR_DISEN       19
#define MS_BR_NEXUS       20
#define MS_BR_TIME        21
#define MS_BR_INERTIA     22
#define MS_BR_GRAVITY     23
#define MS_BR_SHARDS      24
#define MS_BR_PLASMA      25
#define MS_BR_FORCE       26
#define MS_BR_MANA        27
#define MS_BALL_NUKE      28
#define MS_BR_NUKE        29
#define MS_BALL_CHAOS     30
#define MS_BR_DISI        31
#define MS_BALL_ACID      32
#define MS_BALL_ELEC      33
#define MS_BALL_FIRE      34
#define MS_BALL_COLD      35
#define MS_BALL_POIS      36
#define MS_BALL_NETHER    37
#define MS_BALL_WATER     38
#define MS_BALL_MANA      39
#define MS_BALL_DARK      40
#define MS_DRAIN_MANA     41
#define MS_MIND_BLAST     42
#define MS_BRAIN_SMASH    43
#define MS_CAUSE_1        44
#define MS_CAUSE_2        45
#define MS_CAUSE_3        46
#define MS_CAUSE_4        47
#define MS_BOLT_ACID      48
#define MS_BOLT_ELEC      49
#define MS_BOLT_FIRE      50
#define MS_BOLT_COLD      51
#define MS_STARBURST      52
#define MS_BOLT_NETHER    53
#define MS_BOLT_WATER     54
#define MS_BOLT_MANA      55
#define MS_BOLT_PLASMA    56
#define MS_BOLT_ICE       57
#define MS_MAGIC_MISSILE  58
#define MS_SCARE          59
#define MS_BLIND          60
#define MS_CONF           61
#define MS_SLOW           62
#define MS_SLEEP          63
#define MS_SPEED          64
#define MS_HAND_DOOM      65
#define MS_HEAL           66
#define MS_INVULNER       67
#define MS_BLINK          68
#define MS_TELEPORT       69
#define MS_WORLD          70
#define MS_SPECIAL        71
#define MS_TELE_TO        72
#define MS_TELE_AWAY      73
#define MS_TELE_LEVEL     74
#define MS_PSY_SPEAR      75
#define MS_DARKNESS       76
#define MS_MAKE_TRAP      77
#define MS_FORGET         78
#define MS_RAISE_DEAD     79
#define MS_S_KIN          80
#define MS_S_CYBER        81
#define MS_S_MONSTER      82
#define MS_S_MONSTERS     83
#define MS_S_ANT          84
#define MS_S_SPIDER       85
#define MS_S_HOUND        86
#define MS_S_HYDRA        87
#define MS_S_ANGEL        88
#define MS_S_DEMON        89
#define MS_S_UNDEAD       90
#define MS_S_DRAGON       91
#define MS_S_HI_UNDEAD    92
#define MS_S_HI_DRAGON    93
#define MS_S_AMBERITE     94
#define MS_S_UNIQUE       95
#define MS_THROW          96


#define MON_BEGGAR              12
#define MON_LEPER               13
#define MON_BLACK_MARKET        14
#define MON_GHB                 39
#define MON_NOV_PRIEST          45
#define MON_GRIP                53
#define MON_WOLF                54
#define MON_FANG                55
#define MON_CAVE_SPIDER         60
#define MON_SMEAGOL             63
#define MON_LOUSE               69
#define MON_PIRANHA             70
#define MON_COPPER_COINS        85
#define MON_NOV_PALADIN         97
#define MON_GREEN_G             100
#define MON_DEATH_SWORD         107
#define MON_NOV_PRIEST_G        109
#define MON_SILVER_COINS        117
#define MON_D_ELF               122
#define MON_MANES               128
#define MON_LOST_SOUL           133
#define MON_ROBIN_HOOD          138
#define MON_NOV_PALADIN_G       147
#define MON_PHANTOM_W           152
#define MON_WOUNDED_BEAR        159
#define MON_GIANT_SPIDER        175
#define MON_D_ELF_MAGE          178
#define MON_D_ELF_WARRIOR       182
#define MON_GIANT_PIRANHA       187
#define MON_BLUE_HORROR         189
#define MON_PSEUDO_DRAGON       193
#define MON_GOLD_COINS          195
#define MON_VORPAL_BUNNY        205
#define MON_GAZER               218
#define MON_PRIEST              225
#define MON_D_ELF_PRIEST        226
#define MON_AIR_SPIRIT          227
#define MON_TIGER               230
#define MON_MITHRIL_COINS       239
#define MON_DRUID               241
#define MON_PINK_HORROR         242
#define MON_SOFTWARE_BUG        246
#define MON_HILL_GIANT          255
#define MON_CLAY_GOLEM          261
#define MON_MAGIC_MUSHROOM      267
#define MON_WERERAT             270
#define MON_LIGHT_HOUND         271
#define MON_SHADOW_HOUND        272
#define MON_FROST_GIANT         278
#define MON_CLEAR_HOUND         282
#define MON_UMBER_HULK          283
#define MON_ORC_CAPTAIN         285
#define MON_GELATINOUS_CUBE     286
#define MON_FIRE_GIANT          288
#define MON_BERSERKER           293
#define MON_QUASIT              294
#define MON_FOREST_TROLL        297
#define MON_TWO_HEADED_HYDRA    301
#define MON_WATER_SPIRIT        303
#define MON_EARTH_SPIRIT        305
#define MON_FIRE_SPIRIT         306
#define MON_FIRE_HOUND          307
#define MON_COLD_HOUND          308
#define MON_ENERGY_HOUND        309
#define MON_SHAGRAT             314
#define MON_GORBAG              315
#define MON_STONE_GIANT         321
#define MON_STONE_GOLEM         323
#define MON_PHASE_SPIDER        331
#define MON_EARTH_HOUND         337
#define MON_AIR_HOUND           338
#define MON_WATER_HOUND         340
#define MON_QUYLTHULG           342
#define MON_D_ELF_LORD          348
#define MON_CLOUD_GIANT         349
#define MON_FIRE_VOR            354
#define MON_WATER_VOR           355
#define MON_ARCH_VILE           357
#define MON_COLD_VOR            358
#define MON_ENERGY_VOR          359
#define MON_VAMPIRIC_MIST       365
#define MON_IRON_GOLEM          367
#define MON_JADE_MONK           370
#define MON_BLACK_OOZE          371
#define MON_FLESHHOUND_KHORNE   374
#define MON_D_ELF_WARLOCK       375
#define MON_HAGEN               383
#define MON_MENELDOR            384
#define MON_PHANTOM_B           385
#define MON_FOUR_HEADED_HYDRA   387
#define MON_VAMPIRE_BAT         391
#define MON_C_CRAWLER           395
#define MON_XICLOTLAN           396
#define MON_D_ELF_DRUID         400
#define MON_STONE_TROLL         401
#define MON_TROLL_PRIEST        403
#define MON_GWAIHIR             410
#define MON_ANGEL               417
#define MON_ALBERICH            419
#define MON_HELLBLADE           420
#define MON_ADAMANT_COINS       423
#define MON_ALGROTH             424
#define MON_VIBRATION_HOUND     428
#define MON_NEXUS_HOUND         429
#define MON_VAMPIRE             432
#define MON_SPIRIT_NAGA   436
#define MON_FIVE_HEADED_HYDRA 440
#define MON_GACHAPIN      441
#define MON_BLACK_KNIGHT 442
#define MON_DEEP_ONE      452
#define MON_BASILISK      453
#define MON_ICE_TROLL    454
#define MON_ARCHANGEL     456
#define MON_RING_MIMIC    457
#define MON_YOUNG_BRONZE_DRAGON   462
#define MON_AKLASH       463
#define MON_MITHRIL_GOLEM 464
#define MON_THORONDOR     468
#define MON_SHADOW_DRAKE  471
#define MON_GHOST         477
#define MON_OGRE_SHAMAN   479
#define MON_NEXUS_QUYLTHULG             480
#define MON_NINJA         485
#define MON_STORM_GIANT 487
#define MON_SPECTATOR 488
#define MON_BICLOPS       490
#define MON_IVORY_MONK    492
#define MON_ANTI_PALADIN 497
#define MON_LOGRUS_MASTER    498
#define MON_CHAOS_DRAKE           501
#define MON_LAW_DRAKE             502
#define MON_BALANCE_DRAKE         503
#define MON_ETHEREAL_DRAKE        504
#define MON_ETHER_DRAKE   504
#define MON_GOEMON        505
#define MON_CHERUB        511
#define MON_FIRE_ELEMENTAL  510
#define MON_WATER_ELEMENTAL 512
#define MON_WATER_ELEM    512
#define MON_MULTI_HUED_HOUND  513
#define MON_JURT           517
#define MON_LICH           518
#define MON_MASTER_VAMPIRE 520
#define MON_GREATER_MUMMY  522
#define MON_BLOODLETTER    523
#define MON_BLOODLETTER_KHORNE 523
#define MON_EARTH_ELEMENTAL 525
#define MON_AIR_ELEMENTAL   526
#define MON_EOG_GOLEM 530
#define MON_DREAD         534
#define MON_OLOG         538
#define MON_HALFLING_S    539
#define MON_GRAV_HOUND    540
#define MON_GRAVITY_HOUND     540
#define MON_ACIDIC_CYTOPLASM 541
#define MON_INERTIA_HOUND     542
#define MON_IMPACT_HOUND      543
#define MON_XORN        550
#define MON_REVENANT      555
#define MON_RAAL          557
#define MON_COLOSSUS      558
#define MON_YOUNG_GOLD_DRAGON     559
#define MON_MATURE_BRONZE_DRAGON  562
#define MON_NIGHTBLADE    564
#define MON_BODAK  566
#define MON_ELDER_THING   569
#define MON_IPSISSIMUS   571
#define MON_CHAOS_SPAWN   574
#define MON_CRYPT_THING   577
#define MON_MAGMA_ELEMENTAL 584
#define MON_NEXUS_VOR     587
#define MON_PLASMA_VOR    588
#define MON_TIME_VOR      589
#define MON_MATURE_GOLD_DRAGON    590
#define MON_CRYSTAL_DRAKE         591
#define MON_M_MH_DRAGON   593
#define MON_DEATH_KNIGHT 597
#define MON_MANDOR        598
#define MON_SHIM_VOR      600
#define MON_ANCIENT_BRONZE_DRAGON 602
#define MON_BEHOLDER 603
#define MON_SERAPH        605
#define MON_LOGE            606
#define MON_MONASTIC_LICH 611
#define MON_SEVEN_HEADED_HYDRA 614
#define MON_MOIRE           615
#define MON_KAVLAX        616
#define MON_ETTIN         621
#define MON_NIGHTMARE     622
#define MON_VAMPIRE_LORD  623
#define MON_SPIRIT_TROLL 630
#define MON_ROTTING_QUYLTHULG           633
#define MON_LESSER_TITAN 634
#define MON_NINE_HEADED_HYDRA 635
#define MON_ARCHPRIEST   637
#define MON_XAREN       639
#define MON_JUBJUB        640
#define MON_DEATH_DRAKE           643
#define MON_ANCIENT_GOLD_DRAGON   645
#define MON_GREAT_CRYSTAL_DRAKE   646
#define MON_WYRD_SISTER  647
#define MON_CLUB_DEMON          648
#define MON_DEATH_QUASIT        649
#define MON_FALLEN_ANGEL 652
#define MON_UBBO_SATHLA         655
#define MON_D_ELF_SORC          657
#define MON_MASTER_LICH         658
#define MON_RINALDO             660
#define MON_ARCHON              661
#define MON_UND_BEHOLDER        664
#define MON_UNDEAD_BEHOLDER     664
#define MON_SHADOW_DEMON        665
#define MON_IRON_LICH           666
#define MON_JACK_SHADOWS        670
#define MON_JUGGERNAUT_KHORNE   672
#define MON_ETHEREAL_DRAGON     676
#define MON_QUAKER              679
#define MON_LLOIGOR             682
#define MON_SHOGGOTH            685
#define MON_ARIEL               687
#define MON_ELEVEN_HEADED_HYDRA 688
#define MON_HIGH_PRIEST         689
#define MON_DREADMASTER         690
#define MON_DAWN                693
#define MON_NAZGUL              696
#define MON_SMAUG               697
#define MON_STORMBRINGER        698
#define MON_ULTRA_PALADIN       699
#define MON_LEPRECHAUN_FANATIC  700
#define MON_DRACOLICH           701
#define MON_G_TITAN             702
#define MON_GREATER_TITAN       702
#define MON_ENT                 708
#define MON_HRU                 709
#define MON_FAFNER              712
#define MON_GLAURUNG            715
#define MON_G_BALROG            720
#define MON_GREATER_BALROG      720
#define MON_NETHER_HOUND        724
#define MON_TIME_HOUND          725
#define MON_PLASMA_HOUND        726
#define MON_DEMONIC_QUYLTHULG   727
#define MON_ULIK                729
#define MON_HELL_KNIGHT         731
#define MON_BULLGATES           732
#define MON_SANTACLAUS          733
#define MON_LORD_CHAOS          737
#define MON_KHAMUL              738
#define MON_TINDALOS            739
#define MON_HOUND_OF_TINDALOS   739
#define MON_LESSER_KRAKEN       740
#define MON_DEMILICH            742
#define MON_PHOENIX             743
#define MON_NIGHTCRAWLER        744
#define MON_HAND_DRUJ           748
#define MON_EYE_DRUJ            749
#define MON_SKULL_DRUJ          750
#define MON_CHAOS_VOR           751
#define MON_AETHER_VOR          752
#define MON_LERNEAN_HYDRA       754
#define MON_BLOODTHIRSTER       758
#define MON_DRACONIC_QUYLTHULG  759
#define MON_NYOGTHA             760
#define MON_FUNDIN              762
#define MON_DWORKIN             763
#define MON_NIGHTWALKER         768
#define MON_RAPHAEL             769
#define MON_SARUMAN             771
#define MON_GANDALF             772
#define MON_BRAND               773
#define MON_SHADOWLORD          774
#define MON_GREATER_KRAKEN      775
#define MON_ARCHLICH            776
#define MON_CHAOS_HOUND         779
#define MON_VLAD                780
#define MON_ULT_BEHOLDER        781
#define MON_ULTIMATE_BEHOLDER   781
#define MON_GREAT_WYRM_OF_CHAOS 783
#define MON_GREAT_WYRM_OF_LAW   784
#define MON_GREAT_WYRM_OF_BALANCE 785
#define MON_SHAMBLER            786
#define MON_HYPNOS              787
#define MON_BLEYS               789
#define MON_FIONA               791
#define MON_SKY_DRAKE           793
#define MON_JULIAN              794
#define MON_BLACK_REAVER        798
#define MON_CAINE               799
#define MON_MASTER_Q            800
#define MON_MASTER_QUYLTHULG    800
#define MON_G_DRACONIC_Q        801
#define MON_GREATER_DRACONIC_QUYLTHULG  801
#define MON_GREATER_ROTTING_QUYLTHULG   802
#define MON_VECNA               804
#define MON_OMARAX              805
#define MON_GERARD              807
#define MON_UNGOLIANT           808
#define MON_ATLACH_NACHA        809
#define MON_Y_GOLONAC           810
#define MON_AETHER_HOUND        811
#define MON_WARP_DEMON          812
#define MON_ERIC                813
#define MON_UNMAKER             815
#define MON_CYBER               816
#define MON_KLING               819
#define MON_CORWIN              820
#define MON_EMPEROR_QUYLTHULG   821
#define MON_ANGMAR              825
#define MON_CANTORAS            830
#define MON_MEPHISTOPHELES      831
#define MON_GODZILLA            832
#define MON_YMIR                834
#define MON_SPAWN_CTH           836
#define MON_SURTUR              837
#define MON_TARRASQUE           838
#define MON_LUNGORTHIN          839
#define MON_CYBER_KING          843
#define MON_OREMORJ             843
#define MON_WYRM_POWER          847
#define MON_NODENS              849
#define MON_CARCHAROTH          850
#define MON_JORMUNGAND          854
#define MON_DESTROYER           855
#define MON_GOTHMOG             856
#define MON_G_CTHULHU           857
#define MON_SAURON              858
#define MON_UNICORN_ORD         859
#define MON_OBERON              860
#define MON_MORGOTH             861
#define MON_SERPENT             862
#define MON_ONE_RING            864
#define MON_CAAWS               866
#define MON_CULVERIN            867
#define MON_EBONY_MONK          870
#define MON_HAGURE              871
#define MON_METAL_BABBLE        871
#define MON_OROCHI              872
#define MON_ECHIZEN             873
#define MON_SPECT_WYRM          874
#define MON_SPECTRAL_WYRM       874
#define MON_STORM_TROLL         875
#define MON_DIO                 878
#define MON_OHMU                879
#define MON_WONG                880
#define MON_ZOMBI_SERPENT       883
#define MON_D_ELF_SHADE         886
#define MON_MANA_HOUND          887
#define MON_VENOM_WYRM          890
#define MON_TROLL_KING          894
#define MON_SKY_GOLEM           895
#define MON_BAZOOKER            896
#define MON_SHARD_VOR           897
#define MON_FIRE_TROLL          899
#define MON_SMALL_KRAKEN        903
#define MON_POLEAXE_OF_ANIMATED_ATTACK 908
#define MON_MYSTIC              915
#define MON_MASTER_MYS          916
#define MON_MASTER_MYSTIC       916
#define MON_G_MASTER_MYS        917
#define MON_GRAND_MASTER_MYSTIC 917
#define MON_IE                  921
#define MON_TSUCHINOKO          926
#define MON_GCWADL              929
#define MON_LOCKE_CLONE         930
#define MON_CALDARM             931
#define MON_BANORLUPART         932
#define MON_BANOR               933
#define MON_LUPART              934
#define MON_KENSHIROU           936
#define MON_LEMS                937
#define MON_W_KNIGHT            938
#define MON_HOARMURATH          939
#define MON_LESSER_BALROG       940
#define MON_BONE_DRAGON         941
#define MON_PLANETAR            942
#define MON_SOLAR               943
#define MON_BIKETAL             945
#define MON_RICH                948
#define MON_IKETA               949
#define MON_B_DEATH_SWORD       953
#define MON_BROKEN_DEATH_SWORD  953
#define MON_YASE_HORSE          955
#define MON_HORSE               956
#define MON_BOTEI               963
#define MON_KAGE                964
#define MON_JAIAN               967
#define MON_BELD                973
#define MON_THAT_BAT            975
#define MON_SHUTEN              979
#define MON_FENGHUANG           988
#define MON_KIRIN               989
#define MON_NAR                 996
#define MON_BAHAMUT             1000
#define MON_SUKE                1001
#define MON_KAKU                1002
#define MON_GHOST_Q             1003
#define MON_PIP                 1004
#define MON_DWAR                1009
#define MON_A_GOLD              1010
#define MON_A_SILVER            1011
#define MON_ROLENTO             1013
#define MON_RAOU                1018
#define MON_SHURYUUDAN          1023
#define MON_WAHHA               1031
#define MON_DEBBY               1032
#define MON_KNI_TEMPLAR         1037
#define MON_PALADIN             1038
#define MON_CHAMELEON           1040
#define MON_CHAMELEON_K         1041
#define MON_TOPAZ_MONK          1047
#define MON_STONE_DRAGON        1048
#define MON_STEEL_DRAGON        1049
#define MON_ATLAS               1050
#define MON_KRONOS              1051
#define MON_ELDER_VAMPIRE       1058
#define MON_NOBORTA             1059
#define MON_MORI_TROLL          1060
#define MON_BARNEY              1061
#define MON_GROO                1062
#define MON_LOUSY               1063
#define MON_JIZOTAKO            1065
#define MON_TANUKI              1067
#define MON_REINDEER            1081
#define MON_DEATH_BEAST         1082
#define MON_ULT_MAGUS           1083
#define MON_DEATH_SCYTHE        1084
#define MON_SPELLWARP           1085
#define MON_SPELLWARP_AUTOMATON 1085
#define MON_TALOS               1086
#define MON_TONBERRY            1087
#define MON_NINJA_TONBERRY      1088
#define MON_MASTER_TONBERRY     1089
#define MON_THE_HOARD           1090
#define MON_VARIANT_MAINTAINER  1094
#define MON_MONKEY_CLONE        1095
#define MON_ZEUS                1096
#define MON_POSEIDON            1097
#define MON_HADES               1098
#define MON_ATHENA              1099
#define MON_ARES                1100
#define MON_HERMES              1101
#define MON_APOLLO              1102
#define MON_ARTEMIS             1103
#define MON_HEPHAESTUS          1104
#define MON_HERA                1105
#define MON_DEMETER             1106
#define MON_APHRODITE           1107
#define MON_THE_METAL_BABBLE    1110
#define MON_GREATER_DEMONIC_QUYLTHULG   1123
#define MON_ROCK_GIANT          1124
#define MON_ICE_GIANT           1125
#define MON_ELDER_FIRE_GIANT    1126
#define MON_TYPHOEUS            1127
#define MON_ELDER_STORM_GIANT   1128
#define MON_POSSESSOR_SOUL      1129
#define MON_MARILITH            1130
#define MON_MIMIC               1131
#define MON_MULTIHUED_CENTIPEDE 1132

/* The Metal Babble guards the Arena dungeon, but this requires the guardian to be a unique
   monster or the dungeon never gets flagged as completed. Note, this messes up the needle
   code in py_attack_aux() since the needle doesn't work on uniques, but I think I have
   that fixed now ... sigh
*/
#define MON_HAGURE2        1110

#define MON_ARTHUR        1111
#define MON_GALAHAD     1114
#define MON_CAMELOT_KNIGHT 1117
#define MAX_CAMELOT_KNIGHT_NUM 10
#define MON_GRAND_FEARLORD 1121

/* Maximum "Nazguls" number */
#define MAX_NAZGUL_NUM 5

#define DO_AUTOPICK       0x01
#define DO_AUTODESTROY    0x02
#define DO_DISPLAY        0x04
#define DONT_AUTOPICK     0x08
#define ITEM_DISPLAY      0x10
#define DO_QUERY_AUTOPICK 0x20
#define DO_AUTO_ID        0x40


#define MAGIC_GLOVE_REDUCE_MANA 0x0001
#define MAGIC_FAIL_5PERCENT     0x0002
#define MAGIC_GAIN_EXP          0x0004

#define SPELL_DD_S 27
#define SPELL_DD_T 13
#define SPELL_SW   22
#define SPELL_KABE 20

#define KNOW_STAT   0x01
#define KNOW_HPRATE 0x02

/*
 * Music songs
 */
#define MUSIC_NONE              0
#define MUSIC_SLOW              1
#define MUSIC_BLESS             2
#define MUSIC_STUN              3
#define MUSIC_L_LIFE            4
#define MUSIC_FEAR              5
#define MUSIC_HERO              6
#define MUSIC_MIND              7
#define MUSIC_STEALTH           8
#define MUSIC_ID                9
#define MUSIC_CONF              10
#define MUSIC_SOUND             11
#define MUSIC_CHARM             12
#define MUSIC_WALL              13
#define MUSIC_RESIST            14
#define MUSIC_SPEED             15
#define MUSIC_DISPEL            16
#define MUSIC_SARUMAN           17
#define MUSIC_QUAKE             18
#define MUSIC_STASIS            19
#define MUSIC_SHERO             20
#define MUSIC_H_LIFE            21
#define MUSIC_INVULN            22
#define MUSIC_PSI               23

#define MUSIC_DETECT            101

#define music_singing(X) ((p_ptr->pclass == CLASS_BARD) && (p_ptr->magic_num1[0] == (X)))
#define music_singing_any() ((p_ptr->pclass == CLASS_BARD) && p_ptr->magic_num1[0])

#define HISSATSU_NONE   0
#define HISSATSU_2      1
#define HISSATSU_3WAY   2
#define HISSATSU_SUTEMI 3
#define HISSATSU_FIRE   4
#define HISSATSU_COLD   5
#define HISSATSU_POISON 6
#define HISSATSU_ELEC   7
#define HISSATSU_NYUSIN 8
#define HISSATSU_FUKI   9
#define HISSATSU_MAJIN  10
#define HISSATSU_BOOMER 11
#define HISSATSU_DRAIN  12
#define HISSATSU_SEKIRYUKA 13
#define HISSATSU_OTAKEBI 14
#define HISSATSU_SHOUGE 15
#define HISSATSU_CONF   16
#define HISSATSU_ISSEN  17
#define HISSATSU_KYUSHO 18
#define HISSATSU_KONSIN 19
#define HISSATSU_HYAKU  20
#define HISSATSU_MINEUCHI 21
#define HISSATSU_MEKIKI 22
#define HISSATSU_ZANMA  23
#define HISSATSU_UNDEAD 24
#define HISSATSU_HAGAN  25
#define HISSATSU_QUAKE  26
#define HISSATSU_COUNTER 27
#define HISSATSU_HARAI  28
#define HISSATSU_3DAN   29
#define HISSATSU_100NIN 30

#define WEAPONMASTER_FRENZY 31
#define WEAPONMASTER_RETALIATION 32
#define WEAPONMASTER_CRUSADERS_STRIKE 33
#define WEAPONMASTER_VICIOUS_STRIKE 35
#define WEAPONMASTER_MANY_STRIKE 36
#define WEAPONMASTER_WHIRLWIND 37
#define WEAPONMASTER_PIERCING_STRIKE 38
#define WEAPONMASTER_PROXIMITY_ALERT 40
#define WEAPONMASTER_AUTO_BLOW 41
#define WEAPONMASTER_KNOCK_BACK 42
#define WEAPONMASTER_REAPING 43
#define WEAPONMASTER_CUNNING_STRIKE 44
#define WEAPONMASTER_SMITE_EVIL 45
#define WEAPONMASTER_ELUSIVE_STRIKE 46

#define MELEE_AWESOME_BLOW 47
#define ROGUE_ASSASSINATE 48

#define MAULER_CRITICAL_BLOW 49
#define MAULER_STUNNING_BLOW 50
#define MAULER_KNOCKBACK     51
#define MAULER_CRUSHING_BLOW 53
#define MAULER_SCATTER       54

#define MYSTIC_STUN          55
#define MYSTIC_FIRE          56
#define MYSTIC_COLD          57
#define MYSTIC_ELEC          58
#define MYSTIC_ACID          59
#define MYSTIC_POIS          60
#define MYSTIC_CRITICAL      61
#define MYSTIC_CONFUSE       62
#define MYSTIC_KNOCKOUT      63
#define MYSTIC_KILL          64

#define PY_ATTACK_ACID       65

#define GOLEM_BIG_PUNCH      66

#define PY_POWER_ATTACK      67

#define DRAGON_REND          68
#define DRAGON_DEADLY_BITE   69
#define DRAGON_SNATCH        70
#define DRAGON_RAPID_STRIKE  71
#define DRAGON_POWER_STRIKE  72
#define DRAGON_TAIL_SWEEP    73
#define DRAGON_SMITE_EVIL    74

#define WEAPONMASTER_CLEAVE  75

#define DRACONIAN_STRIKE_FIRE 76
#define DRACONIAN_STRIKE_COLD 77
#define DRACONIAN_STRIKE_ELEC 78
#define DRACONIAN_STRIKE_ACID 79
#define DRACONIAN_STRIKE_POIS 80
#define DRACONIAN_STRIKE_CONF 81
#define DRACONIAN_STRIKE_STUN 82
#define DRACONIAN_STRIKE_VORPAL 83
#define DRACONIAN_STRIKE_VAMP 84

#define WEAPONMASTER_FLURRY  85

#define HISSATSU_IAI    100

#define DUNGEON_MODE_NONE       0
#define DUNGEON_MODE_AND        1
#define DUNGEON_MODE_NAND       2
#define DUNGEON_MODE_OR         3
#define DUNGEON_MODE_NOR        4

/*** Dungeon type flags -- DG ***/
#define DF1_WINNER              0x00000001
#define DF1_MAZE                0x00000002
#define DF1_SMALLEST            0x00000004
#define DF1_BEGINNER            0x00000008
#define DF1_BIG                 0x00000010
#define DF1_NO_DOORS            0x00000020
#define DF1_WATER_RIVER         0x00000040
#define DF1_LAVA_RIVER          0x00000080
#define DF1_CURTAIN             0x00000100
#define DF1_GLASS_DOOR          0x00000200
#define DF1_CAVE                0x00000400
#define DF1_CAVERN              0x00000800
#define DF1_RANDOM              0x00001000
#define DF1_XXX13               0x00002000
#define DF1_XXX14               0x00004000
#define DF1_XXX15               0x00008000
#define DF1_FORGET              0x00010000
#define DF1_LAKE_WATER          0x00020000
#define DF1_LAKE_LAVA           0x00040000
#define DF1_LAKE_RUBBLE         0x00080000
#define DF1_LAKE_TREE           0x00100000
#define DF1_NO_VAULT            0x00200000
#define DF1_ARENA               0x00400000
#define DF1_DESTROY             0x00800000
#define DF1_GLASS_ROOM          0x01000000
#define DF1_NO_CAVE             0x02000000
#define DF1_NO_MAGIC            0x04000000
#define DF1_NO_MELEE            0x08000000
#define DF1_CHAMELEON           0x10000000
#define DF1_DARKNESS            0x20000000
#define DF1_XXX30               0x40000000
#define DF1_XXX31               0x80000000

#define DF1_LAKE_MASK (DF1_LAKE_WATER | DF1_LAKE_LAVA | DF1_LAKE_RUBBLE | DF1_LAKE_TREE)

#define DUNGEON_ANGBAND  1
#define DUNGEON_CAMELOT  2
#define DUNGEON_ORC      3
#define DUNGEON_MAZE     4
#define DUNGEON_DRAGON   5
#define DUNGEON_GRAVE    6
#define DUNGEON_WOOD     7
#define DUNGEON_VOLCANO  8
#define DUNGEON_HELL     9
#define DUNGEON_HEAVEN   10
#define DUNGEON_OCEAN    11
#define DUNGEON_CASTLE   12
#define DUNGEON_CTH      13
#define DUNGEON_MOUNTAIN 14
#define DUNGEON_GOLD     15
#define DUNGEON_NO_MAGIC 16
#define DUNGEON_NO_MELEE 17
#define DUNGEON_CHAMELEON 18
#define DUNGEON_DARKNESS 19
#define DUNGEON_OLYMPUS  22
#define DUNGEON_ARENA    25
#define DUNGEON_STRONGHOLD 30


#define DUNGEON_FEAT_PROB_NUM 3


/*
 * Flags for change floor mode
 */
#define CFM_UP             0x0001  /* Move up */
#define CFM_DOWN           0x0002  /* Move down */
#define CFM_LONG_STAIRS  0x0004  /* Randomly occurred long stairs/shaft */
#define CFM_XXX       0x0008  /* XXX */
#define CFM_SHAFT          0x0010  /* Shaft */
#define CFM_RAND_PLACE   0x0020  /* Arrive at random grid */
#define CFM_RAND_CONNECT 0x0040  /* Connect with random stairs */
#define CFM_SAVE_FLOORS  0x0080  /* Save floors */
#define CFM_NO_RETURN    0x0100  /* Flee from random quest etc... */
#define CFM_FIRST_FLOOR  0x0200  /* Create exit from the dungeon */


/*
 * Flags for save/load temporal saved floor file
 */
#define SLF_SECOND          0x0001  /* Called from another save/load function */
#define SLF_NO_KILL      0x0002  /* Don't kill temporal files */


/*
 * Fields for wr_item()/rd_item() stored in savefiles (byte).
 */
enum object_save_fields_e {
    SAVE_ITEM_DONE = 0,
    SAVE_ITEM_PVAL,
    SAVE_ITEM_DISCOUNT,
    SAVE_ITEM_NUMBER,
    SAVE_ITEM_NAME1,
    SAVE_ITEM_NAME2,
    SAVE_ITEM_NAME3,
    SAVE_ITEM_ART_NAME,
    SAVE_ITEM_TIMEOUT,
    SAVE_ITEM_COMBAT,
    SAVE_ITEM_ARMOR,
    SAVE_ITEM_DAMAGE_DICE,
    SAVE_ITEM_IDENT,
    SAVE_ITEM_MARKED_BYTE,
    SAVE_ITEM_FEELING,
    SAVE_ITEM_INSCRIPTION,
    SAVE_ITEM_ART_FLAGS_0,
    SAVE_ITEM_ART_FLAGS_1,
    SAVE_ITEM_ART_FLAGS_2,
    SAVE_ITEM_ART_FLAGS_3,
    SAVE_ITEM_ART_FLAGS_4,
    SAVE_ITEM_ART_FLAGS_5,
    SAVE_ITEM_ART_FLAGS_6,
    SAVE_ITEM_ART_FLAGS_7,
    SAVE_ITEM_ART_FLAGS_8,
    SAVE_ITEM_ART_FLAGS_9,
    SAVE_ITEM_CURSE_FLAGS,
    SAVE_ITEM_RUNE_FLAGS,
    SAVE_ITEM_HELD_M_IDX,
    SAVE_ITEM_XTRA1,
    SAVE_ITEM_XTRA2,
    SAVE_ITEM_XTRA3,
    SAVE_ITEM_XTRA4,
    SAVE_ITEM_XTRA5_OLD,
    SAVE_ITEM_ACTIVATION,
    SAVE_ITEM_MULT,
    SAVE_ITEM_MARKED,
    SAVE_ITEM_XTRA5,
    SAVE_ITEM_KNOWN_FLAGS_0,
    SAVE_ITEM_KNOWN_FLAGS_1,
    SAVE_ITEM_KNOWN_FLAGS_2,
    SAVE_ITEM_KNOWN_FLAGS_3,
    SAVE_ITEM_KNOWN_FLAGS_4,
    SAVE_ITEM_KNOWN_FLAGS_5,
    SAVE_ITEM_KNOWN_FLAGS_6,
    SAVE_ITEM_KNOWN_FLAGS_7,
    SAVE_ITEM_KNOWN_FLAGS_8,
    SAVE_ITEM_KNOWN_FLAGS_9,
    SAVE_ITEM_KNOWN_CURSE_FLAGS,
};

/*
 * Fields for wr_monster()/rd_monster() stored in savefiles (byte).
 */
enum mon_save_fields_e {
    SAVE_MON_DONE = 0,
    SAVE_MON_AP_R_IDX,
    SAVE_MON_SUB_ALIGN,
    SAVE_MON_TIMER,
    SAVE_MON_TARGET_Y,
    SAVE_MON_TARGET_X,
    SAVE_MON_SMART,
    SAVE_MON_EXP,
    SAVE_MON_MFLAG2,
    SAVE_MON_NICKNAME,
    SAVE_MON_PARENT,
    SAVE_MON_PACK_IDX,
    SAVE_MON_AC,
    SAVE_MON_MELEE,
    SAVE_MON_EGO_WHIP,
    SAVE_MON_ANTI_MAGIC,
    SAVE_MON_FORGOT_4,
    SAVE_MON_FORGOT_5,
    SAVE_MON_FORGOT_6,
    SAVE_MON_SUMMON_CT,
    SAVE_MON_DROP_CT,
    SAVE_MON_STOLEN_CT,
    SAVE_MON_PEXP,
    SAVE_MON_PARALYZED,
    SAVE_MON_ANGER_CT,
};

/* Sub-alignment flags for neutral monsters */
#define SUB_ALIGN_NEUTRAL 0x0000
#define SUB_ALIGN_EVIL    0x0001
#define SUB_ALIGN_GOOD    0x0002

/* Temporary flags macro */
#define IS_FAST() (p_ptr->fast || music_singing(MUSIC_SPEED) || music_singing(MUSIC_SHERO) || wild_has_power(WILD_SPEED))
#define IS_LIGHT_SPEED() (p_ptr->lightspeed || wild_has_power(WILD_LIGHT_SPEED))
#define IS_INVULN() (p_ptr->invuln || music_singing(MUSIC_INVULN) || wild_has_power(WILD_INVULN) || (p_ptr->special_defense & DEFENSE_SANCTUARY))
#define IS_HERO() (p_ptr->hero || music_singing(MUSIC_HERO) || music_singing(MUSIC_SHERO) || p_ptr->constant_hero || player_is_monster_king())
#define IS_BLESSED() (p_ptr->blessed || music_singing(MUSIC_BLESS) || hex_spelling(HEX_BLESS) || wild_has_power(WILD_BLESS))
#define IS_SHERO() (p_ptr->shero || p_ptr->pclass == CLASS_BERSERKER || wild_has_power(WILD_BERSERK))
#define IS_OPPOSE_ACID() (p_ptr->oppose_acid || music_singing(MUSIC_RESIST) || (p_ptr->special_defense & KATA_MUSOU) || wild_has_power(WILD_RESIST))
#define IS_OPPOSE_ELEC() (p_ptr->oppose_elec || music_singing(MUSIC_RESIST) || (p_ptr->special_defense & KATA_MUSOU) || wild_has_power(WILD_RESIST))
#define IS_OPPOSE_FIRE() (p_ptr->oppose_fire || music_singing(MUSIC_RESIST) || (p_ptr->special_defense & KATA_MUSOU) || wild_has_power(WILD_RESIST))
#define IS_OPPOSE_COLD() (p_ptr->oppose_cold || music_singing(MUSIC_RESIST) || (p_ptr->special_defense & KATA_MUSOU) || wild_has_power(WILD_RESIST))
#define IS_OPPOSE_POIS() (p_ptr->oppose_pois || music_singing(MUSIC_RESIST) || (p_ptr->special_defense & KATA_MUSOU) || wild_has_power(WILD_RESIST))
#define IS_RESIST() ((p_ptr->oppose_acid && p_ptr->oppose_elec && p_ptr->oppose_fire && p_ptr->oppose_cold && p_ptr->oppose_pois) || music_singing(MUSIC_RESIST) || (p_ptr->special_defense & KATA_MUSOU) || wild_has_power(WILD_RESIST))
#define IS_TIM_ESP() (p_ptr->tim_esp || music_singing(MUSIC_MIND) || (p_ptr->concent >= CONCENT_TELE_THRESHOLD) || wild_has_power(WILD_ESP))
#define IS_TIM_STEALTH() (p_ptr->tim_stealth || music_singing(MUSIC_STEALTH))
#define IS_TIM_INFRA() (p_ptr->tim_infra || wild_has_power(WILD_INFRAVISION))
#define IS_PROT_EVIL() (p_ptr->protevil || wild_has_power(WILD_PROT_EVIL))
#define IS_RESIST_MAGIC() (p_ptr->resist_magic || wild_has_power(WILD_MAGIC_RESIST))
#define IS_STONE_SKIN() (p_ptr->shield || wild_has_power(WILD_STONE_SKIN))
#define IS_PASSWALL() (p_ptr->kabenuke || wild_has_power(WILD_PASSWALL))
#define IS_REVENGE() (p_ptr->tim_eyeeye || hex_spelling(HEX_EYE_FOR_EYE) || wild_has_power(WILD_REVENGE) || weaponmaster_get_toggle() == TOGGLE_SHIELD_REVENGE || psion_backlash())
#define IS_WRAITH() (p_ptr->wraith_form || wild_has_power(WILD_WRAITH))

/* Multishadow effects is determined by turn */
#define CHECK_MULTISHADOW() (p_ptr->multishadow && (game_turn & 1))

/* Is "teleport level" ineffective to this target? */
#define TELE_LEVEL_IS_INEFF(TARGET) \
    (p_ptr->inside_arena || p_ptr->inside_battle || \
     (p_ptr->inside_quest && !random_quest_number(dun_level)) || \
     (((TARGET) <= 0) && (quest_number(dun_level) || (dun_level >= d_info[dungeon_type].maxdepth)) && \
      (dun_level >= 1) && ironman_downward))


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


#define MTIMED_CSLEEP   0 /* Monster is sleeping */
#define MTIMED_FAST     1 /* Monster is temporarily fast */
#define MTIMED_SLOW     2 /* Monster is temporarily slow */
#define MTIMED_STUNNED  3 /* Monster is stunned */
#define MTIMED_CONFUSED 4 /* Monster is confused */
#define MTIMED_MONFEAR  5 /* Monster is afraid */
#define MTIMED_INVULNER 6 /* Monster is temporarily invulnerable */

#define MTIMED_MAX      7
#define MAX_MTIMED      7

#define MON_CSLEEP(M_PTR)   ((M_PTR)->mtimed[MTIMED_CSLEEP])
#define MON_FAST(M_PTR)     ((M_PTR)->mtimed[MTIMED_FAST])
#define MON_SLOW(M_PTR)     ((M_PTR)->mtimed[MTIMED_SLOW])
#define MON_STUNNED(M_PTR)  ((M_PTR)->mtimed[MTIMED_STUNNED])
#define MON_CONFUSED(M_PTR) ((M_PTR)->mtimed[MTIMED_CONFUSED])
#define MON_MONFEAR(M_PTR)  ((M_PTR)->mtimed[MTIMED_MONFEAR])
#define MON_INVULNER(M_PTR) ((M_PTR)->mtimed[MTIMED_INVULNER])

/*
 * Bit flags for screen_object()
 */
#define SCROBJ_FAKE_OBJECT  0x00000001
#define SCROBJ_FORCE_DETAIL 0x00000002

/* Sniper */
#define SP_NONE          0
#define SP_LITE          1
#define SP_AWAY          2
#define SP_FIRE          3
#define SP_KILL_WALL     4
#define SP_COLD          5
#define SP_KILL_TRAP     6
#define SP_ELEC          7
#define SP_PIERCE        8
#define SP_RUSH          9
#define SP_DOUBLE        10
#define SP_EXPLODE       11
#define SP_EVILNESS      12
#define SP_HOLYNESS      13
#define SP_FINAL         14
#define SP_NEEDLE        15

#define CONCENT_RADAR_THRESHOLD 2
#define CONCENT_TELE_THRESHOLD  5

/* Hex */
#define hex_spelling_any() \
    ((p_ptr->realm1 == REALM_HEX) && (p_ptr->magic_num1[0]))
#define hex_spelling(X) \
    ((p_ptr->realm1 == REALM_HEX) && (p_ptr->magic_num1[0] & (1L << (X))))
/* 1st book */
#define HEX_BLESS             0
#define HEX_CURE_LIGHT        1
#define HEX_DEMON_AURA        2
#define HEX_STINKING_MIST     3
#define HEX_XTRA_MIGHT        4
#define HEX_CURSE_WEAPON      5
#define HEX_DETECT_EVIL       6
#define HEX_PATIENCE          7
/* 2nd book */
#define HEX_ICE_ARMOR         8
#define HEX_CURE_SERIOUS      9
#define HEX_INHAIL           10
#define HEX_VAMP_MIST        11
#define HEX_RUNESWORD        12
#define HEX_CONFUSION        13
#define HEX_BUILDING         14
#define HEX_ANTI_TELE        15
/* 3rd book */
#define HEX_SHOCK_CLOAK      16
#define HEX_CURE_CRITICAL    17
#define HEX_RECHARGE         18
#define HEX_RAISE_DEAD       19
#define HEX_CURSE_ARMOUR     20
#define HEX_SHADOW_CLOAK     21
#define HEX_PAIN_TO_MANA     22
#define HEX_EYE_FOR_EYE      23
/* 4th book */
#define HEX_ANTI_MULTI       24
#define HEX_RESTORE          25
#define HEX_DRAIN_CURSE      26
#define HEX_VAMP_BLADE       27
#define HEX_STUN_MONSTERS    28
#define HEX_SHADOW_MOVE      29
#define HEX_ANTI_MAGIC       30
#define HEX_REVENGE          31

#define CUT_MORTAL_WOUND 1000
#define CUT_DEEP_GASH     200
#define CUT_SEVERE        100
#define CUT_NASTY          50
#define CUT_BAD            25
#define CUT_LIGHT          10
#define CUT_GRAZE           1
#define CUT_NONE            0

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

/* Weaponmaster shooting powers */
#define SHOOT_NONE          0
#define SHOOT_BOUNCE        1
#define SHOOT_PIERCE        2
#define SHOOT_RUN           3
#define SHOOT_MANY          4
#define SHOOT_ALL           5
#define SHOOT_VOLLEY        7
#define SHOOT_TRANQUILIZE   8
#define SHOOT_NEEDLE        9
#define SHOOT_DISINTEGRATE 10
#define SHOOT_RETALIATE    11
#define SHOOT_SHATTER      12
#define SHOOT_KNOCKBACK    13
#define SHOOT_ELEMENTAL    14
#define SHOOT_SNIPING      15  /* Scout */

/* Weaponmaster et. al. toggle modes */
#define TOGGLE_NONE                0

/* Slingmaster */
#define TOGGLE_SHOT_ON_THE_RUN    1
#define TOGGLE_RAPID_SHOT        2

/* Daggermaster */
#define TOGGLE_FLYING_DAGGER_STANCE    3
#define TOGGLE_SHADOW_STANCE        4
#define TOGGLE_FRENZY_STANCE        5

/* Clubmaster */
#define TOGGLE_COMBAT_EXPERTISE        6
#define TOGGLE_TRADE_BLOWS            8

/* Axemaster */
#define TOGGLE_POWER_ATTACK            9

/* Swordmaster */
#define TOGGLE_BURNING_BLADE        10
#define TOGGLE_ICE_BLADE            11
#define TOGGLE_THUNDER_BLADE        12
#define TOGGLE_BLOOD_BLADE            13
#define TOGGLE_HOLY_BLADE            14
#define TOGGLE_ORDER_BLADE            15
#define TOGGLE_WILD_BLADE            16

/* Scythemaster */
#define TOGGLE_MANY_STRIKE            17
#define TOGGLE_PIERCING_STRIKE        18
#define TOGGLE_TRIP                    19

/* Pickmaster */
#define TOGGLE_STRENGTH_OF_THE_UNDERTAKER    22
#define TOGGLE_STOICISM                        23
#define TOGGLE_INDUSTRIOUS_MORTICIAN        24

/* Shieldmaster */
#define TOGGLE_SHIELD_BASH      25
#define TOGGLE_BULWARK            26
#define TOGGLE_SHIELD_REVENGE    27

/* Mauler */
#define MAULER_TOGGLE_BLOCK     28
#define MAULER_TOGGLE_SHATTER   29
#define MAULER_TOGGLE_TUNNEL    30
#define MAULER_TOGGLE_DRAIN     31
#define MAULER_TOGGLE_MAUL      32
#define MAULER_TOGGLE_SPLATTER  33

/* Bowmaster */
#define TOGGLE_READIED_SHOT      34
#define TOGGLE_PIERCING_ARROW    35

/* Crossbowmaster */
#define TOGGLE_RAPID_RELOAD      36
#define TOGGLE_EXPLODING_BOLT    37
#define TOGGLE_OVERDRAW          38

/* Mystic */
#define MYSTIC_TOGGLE_STEALTH    39
#define MYSTIC_TOGGLE_FAST       40
#define MYSTIC_TOGGLE_RETALIATE  41
#define MYSTIC_TOGGLE_OFFENSE    42
#define MYSTIC_TOGGLE_DEFENSE    43

#define LEPRECHAUN_TOGGLE_BLINK    44

#define WARLOCK_DRAGON_TOGGLE_BLESS 45
#define WARLOCK_DRAGON_TOGGLE_CANTER 46
#define WARLOCK_DRAGON_TOGGLE_GALLOP 47
#define WARLOCK_DRAGON_TOGGLE_HEALING 48
#define WARLOCK_DRAGON_TOGGLE_HEROIC_CHARGE 49

#define LEPRECHAUN_TOGGLE_HOARDING 50

/* Wild Counters */
#define WILD_INFRAVISION 1
#define WILD_BLESS 2
#define WILD_BERSERK 3
#define WILD_SPEED 4
#define WILD_ESP 5
#define WILD_PROT_EVIL 6
#define WILD_MAGIC_RESIST 7
#define WILD_RESIST 8
#define WILD_STONE_SKIN 9
#define WILD_PASSWALL 10
#define WILD_REVENGE 11
#define WILD_INVULN 12
#define WILD_WRAITH 13
#define WILD_LIGHT_SPEED 14

#define LEAVING_UNKOWN 0
#define LEAVING_RECALL 1
#define LEAVING_REWIND_TIME 2
#define LEAVING_TELEPORT_LEVEL 3
#define LEAVING_ALTER_REALITY 4

#define MAX_SUMMONS 50

/* All of the following enumeration values are persisted in
   savefiles, so should not be changed. Add new options to
   the bottom of the respective enum (just before the _MAX
   entry, if present)
*/

enum slot_e {
    EQUIP_SLOT_NONE,
    EQUIP_SLOT_GLOVES,
    EQUIP_SLOT_WEAPON_SHIELD,
    EQUIP_SLOT_RING,
    EQUIP_SLOT_BOW,
    EQUIP_SLOT_AMULET,
    EQUIP_SLOT_LITE,
    EQUIP_SLOT_BODY_ARMOR,
    EQUIP_SLOT_CLOAK,
    EQUIP_SLOT_BOOTS,
    EQUIP_SLOT_HELMET,
    EQUIP_SLOT_ANY,
    EQUIP_SLOT_WEAPON,
    EQUIP_SLOT_CAPTURE_BALL,
    EQUIP_SLOT_MAX
};

/* Many ego types are now shared across mutliple kinds of equipment */
enum ego_e {
    EGO_TYPE_NONE         = 0,

    /* Melee Weapons */
    EGO_TYPE_WEAPON       = 0x00001,
    EGO_TYPE_DIGGER       = 0x00002,

    /* Armor */
    EGO_TYPE_SHIELD       = 0x00004,
    EGO_TYPE_BODY_ARMOR   = 0x00008,
    EGO_TYPE_ROBE         = 0x00010,
    EGO_TYPE_DRAGON_ARMOR = 0x00020,
    EGO_TYPE_CLOAK        = 0x00040,
    EGO_TYPE_HELMET       = 0x00080,
    EGO_TYPE_CROWN        = 0x00100,
    EGO_TYPE_GLOVES       = 0x00200,
    EGO_TYPE_BOOTS        = 0x00400,

    /* Missile Weapons */
    EGO_TYPE_BOW          = 0x00800,
    EGO_TYPE_AMMO         = 0x01000,
    EGO_TYPE_HARP         = 0x02000,

    /* Jewelry, Lights, Devices */
    EGO_TYPE_RING         = 0x04000,
    EGO_TYPE_AMULET       = 0x08000,
    EGO_TYPE_LITE         = 0x10000,
    EGO_TYPE_DEVICE       = 0x20000,

    /* (Blasted) */
    EGO_TYPE_SPECIAL      = 0x40000,
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
    EGO_WEAPON_WILD,
    EGO_WEAPON_ORDER,
    EGO_WEAPON_DEFENDER,
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
    EGO_BOOTS_FEANOR = 150,
    EGO_BOOTS_SPRITE,
    EGO_BOOTS_GOLEM,

    /* Missile Weapons */
    EGO_BOW_ACCURACY = 160,
    EGO_BOW_VELOCITY,
    EGO_BOW_EXTRA_MIGHT,
    EGO_BOW_EXTRA_SHOTS,
    EGO_BOW_LOTHLORIEN,
    EGO_BOW_HARADRIM = 165,
    EGO_BOW_BUCKLAND,

    EGO_AMMO_HURT_ANIMAL = 180,
    EGO_AMMO_HURT_DRAGON,
    EGO_AMMO_HURT_EVIL,
    EGO_AMMO_SLAYING,
    EGO_AMMO_WOUNDING,
    EGO_AMMO_SHOCKING = 185,
    EGO_AMMO_FLAME,
    EGO_AMMO_FROST,
    EGO_AMMO_PRISM,
    EGO_AMMO_RETURNING,
    EGO_AMMO_HOLY_MIGHT = 190,

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
    EGO_LITE_EXTRA_LIGHT = 235,
    EGO_LITE_ILLUMINATION,
    EGO_LITE_DURATION,
    EGO_LITE_INFRAVISION,
    EGO_LITE_IMMOLATION,
    EGO_LITE_DARKNESS = 240,
    EGO_LITE_IMMORTAL_EYE,
    EGO_LITE_VALINOR,
    EGO_LITE_SCRYING,

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
};


enum effect_e
{
    EFFECT_NONE = 0,

    /* Detection */
    EFFECT_LITE_AREA = 1,
    EFFECT_LITE_MAP_AREA,
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
    EFFECT_SPEED_ESSENTIA,
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
    EFFECT_BOLT_LITE,
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
    EFFECT_BEAM_LITE_WEAK = 350,
    EFFECT_BEAM_LITE,
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
    EFFECT_BALL_LITE,
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
    EFFECT_BREATHE_LITE,
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
    EFFECT_CONFUSING_LITE,
    EFFECT_ARROW,
    EFFECT_WRATH_OF_GOD,
    EFFECT_METEOR,
    EFFECT_HOLINESS,
    EFFECT_STARBURST,      /* Centered on player. cf EFFECT_BALL_LITE */
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

    EFFECT_FISHING,
    EFFECT_PIERCING_SHOT,
    EFFECT_CHARGE,
    EFFECT_WALL_BUILDING,
    EFFECT_POLYMORPH,
    EFFECT_STARLITE,
    EFFECT_NOTHING,      /* Food for undead players */

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
    EFFECT_MITO_KOUMON,
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
