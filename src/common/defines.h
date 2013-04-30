/*
 * File: defines.h
 * Purpose: Global constants and macro definitions
 */


#ifndef INCLUDED_DEFINES_H
#define INCLUDED_DEFINES_H


/*
 * Main screen normal size - hardcoded in many places
 */
#define NORMAL_WID  80
#define NORMAL_HGT  24


/*
 * Number of grids in each screen (vertically)
 * Status line: 1 row
 * Top line: 1 row
 */
#define ROW_MAP 1
#define SCREEN_HGT  (NORMAL_HGT - ROW_MAP - 1)


/*
 * Number of grids in each screen (horizontally)
 * Compact display: 13 columns
 * Right margin: 1 column
 */
#define COL_MAP 13
#define SCREEN_WID  (NORMAL_WID - COL_MAP - 1)


/*
 * Number of grids in each dungeon (vertically)
 * Must be a multiple of SCREEN_HGT
 * Must be less or equal to 256
 */
#define DUNGEON_HGT 66


/*
 * Number of grids in each dungeon (horizontally)
 * Must be a multiple of SCREEN_WID
 * Must be less or equal to 256
 */
#define DUNGEON_WID 198


/*
 * Maximum number of player "sex" types (see "table.c", etc)
 */
#define MAX_SEXES   3


/*
 * Number of tval/min-sval/max-sval slots per ego_item
 */
#define EGO_TVALS_MAX 4


/*
 * Maximum dungeon level.  The player can never reach this level
 * in the dungeon, and this value is used for various calculations
 * involving object and monster creation.  It must be at least 100.
 * Setting it below 128 may prevent the creation of some objects.
 */
#define MAX_DEPTH   128


/*
 * Maximum size of the "view" array (see "cave.c")
 * Note that the "view radius" will NEVER exceed 20, and even if the "view"
 * was octagonal, we would never require more than 1520 entries in the array.
 */
#define VIEW_MAX    1536


/*
 * Maximum size of the "temp" array (see "cave.c")
 * We must be as large as "VIEW_MAX" and "LIGHT_MAX" for proper functioning
 * of "update_view()" and "update_light()".  We must also be as large as the
 * largest illuminatable room, but no room is larger than 800 grids.  We
 * must also be large enough to allow "good enough" use as a circular queue,
 * to calculate monster flow, but note that the flow code is "paranoid".
 */
#define TEMP_MAX 1536


/*
 * Player constants
 */
#define PY_MAX_EXP      99999999L   /* Maximum exp */
#define PY_MAX_GOLD     999999999L  /* Maximum gold */
#define PY_MAX_LEVEL    50          /* Maximum level */


/*
 * Player "food" crucial values
 */
#define PY_FOOD_UPPER   20000   /* Upper limit on food counter */
#define PY_FOOD_MAX     15000   /* Food value (Bloated) */
#define PY_FOOD_FULL    10000   /* Food value (Normal) */
#define PY_FOOD_ALERT   2000    /* Food value (Hungry) */
#define PY_FOOD_WEAK    1000    /* Food value (Weak) */
#define PY_FOOD_FAINT   500     /* Food value (Fainting) */
#define PY_FOOD_STARVE  100     /* Food value (Starving) */


/*
 * Maximum number of players spells
 */
#define PY_MAX_SPELLS 64


/*
 * Number of spells per book
 */
#define SPELLS_PER_BOOK 9


/*
 * Maximum number of objects allowed in a single dungeon grid.
 *
 * The main-screen has a minimum size of 24 rows, so we can always
 * display 23 objects + 1 header line.
 *
 * No piles of objects at this time.
 */
#define MAX_FLOOR_STACK 1


/*
 * Timed effects
 */
enum
{
    TMD_FAST = 0, TMD_SLOW, TMD_BLIND, TMD_PARALYZED, TMD_CONFUSED,
    TMD_AFRAID, TMD_IMAGE, TMD_POISONED, TMD_CUT, TMD_STUN, TMD_PROTEVIL,
    TMD_INVULN, TMD_HERO, TMD_SHERO, TMD_SHIELD, TMD_BLESSED, TMD_SINVIS,
    TMD_SINFRA, TMD_OPP_ACID, TMD_OPP_ELEC, TMD_OPP_FIRE, TMD_OPP_COLD,
    TMD_OPP_POIS, TMD_OPP_CONF, TMD_AMNESIA, TMD_ESP, TMD_STONESKIN,
    TMD_TERROR, TMD_SPRINT, TMD_BOLD,
    TMD_WRAITH, TMD_MEDITATE, TMD_MANASHIELD,
    TMD_INVIS, TMD_MIMIC, TMD_TRAPS, TMD_BOWBRAND, TMD_ANCHOR,
    TMD_PROBTRAVEL, TMD_ADRENALINE, TMD_BIOFEEDBACK, TMD_TOUCH, TMD_SOUL,
    TMD_DEADLY, TMD_EPOWER, TMD_ICY_AURA, TMD_SGRASP, TMD_FARSIGHT, TMD_ZFARSIGHT,
    TMD_REGEN, TMD_HARMONY, TMD_ANTISUMMON,

    TMD_MAX
};


/*
 * Skill indexes
 */
enum
{
    SKILL_DISARM,           /* Skill: Disarming */
    SKILL_DEVICE,           /* Skill: Magic Devices */
    SKILL_SAVE,             /* Skill: Saving throw */
    SKILL_STEALTH,          /* Skill: Stealth factor */
    SKILL_SEARCH,           /* Skill: Searching ability */
    SKILL_SEARCH_FREQUENCY, /* Skill: Searching frequency */
    SKILL_TO_HIT_MELEE,     /* Skill: To hit (normal) */
    SKILL_TO_HIT_BOW,       /* Skill: To hit (shooting) */
    SKILL_TO_HIT_THROW,     /* Skill: To hit (throwing) */
    SKILL_DIGGING,          /* Skill: Digging */

    SKILL_MAX
};


/*
 * Indexes of the various "stats" (hard-coded by savefiles, etc).
 */
enum
{
    A_STR = 0,
    A_INT,
    A_WIS,
    A_DEX,
    A_CON,
    A_CHR,

    A_MAX
};


/*
 * Player sex constants (hard-coded by save-files, arrays, etc)
 */
#define SEX_FEMALE  0
#define SEX_MALE    1


/*
 * Special values for the number of turns to rest, these need to be
 * negative numbers, as postive numbers are taken to be a turncount,
 * and zero means "not resting".
 */
enum
{
    REST_COMPLETE = -2,
    REST_ALL_POINTS = -1,
    REST_SOME_POINTS = -3
};


/*** Feature Indexes (see "lib/edit/terrain.txt") ***/


/* Nothing */
#define FEAT_NONE       0x00

/* Various */
#define FEAT_FLOOR      0x01
#define FEAT_INVIS      0x02
#define FEAT_GLYPH      0x03
#define FEAT_OPEN       0x04
#define FEAT_BROKEN     0x05
#define FEAT_LESS       0x06
#define FEAT_MORE       0x07

/* Shops */
#define FEAT_SHOP_HEAD  0x08
#define FEAT_SHOP_TAIL  0x11

/* Traps */
#define FEAT_TRAP_HEAD  0x20
#define FEAT_TRAP_TAIL  0x2F

/* Doors */
#define FEAT_DOOR_HEAD  0x30
#define FEAT_DOOR_TAIL  0x3F

/* Secret door */
#define FEAT_SECRET     0x40

/* Rubble */
#define FEAT_RUBBLE     0x41

/* Mineral seams */
#define FEAT_MAGMA      0x42
#define FEAT_QUARTZ     0x43
#define FEAT_MAGMA_H    0x44
#define FEAT_QUARTZ_H   0x45
#define FEAT_MAGMA_K    0x46
#define FEAT_QUARTZ_K   0x47

/* Walls */
#define FEAT_WALL_EXTRA 0x48
#define FEAT_WALL_INNER 0x49
#define FEAT_WALL_OUTER 0x4A
#define FEAT_WALL_SOLID 0x4B
#define FEAT_PERM_EXTRA 0x4C
#define FEAT_PERM_BASIC 0x4D
#define FEAT_PERM_FAKE  0x4E
#define FEAT_PERM_ARENA 0x4F
#define FEAT_PERM_SOLID 0x50

/* MAngband-specific terrain elements */
#define FEAT_WATER      0x51
#define FEAT_MUD        0x52
#define FEAT_DRAWBRIDGE 0x53
#define FEAT_FOUNTAIN   0x54
#define FEAT_FNT_DRIED  0x55
#define FEAT_LOOSE_DIRT 0x56
#define FEAT_DIRT       0x57
#define FEAT_FLOOR_SAFE 0x58
#define FEAT_LAVA       0x59
#define FEAT_STREET     0x5A

/* Vegetation */
#define FEAT_GRASS      0x60
#define FEAT_CROP       0x61

/* Trees */
#define FEAT_TREE       0x62
#define FEAT_EVIL_TREE  0x63

/* Mountains */
#define FEAT_MOUNTAIN   0x65

/* Other wilderness features */
#define FEAT_LOGS       0x68
#define FEAT_SWAMP      0x6A
#define FEAT_TOWN       0x6B
#define FEAT_PERM_CLEAR 0x70

/* Trap border & overlay */
#define FEAT_FLOOR_TRAP 0x71
#define FEAT_OVER_TRAP  0x72

/* Special "home doors" */
#define FEAT_HOME_OPEN  0x74
#define FEAT_HOME_HEAD  0x75
#define FEAT_HOME_TAIL  0x7F


/*** Monster blow constants ***/


#define MONSTER_BLOW_MAX    4


/*
 * Bit flags for the "target_set_interactive" function
 *
 *    KILL:  Target monsters
 *    LOOK:  Describe grid fully
 *    HELP:  Target friendly players
 *    QUIET: Don't display "targeted" message
 *    AIM:   Get an "aiming direction"
 */
#define TARGET_KILL     0x01
#define TARGET_LOOK     0x02
#define TARGET_HELP     0x04
#define TARGET_QUIET    0x08
#define TARGET_AIM      0x10


/*
 * Bit flags for the "p_ptr->redraw" variable
 */
#define PR_MISC             0x00000001L /* Display Race/Class */
#define PR_TITLE            0x00000002L /* Display Title */
#define PR_LEV              0x00000004L /* Display Level */
#define PR_EXP              0x00000008L /* Display Experience */
#define PR_STATS            0x00000010L /* Display Stats */
#define PR_ARMOR            0x00000020L /* Display Armor */
#define PR_HP               0x00000040L /* Display Hitpoints */
#define PR_MANA             0x00000080L /* Display Mana */
#define PR_GOLD             0x00000100L /* Display Gold */
#define PR_OTHER            0x00000200L /* Display other info */
#define PR_ITEMLIST         0x00000400L /* Display item list */
#define PR_HEALTH           0x00000800L /* Display Health Bar */
#define PR_SPEED            0x00001000L /* Display Extra (Speed) */
#define PR_STUDY            0x00002000L /* Display Extra (Study) */
#define PR_DEPTH            0x00004000L /* Display Depth */
#define PR_STATUS           0x00008000L
#define PR_DTRAP            0x00010000L /* Display Extra (Trap detection) */
#define PR_STATE            0x00020000L /* Display Extra (State) */
#define PR_MAP              0x00040000L /* Redraw whole map */
#define PR_INVEN            0x00080000L /* Display inven/equip */
#define PR_EQUIP            0x00100000L /* Display equip/inven */
#define PR_MESSAGE          0x00200000L /* Display messages */
#define PR_MONSTER          0x00400000L /* Display monster recall */
#define PR_OBJECT           0x00800000L /* Display object recall */
#define PR_MONLIST          0x01000000L /* Display monster list */
#define PR_MESSAGE_CHAT     0x02000000L /* Display chat messages */
#define PR_SPELL            0x04000000L /* Display spell list */
#define PR_SPECIAL_INFO     0x08000000L /* Display special info */
#define PR_LAG              0x10000000L /* Display Lag Bar */
#define PR_PLUSSES          0x20000000L /* Display Plusses to Hit/Damage */
#define PR_CURSOR           0x40000000L /* Display Cursor */
#define PR_FLOOR            0x80000000L /* Display floor object */

/* Display Basic Info */
#define PR_BASIC \
    (PR_MISC | PR_TITLE | PR_LEV | PR_EXP | PR_STATS | PR_ARMOR | PR_HP | \
     PR_MANA | PR_GOLD | PR_DEPTH | PR_EQUIP | PR_HEALTH | PR_SPEED)

/* Display Extra Info */
#define PR_EXTRA \
    (PR_STATUS | PR_STATE | PR_STUDY)


/*
 * Bit flags for the "window_flag" variable
 */
#define PW_INVEN        0x00000001L /* Display inven/equip */
#define PW_EQUIP        0x00000002L /* Display equip/inven */
#define PW_PLAYER_0     0x00000004L /* Display player (basic) */
#define PW_PLAYER_1     0x00000008L /* Display player (extra) */
#define PW_PLAYER_2     0x00000010L /* Display player (compact) */
#define PW_MAP          0x00000020L /* Display dungeon map */
#define PW_MESSAGE      0x00000040L /* Display messages */
/* xxx Display overhead view xxx */
#define PW_MONSTER      0x00000100L /* Display monster recall */
#define PW_OBJECT       0x00000200L /* Display object recall */
#define PW_MONLIST      0x00000400L /* Display monster list */
#define PW_STATUS       0x00000800L /* Display status */
#define PW_MESSAGE_CHAT 0x00001000L /* Display chat messages */
#define PW_SPELL        0x00002000L /* Display spell list */
#define PW_ITEMLIST     0x00004000L /* Display item list */
#define PW_SPECIAL_INFO 0x00008000L /* Display special info */


#define PW_MAX_FLAGS    16


/*
 * Player race and class flags
 */
enum
{
    #define PF(a, b) PF_##a,
    #include "list-player-flags.h"
    #undef PF
    PF__MAX
};

#define PF_SIZE                FLAG_SIZE(PF__MAX)

#define pf_has(f, flag)        flag_has_dbg(f, PF_SIZE, flag, #f, #flag)
#define pf_next(f, flag)       flag_next(f, PF_SIZE, flag)
#define pf_is_empty(f)         flag_is_empty(f, PF_SIZE)
#define pf_is_full(f)          flag_is_full(f, PF_SIZE)
#define pf_is_inter(f1, f2)    flag_is_inter(f1, f2, PF_SIZE)
#define pf_is_subset(f1, f2)   flag_is_subset(f1, f2, PF_SIZE)
#define pf_is_equal(f1, f2)    flag_is_equal(f1, f2, PF_SIZE)
#define pf_on(f, flag)         flag_on_dbg(f, PF_SIZE, flag, #f, #flag)
#define pf_off(f, flag)        flag_off(f, PF_SIZE, flag)
#define pf_wipe(f)             flag_wipe(f, PF_SIZE)
#define pf_setall(f)           flag_setall(f, PF_SIZE)
#define pf_negate(f)           flag_negate(f, PF_SIZE)
#define pf_copy(f1, f2)        flag_copy(f1, f2, PF_SIZE)
#define pf_union(f1, f2)       flag_union(f1, f2, PF_SIZE)
#define pf_comp_union(f1, f2)  flag_comp_union(f1, f2, PF_SIZE)
#define pf_inter(f1, f2)       flag_inter(f1, f2, PF_SIZE)
#define pf_diff(f1, f2)        flag_diff(f1, f2, PF_SIZE)

#define player_has(P, flag) \
    (pf_has((P)->race->pflags, (flag)) || pf_has((P)->clazz->pflags, (flag)))


/*** Monster flags ***/


/*
 * Monster property and ability flags (race flags)
 */
enum
{
    #define RF(a, b) RF_##a,
    #include "list-mon-flags.h"
    #undef RF
    RF_MAX
};

#define RF_SIZE FLAG_SIZE(RF_MAX)


/*
 * Monster spell flags
 */
enum
{
    #define RSF(a, b, c, d, e, f, g, h, i, j, k, l, m, n) RSF_##a,
    #include "list-mon-spells.h"
    #undef RSF
    RSF_MAX
};

/*
 * Spell type bitflags
 */
enum mon_spell_type
{
    RST_BOLT = 0x001,
    RST_BALL = 0x002,
    RST_BREATH = 0x004,
    RST_ATTACK = 0x008, /* Direct (non-projectable) attacks */
    RST_ANNOY = 0x010,  /* Irritant spells, usually non-fatal */
    RST_HASTE = 0x020,  /* Relative speed advantage */
    RST_HEAL = 0x040,
    RST_TACTIC = 0x080, /* Get a better position */
    RST_ESCAPE = 0x100,
    RST_SUMMON = 0x200,
    RST_MISSILE = 0x400,
    RST_SPECIAL = 0x800
};

typedef enum
{
    RSV_SKILL = 0x1,
    RSV_UNDEAD = 0x2
} mon_spell_save;

#define RSF_SIZE    FLAG_SIZE(RSF_MAX)


/*** Hack ***/


/*
 * Default graphic mode
 */
#define GRAPHICS_NONE   0


/*** PWMAngband ***/


/*
 * Maximum message length
 */
#define MSG_LEN 1024


/*
 * Maximum number of messages to keep in player message history
 */
#define MAX_MSG_HIST 60


/*
 * Maximum number of players playing at once.
 *
 * The number of connections is limited by the number of bases
 * and the max number of possible file descriptors to use in
 * the select(2) call minus those for stdin, stdout, stderr,
 * the contact socket, and the socket for the resolver library routines.
 *
 * This limit has never been stretched, and it would be interesting to see
 * what happens when 100 or so players play at once.
 */
#define MAX_PLAYERS 1018


/*
 * Maximum channel name length
 */
#define MAX_CHAN_LEN 12


/*
 * Default chat channel
 */
#define DEFAULT_CHANNEL "#public"
#define MAX_CHANNELS 255


/*
 * Maximum length for names and passwords
 *
 */
#define MAX_NAME_LEN    20
#define MAX_PASS_LEN    40


/* The number of wilderness levels we have allocated.
*/
#define MAX_WILD    2048


/*
 * RLE encoding modes
 */
#define RLE_NONE 0
#define RLE_CLASSIC 1
#define RLE_LARGE 2
#define DUNGEON_RLE_MODE(P) ((P)->use_graphics? RLE_LARGE: RLE_CLASSIC)


/*
 * Party commands
 */
#define PARTY_CREATE    1
#define PARTY_ADD       2
#define PARTY_DELETE    3
#define PARTY_REMOVE_ME 4
#define PARTY_HOSTILE   5
#define PARTY_PEACE     6


/*
 * Dungeon master commands
 */
#define MASTER_NULL     0
#define MASTER_LEVEL    1
#define MASTER_BUILD    2
#define MASTER_SUMMON   3
#define MASTER_GENERATE 4
#define MASTER_PLAYER   5
#define MASTER_VISUALS  6
#define MASTER_ORDER    7


/*
 * The types of special file perusal.
 */
#define SPECIAL_FILE_NONE       0
#define SPECIAL_FILE_OBJECT     1
#define SPECIAL_FILE_ARTIFACT   2
#define SPECIAL_FILE_EGO        3
#define SPECIAL_FILE_KILL       4
#define SPECIAL_FILE_FEATURE    5
#define SPECIAL_FILE_SCORES     7
#define SPECIAL_FILE_UNIQUE     8
#define SPECIAL_FILE_GEAR       9
#define SPECIAL_FILE_HISTORY    10
#define SPECIAL_FILE_HOUSES     11
#define SPECIAL_FILE_PLAYER     12
#define SPECIAL_FILE_OTHER      13
#define SPECIAL_FILE_POLY       14
#define SPECIAL_FILE_SOCIALS    15
#define SPECIAL_FILE_HELP       16


/*
 * Define for object shimmering code
 */
#define TERM_MULTI  28


/*
 * Special coloring
 */
#define TERM_SPECIAL    29
#define TERM_SYMBOL     30


/*
 * Maximum number of lines in 'special info' (*ID*, Self-Knowledge, Recalls)
 */
#define MAX_TXT_INFO    384


/* Is string empty? Beats calling strlen */
#define STRZERO(S) \
    ((S)[0] == '\0')


/* Constants for character history */
#define N_HIST_LINES    3
#define N_HIST_WRAP     73


/* Constants for item hooks */
#define HOOK_UNDEFINED  0
#define HOOK_AMMO       1
#define HOOK_RECHARGE   2
#define HOOK_WEAPON     3
#define HOOK_ARMOR      4

#define N_HOOKS         5

/* Special hook */
#define HOOK_CARRY      255


/* Character rolling methods */
typedef enum birth_rollers
{
    BR_QDYNA = -2,
    BR_QUICK = -1,
    BR_POINTBASED = 0,
    BR_NORMAL,
    MAX_BIRTH_ROLLERS
} birth_rollers;


/*
 * Maximum number of characters per account
 */
#define MAX_ACCOUNT_CHARS 12


/*
 * Steps for the "target_set_interactive" function
 *
 *    NONE:  Initial lookup
 *    MON:   Describe monster (or player)
 *    OBJ:   Describe object
 *    FEAT:  Describe feature
 */
#define TARGET_NONE 0
#define TARGET_MON  1
#define TARGET_OBJ  2
#define TARGET_FEAT 3


/*
 * Melee
 */
#define melee_p(T) \
    (((T)->tval == TV_HAFTED) || ((T)->tval == TV_POLEARM) || \
        ((T)->tval == TV_SWORD))


/*
 * Body armor
 */
#define body_armor_p(T) \
    (((T)->tval == TV_SOFT_ARMOR) || ((T)->tval == TV_HARD_ARMOR) || \
        ((T)->tval == TV_DRAG_ARMOR))


/*
 * Armor
 */
#define armor_p(T) \
    (((T)->tval == TV_BOOTS) || ((T)->tval == TV_GLOVES) || \
        ((T)->tval == TV_CLOAK) || ((T)->tval == TV_CROWN) || \
        ((T)->tval == TV_HELM) || ((T)->tval == TV_SHIELD) || body_armor_p(T))


/*
 * Weapon
 */
#define weapon_p(T) \
    (((T)->tval == TV_BOW) || melee_p(T))


/*
 * Wieldable
 */
#define wieldable_p(T) \
    (weapon_p(T) || obj_is_ammo(NULL, T))


/* Undead Form */
#define player_can_undead(P) \
    (player_has((P), PF_UNDEAD_POWERS) && ((P)->state.stat_use[A_INT] >= 18+70))


/* Hack -- Ensure a variable fits into ddx/ddy array bounds */
#define VALID_DIR(D) (((D) >= 0) && ((D) < 10))


/*
 * Buffers for ".txt" text files
 */
#define MAX_TEXTFILES   3
#define TEXTFILE__WID   (NORMAL_WID + 20)
#define TEXTFILE__HGT   (NORMAL_HGT - 1)
#define TEXTFILE_MOTD   0
#define TEXTFILE_TOMB   1
#define TEXTFILE_CRWN   2


/*** These don't belong here ***/


/*
 * Maximum size of the "light" array (see "cave.c")
 * Note that the "light radius" will NEVER exceed 5, and even if the "light"
 * was rectangular, we would never require more than 128 entries in the array.
 */
#define LIGHT_MAX 128


/*  
 * List of resistances and abilities to display
 */
#define RES_PANELS  4
#define RES_ROWS    9
#define RES_COLS    14


/*
 * Modes for item lists in "show_inven()", "show_equip()" and "show_floor()"
 */
typedef enum
{
    OLIST_NONE   = 0x00,    /* No options */
    OLIST_WINDOW = 0x01,    /* Display list in a sub-term (left-align) */
    OLIST_QUIVER = 0x02,    /* Display quiver lines */
    OLIST_GOLD   = 0x04,    /* Include gold in the list */
    OLIST_WEIGHT = 0x08,    /* Show item weight */
    OLIST_PRICE  = 0x10,    /* Show item price */
    OLIST_FAIL   = 0x20,    /* Show device failure */
    OLIST_FLOOR  = 0x40     /* Include first floor item in the list */
} olist_detail_t;


/*
 * List of kinds of item, for pseudo-id squelch.
 */
typedef enum
{
    TYPE_JEWELRY,
    TYPE_DRAG_ARMOR,
    TYPE_WEARABLE,
    TYPE_BOOK,
    TYPE_CONSUMABLE,

    TYPE_MAX
} squelch_type_t;


/*
 * The different kinds of quality squelch
 */
enum
{
    SQUELCH_NONE,
    SQUELCH_WORTHLESS,
    SQUELCH_AVERAGE,
    SQUELCH_GOOD,
    SQUELCH_EXCELLENT_NO_HI,
    SQUELCH_EXCELLENT_NO_SPL,
    SQUELCH_ALL,

    SQUELCH_MAX
};


#endif /* INCLUDED_DEFINES_H */
