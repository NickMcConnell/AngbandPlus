/*
 * File: s-defines.h
 * Purpose: Server constants and macro definitions
 */


#ifndef INCLUDED_S_DEFINES_H
#define INCLUDED_S_DEFINES_H

/*
 * Number of grids in each block (vertically)
 * Probably hard-coded to 11, see "generate.c"
 */
#define BLOCK_HGT   11


/*
 * Number of grids in each block (horizontally)
 * Probably hard-coded to 11, see "generate.c"
 */
#define BLOCK_WID   11


/* History message types */
#define HISTORY_PLAYER_BIRTH        0x0001  /* Player was born */
#define HISTORY_ARTIFACT_UNKNOWN    0x0002  /* Player has generated an artifact */
#define HISTORY_ARTIFACT_KNOWN      0x0004  /* Player has found an artifact */
#define HISTORY_ARTIFACT_LOST       0x0008  /* Player had an artifact and lost it */
#define HISTORY_PLAYER_DEATH        0x0010  /* Player has been slain */
#define HISTORY_SLAY_UNIQUE         0x0020  /* Player has slain a unique monster */
#define HISTORY_HELP_UNIQUE         0x0040  /* Player helped to slay a unique monster */
#define HISTORY_PLAYER_REVIVE       0x0080  /* Player has been revived */
#define HISTORY_GAIN_LEVEL          0x0100  /* Player gained a level */


/*
 * Misc constants
 */
#define TOWN_DAWN       10000   /* Number of turns from dawn to dawn */
#define TOWN_DUSK       5000    /* Number of turns from dawn to dusk */
#define BREAK_GLYPH     550     /* Rune of protection resistance */
#define BTH_PLUS_ADJ    3       /* Adjust BTH per plus-to-hit */
#define MON_MULT_ADJ    8       /* High value slows multiplication */
#define MON_DRAIN_LIFE  2       /* Percent of player exp drained per hit */


/*
 * There is a 1/25 (4%) chance of inflating the requested monster level
 * during the creation of a monster (see "get_mon_num()" in "monster.c").
 * Lower values yield harder monsters more often.
 */
#define NASTY_MON   25  /* 1/chance of inflated monster level */
#define MON_OOD_MAX 10  /* Maximum out-of-depth amount */


/*
 * Refueling constants
 */
#define FUEL_TORCH      5000            /* Maximum amount of fuel in a torch */
#define FUEL_LAMP       15000           /* Maximum amount of fuel in a lamp */
#define DEFAULT_TORCH   FUEL_TORCH      /* Default amount of fuel in a torch */
#define DEFAULT_LAMP    (FUEL_LAMP / 2) /* Default amount of fuel in a lamp */


/*
 * More maximum values
 */
#define MAX_SIGHT_LGE   20  /* Maximum view distance */
#define MAX_RANGE_LGE   20  /* Maximum projection range */
#define MAX_SIGHT_SML   10  /* Maximum view distance (small devices) */
#define MAX_RANGE_SML   10  /* Maximum projection range (small devices) */
#define MAX_SIGHT (cfg_small_range? MAX_SIGHT_SML: MAX_SIGHT_LGE)
#define MAX_RANGE (cfg_small_range? MAX_RANGE_SML: MAX_RANGE_LGE)


/*
 * There is a 1/160 chance per round of creating a new monster
 */
#define MAX_M_ALLOC_CHANCE  160


/*
 * Normal levels get at least 14 monsters
 */
#define MIN_M_ALLOC_LEVEL   14


/*
 * The town starts out with 4 residents during the day
 */
#define MIN_M_ALLOC_TD      4


/*
 * The town starts out with 8 residents during the night
 */
#define MIN_M_ALLOC_TN      8


/*
 * A monster can only "multiply" (reproduce) if there are fewer than 100
 * monsters on the level capable of such spontaneous reproduction.  This
 * is a hack which prevents the "mon_list[]" array from exploding due to
 * reproducing monsters.  Messy, but necessary.
 */
#define MAX_REPRO  100


/*
 * Player regeneration constants
 */
#define PY_REGEN_NORMAL     197     /* Regen factor*2^16 when full */
#define PY_REGEN_WEAK       98      /* Regen factor*2^16 when weak */
#define PY_REGEN_FAINT      33      /* Regen factor*2^16 when fainting */
#define PY_REGEN_HPBASE     1442    /* Min amount hp regen*2^16 */
#define PY_REGEN_MNBASE     524     /* Min amount mana regen*2^16 */


/*
 * Flags for player_type.spell_flags[]
 */
#define PY_SPELL_LEARNED    0x01 /* Spell has been learned */
#define PY_SPELL_WORKED     0x02 /* Spell has been successfully tried */
#define PY_SPELL_FORGOTTEN  0x04 /* Spell has been forgotten */


/*
 * Maximum number of "normal" pack slots, and the index of the "overflow"
 * slot, which can hold an item, but only temporarily, since it causes the
 * pack to "overflow", dropping the "last" item onto the ground.  Since this
 * value is used as an actual slot, it must be less than "INVEN_WIELD" (below).
 * Note that "INVEN_PACK" is probably hard-coded by its use in savefiles, and
 * by the fact that the screen can only show 23 items plus a one-line prompt.
 */
#define INVEN_PACK  23


/*
 * Like the previous but takes into account the (variably full quiver).
 */
#define INVEN_MAX_PACK(P)  (INVEN_PACK - (P)->quiver_slots)


/*
 * Indexes used for various "equipment" slots (hard-coded by savefiles, etc).
 */
#define INVEN_WIELD     24
#define INVEN_BOW       25
#define INVEN_LEFT      26
#define INVEN_RIGHT     27
#define INVEN_NECK      28
#define INVEN_LIGHT     29
#define INVEN_BODY      30
#define INVEN_OUTER     31
#define INVEN_ARM       32
#define INVEN_HEAD      33
#define INVEN_HANDS     34
#define INVEN_FEET      35
#define INVEN_TOOL      36


/*
 * Total number of inventory slots (hard-coded).
 */
#define INVEN_TOTAL     37


/*
 * Quiver
 */
#define QUIVER_START    38
#define QUIVER_SIZE     5
#define QUIVER_END      43

#define ALL_INVEN_TOTAL 43


/*
 * A "stack" of items is limited to 40 items (hard-coded).
 */
#define MAX_STACK_SIZE  41


/*
 * The range of possible indexes into tables based upon stats.
 * Currently things range from 3 to 18/220 = 40.
 */
#define STAT_RANGE  38


/*** Important artifact indexes (see "lib/edit/artifact.txt") ***/


#define ART_POWER       13
#define ART_MORGOTH     34
#define ART_GROND       111
#define ART_MAX_STATIC  139
#define ART_SILMARIL    208


/*** Ego-Item indexes (see "lib/edit/ego_item.txt") ***/


#define EGO_CLOAK_LORDLY_RES    136
#define EGO_ELEMENTAL           143


/*** Player flags ***/


/*
 * Bit flags for the "p_ptr->notice" variable
 */
#define PN_COMBINE      0x00000001L /* Combine the pack */
#define PN_REORDER      0x00000002L /* Reorder the pack */
#define PN_SQUELCH      0x00000010L /* Squelch stuff */
#define PN_SORT_QUIVER  0x00000020L /* Sort the quiver */
#define PN_MON_MESSAGE  0x00000040L /* Flush monster pain messages */
#define PN_WAIT         0x00000080L /* Wait (item request is pending) */


/*
 * Bit flags for the "p_ptr->update" variable
 */
#define PU_BONUS        0x00000001L /* Calculate bonuses */
#define PU_TORCH        0x00000002L /* Calculate torch radius */
#define PU_HP           0x00000010L /* Calculate chp and mhp */
#define PU_MANA         0x00000020L /* Calculate csp and msp */
#define PU_SPELLS       0x00000040L /* Calculate spells */
#define PU_FORGET_VIEW  0x00010000L /* Forget field of view */
#define PU_UPDATE_VIEW  0x00020000L /* Update field of view */
#define PU_FORGET_FLOW  0x00100000L /* Forget flow data */
#define PU_UPDATE_FLOW  0x00200000L /* Update flow data */
#define PU_MONSTERS     0x01000000L /* Update monsters */
#define PU_DISTANCE     0x02000000L /* Update distances */


/*** Cave flags ***/


/*
 * Special cave grid flags (global)
 */
#define CAVE_GLOW   0x01    /* Self-illuminating */
#define CAVE_ICKY   0x02    /* Part of a vault */
#define CAVE_ROOM   0x04    /* Part of a room */
#define CAVE_TEMP   0x08    /* Temp flag */
#define CAVE_WALL   0x10    /* Wall flag */
#define CAVE_NOTELE 0x40    /* Part of a pit */


/*
 * Special cave grid flags (player)
 */
#define CAVE_MARK   0x01    /* Memorized feature */
#define CAVE_SEEN   0x02    /* Seen flag */
#define CAVE_VIEW   0x04    /* View flag */
#define CAVE_DTRAP  0x08    /* Trap detected grid */
#define CAVE_DEDGE  0x10    /* Border of trap detected area */


/*
 * Special cave grid flags (global + player)
 */
#define CAVE_FEEL   0x20    /* Hidden points to trigger feelings */


/*
 * Information for Feelings
 */
#define FEELING_TOTAL   100 /* Total number of feeling squares per level */
#define FEELING1        10  /* Squares needed to see in order to trigger first feeling */


/*
 * Chest trap flags (see "object/chest.c")
 */
#define CHEST_LOSE_STR      0x01
#define CHEST_LOSE_CON      0x02
#define CHEST_POISON        0x04
#define CHEST_PARALYZE      0x08
#define CHEST_EXPLODE       0x10
#define CHEST_SUMMON        0x20


/*** Terrain flags ***/


enum
{
    FF_PWALK        = 0x00000001,
    FF_PPASS        = 0x00000002,
    FF_MWALK        = 0x00000004,
    FF_MPASS        = 0x00000008,
    FF_LOOK         = 0x00000010,
    FF_DIG          = 0x00000020,
    FF_DOOR         = 0x00000040,
    FF_EXIT_UP      = 0x00000080,
    FF_EXIT_DOWN    = 0x00000100,
    FF_PERM         = 0x00000200,
    FF_TRAP         = 0x00000400,
    FF_SHOP         = 0x00000800,
    FF_HIDDEN       = 0x00001000,
    FF_BORING       = 0x00002000
};


/*
 * Convert a "location" (Y,X) into a "grid" (G)
 */
#define GRID(Y, X) \
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
 * Determines if a map location is "meaningful"
 */
#define in_bounds(Y, X) \
    (((unsigned)(Y) < (unsigned)(DUNGEON_HGT)) && \
     ((unsigned)(X) < (unsigned)(DUNGEON_WID)))


/*
 * Determines if a map location is fully inside the outer walls
 * This is more than twice as expensive as "in_bounds()", but
 * often we need to exclude the outer walls from calculations.
 */
#define in_bounds_fully(Y, X) \
    (((Y) > 0) && ((Y) < DUNGEON_HGT - 1) && ((X) > 0) && ((X) < DUNGEON_WID - 1))


/*
 * Determines if a map location is currently "on screen"
 * Note that "panel_contains(Y,X)" always implies "in_bounds(Y,X)".
 * Pre-storing this into a cave->info flag would be nice.  XXX XXX
 */
#define panel_contains(P, Y, X) \
    (((unsigned)((Y) - (P)->offset_y) < (unsigned)((P)->screen_rows / (P)->tile_hgt)) && \
        ((unsigned)((X) - (P)->offset_x) < (unsigned)((P)->screen_cols / (P)->tile_wid)))


/*
 * Determine if a "legal" grid is a "floor" grid
 *
 * Line 1 -- forbid doors, rubble, seams, walls, trees and other wilderness features
 *
 * Note the use of the new "CAVE_WALL" flag.
 */
#define cave_floor_bold(Z, Y, X) \
    (!(cave_get(Z)->info[Y][X] & (CAVE_WALL)))


/*
 * Determine if a "legal" grid is a "clean" floor grid
 *
 * Line 1 & 2 -- forbid non-floors
 * Line 3 -- forbid normal objects
 */
#define cave_clean_bold(DEPTH, Y, X) \
    ((cave_isfloor(cave_get(DEPTH), Y, X) || \
        cave_isotherfloor(cave_get(DEPTH), Y, X)) && \
        (cave_get(DEPTH)->o_idx[Y][X] == 0))


/*
 * Determine if a "legal" grid is an "empty" floor grid
 *
 * Line 1 -- forbid doors, rubble, seams, walls, trees and other wilderness features
 * Line 2 -- forbid player/monsters
 */
#define cave_empty_bold(DEPTH, Y, X) \
    (cave_floor_bold(DEPTH, Y, X) && \
        (cave_get(DEPTH)->m_idx[Y][X] == 0))


/*
 * Determine if a "legal" grid is an "naked" floor grid
 *
 * Line 1 -- forbid non-floors and normal objects
 * Line 2 -- forbid player/monsters
 */
#define cave_naked_bold(DEPTH, Y, X) \
    (cave_clean_bold(DEPTH, Y, X) && \
        (cave_get(DEPTH)->m_idx[Y][X] == 0))


/*
 * Determine if a "legal" grid is "permanent"
 *
 * Line 1 -- perma-walls
 * Line 2 -- stairs
 * Line 3 -- shop doors
 * Line 4 -- house doors
 */
#define cave_perma_bold(DEPTH, Y, X) \
    (cave_isperm(cave_get(DEPTH), Y, X) || \
        cave_isstairs(cave_get(DEPTH), Y, X) || \
        cave_isshop(cave_get(DEPTH), Y, X) || \
        cave_ishomedoor(cave_get(DEPTH), Y, X))


/*
 * Determine if a "legal" grid is within "los" of the player
 *
 * Note the use of comparison to zero to force a "boolean" result
 */
#define player_has_los_bold(P, Y, X) \
    (((P)->cave->info[Y][X] & CAVE_VIEW) != 0)


/*
 * Determine if a "legal" grid can be "seen" by the player
 *
 * Note the use of comparison to zero to force a "boolean" result
 */
#define player_can_see_bold(P, Y, X) \
    (((P)->cave->info[Y][X] & CAVE_SEEN) != 0)


/*
 * Say whether it's daytime or not
 */
#define is_daytime() \
    ((turn.turn % (10L * TOWN_DAWN)) < (10L * TOWN_DUSK))


#define MAX_ITEMLIST 2560


/*
 * Maximum number of rvals (monster templates) that a pit can specify.
 */
#define MAX_RVALS 5


/*** PWMAngband ***/


#define SERVER_SAVE     1       /* Minutes between server saves */
#define SERVER_PURGE    24      /* Hours between server purges */
#define GROW_TREE       5000    /* How often to grow a new tree in town */


/*
 * This is very important...
 *
 * This is the number of "frames" to produce per second.  It determines
 * the speed of the game.
 */
#define FPS 12


/*
 * The types of communication that we send to the metaserver
 */
#define META_START  0x01
#define META_DIE    0x02
#define META_UPDATE 0x04


/*
 * Hack -- This is used to make sure that every player that has a structure
 * dedicated to them is actually connected
 */
#define NOT_CONNECTED   (-1)


/*
 * Maximum number of player "race" types (see "table.c", etc)
 */
#define MAX_RACES   15


/*
 * Maximum number of player "class" types (see "table.c", etc)
 */
#define MAX_CLASS   15


/*
 * Maximum number of parties to allow.  If, while trying to create a new
 * party, you get a "No empty party slot" or somesuch message, increase
 * this number.  However, you should NEVER decrease this number after a
 * server has been run, or all sorts of bad things could happen.
 */
#define MAX_PARTIES 256


/*
 * Maximum number of houses available.
 */
#define MAX_HOUSES  1024


/*
 * Number of entries in the player name hash table.
 * This must be a power of 2!
 */
#define NUM_HASH_ENTRIES    1024


/*
 * Maximum number of special pre-designed static levels.
 */
#define MAX_SPECIAL_LEVELS 10


/*
 * Methods of leaving a level
 */
#define LEVEL_UP            0
#define LEVEL_DOWN          1
#define LEVEL_RAND          2
#define LEVEL_GHOST         3
#define LEVEL_OUTSIDE       4
#define LEVEL_OUTSIDE_RAND  5


/*
 * Maximum number of picked up/stolen objects a monster can carry
 */
#define MAX_MONSTER_BAG 25


/*
 * Number of spells for ghosts
 */
#define GHOST_SPELLS    8


/*
 * Number of spells for shapechangers
 */
#define MIMIC_SPELLS    65


/*
 * Special return codes corresponding to item request.
 */
#define ITEM_REQUEST    102
#define ITEM_PENDING    103


/*
 * Squelch flags
 */
#define SQUELCH_ALLOW       1
#define SQUELCH_PROTECT     2


/*
 * Total number of arenas
 */
#define MAX_ARENAS  10


/*
 * Different types of terrain, used for the wilderness.
 */
#define WILD_UNDEFINED      0
#define WILD_SHORE          1
#define WILD_GRASS          2
#define WILD_WOOD           3
#define WILD_SWAMP          4
#define WILD_WASTE          5
#define WILD_MOUNTAIN       6
#define WILD_VOLCANO        7
#define WILD_CLONE          8
#define WILD_TOWN           9
#define WILD_MUDPIT         10
#define WILD_SCORCHED       11


/* Different buildings */
#define WILD_LOG_CABIN      0
#define WILD_TOWN_HOME      1
#define WILD_ARENA          2


/* Types of crops */
#define WILD_CROP_POTATO    0
#define WILD_CROP_CABBAGE   1
#define WILD_CROP_CARROT    2
#define WILD_CROP_BEET      3
#define WILD_CROP_MUSHROOM  4
#define WILD_CROP_SQUASH    5
#define WILD_CROP_CORN      6


/* Used for wilderness generation */
#define DIR_NORTH   0
#define DIR_EAST    1
#define DIR_SOUTH   2
#define DIR_WEST    3


/* Monk martial arts */
#define MAX_MA  17
#define MA_KNEE 1
#define MA_SLOW 2
#define MA_STUN 3


/* Mental links */
#define LINK_NONE       0
#define LINK_DOMINANT   1
#define LINK_DOMINATED  2


/* Space/Time Anchor radius */
#define ANCHOR_RADIUS   12


/* Probing radius */
#define PROBE_RADIUS    20


/* Bow brands */
enum
{
    BOW_BRAND_ELEC = 0,
    BOW_BRAND_COLD,
    BOW_BRAND_FIRE,
    BOW_BRAND_ACID,
    BOW_BRAND_THUNDER,
    BOW_BRAND_CONF,
    BOW_BRAND_POISON,
    BOW_BRAND_PIERCE,
    BOW_BRAND_SHARDS,
    BOW_BRAND_ICE,
    BOW_BRAND_FLAME,
    BOW_BRAND_WATER,
    BOW_BRAND_POWER,
    BOW_BRAND_SONIC
};


/* Monster status (hostile by default) */
#define MSTATUS_HOSTILE     0   /* hostile */
#define MSTATUS_SUMMONED    1   /* hostile, summoned by the player */
#define MSTATUS_GUARD       2   /* guard, controlled by the player */
#define MSTATUS_FOLLOW      3   /* follower, controlled by the player */
#define MSTATUS_ATTACK      4   /* attacker, controlled by the player */


/* Dungeon master flags */
#define DM_IS_MASTER        0x00000001
#define DM_SECRET_PRESENCE  0x00000002  /* Hidden dungeon master */
#define DM_CAN_MUTATE_SELF  0x00000004  /* This option allows change of the DM_ options (self) */
#define DM_CAN_ASSIGN       0x00000008  /* This option allows change of the DM_ options (other) */
#define DM___MENU           0x000000F0  /* Dungeon Master Menu: (shortcut to set all) */
#define DM_CAN_BUILD        0x00000010  /* Building menu */
#define DM_LEVEL_CONTROL    0x00000020  /* Static/unstatic level */
#define DM_CAN_SUMMON       0x00000040  /* Summon monsters */
#define DM_CAN_GENERATE     0x00000080  /* Generate vaults/items */
#define DM_MONSTER_FRIEND   0x00000100  /* Monsters are non hostile */
#define DM_INVULNERABLE     0x00000200  /* Cannot be harmed */
#define DM_GHOST_HANDS      0x00000400  /* Can interact with world even as a ghost */
#define DM_GHOST_BODY       0x00000800  /* Can carry/wield items even as a ghost */
#define DM_NEVER_DISTURB    0x00001000  /* Never disturbed (currently unused) */
#define DM_SEE_LEVEL        0x00002000  /* See all level */
#define DM_SEE_MONSTERS     0x00004000  /* Free ESP */
#define DM_SEE_PLAYERS      0x00008000  /* Can ESP other players */
#define DM_HOUSE_CONTROL    0x00010000  /* Can reset houses */
#define DM_MISC_XXX1        0x10000000
#define DM_MISC_XXX2        0x20000000
#define DM_MISC_XXX3        0x40000000
#define DM_MISC_XXX4        0x80000000


/*
 * Flags for monster generation
 */
#define MON_SLEEP   0x01    /* Generate asleep */
#define MON_GROUP   0x02    /* Add an escort */
#define MON_CLONE   0x04    /* Generate as clone */


/* PvP modes */
#define PVP_CHECK_ONE   0
#define PVP_CHECK_BOTH  1
#define PVP_DIRECT      2
#define PVP_INDIRECT    3
#define PVP_ADD         4
#define PVP_REMOVE      5

#define PVP_BRUTAL      0
#define PVP_DANGEROUS   1
#define PVP_NORMAL      2
#define PVP_SAFE        3
#define PVP_DISABLED    4


/*
 * Time bubble scale factors in percentage terms
 */
#define MAX_TIME_SCALE  1000
#define MIN_TIME_SCALE  10
#define RUNNING_FACTOR  500 /* Increase time by this percentage when running */
#define NORMAL_TIME     100 /* 100% */


/*
 * Chat Channels defines
 */

/* Channel modes */
#define CM_KEYLOCK      0x01 /* +k More similar to IRC's +A */
#define CM_SECRET       0x02 /* +s */
#define CM_MODERATE     0x04 /* +m */
#define CM_SERVICE      0x08 /* +! service channel */
#define CM_PLOG         0x10 /* +p log to plog */

/* User-channel modes */
#define UCM_EAR         0x01
#define UCM_VOICE       0x02
#define UCM_OPER        0x04
#define UCM_BAN         0x08

#define UCM_LEAVE       (UCM_EAR | UCM_VOICE | UCM_OPER)


/*
 * Per-player artifact states
 */
#define ARTS_NOT_FOUND  0
#define ARTS_GENERATED  1
#define ARTS_FOUND      2
#define ARTS_ABANDONED  3
#define ARTS_SOLD       4
#define ARTS_CREATED    5


/*
 * Drop states
 */
#define DROP_NONE   0
#define DROP_ERROR  1
#define DROP_OK     2


#define INHIBIT_DEPTH   -100


#define restrict_winner(P, T) \
    ((P)->total_winner && \
        ((T)->artifact->aidx != ART_MORGOTH) && ((T)->artifact->aidx != ART_GROND))


#define restrict_artifacts(P, T) \
    (OPT_P(P, birth_no_artifacts) && \
        ((T)->artifact->aidx != ART_MORGOTH) && ((T)->artifact->aidx != ART_GROND))


/*
 * Cave location is icky (house, vault...)
 */
#define is_icky(D, Y, X) \
    (cave_get(D) && (cave_get(D)->info[(Y)][(X)] & CAVE_ICKY))


/*
 * Cave location is no-teleport (pit, nest)
 */
#define is_notele(D, Y, X) \
    (cave_get(D) && (cave_get(D)->info[(Y)][(X)] & CAVE_NOTELE))


/*
 * Is that object shimmering?
 */
#define object_shimmer(T) \
    (object_attr_default((T)->kind) == TERM_MULTI)


/*
 * Object is marked
 */
#define object_marked(P, T) \
    ((P)->obj_marked[(T)] || ((P)->dm_flags & DM_SEE_LEVEL))


/*
 * Object is marked and seen
 */
#define object_seen(P, T) \
    (((P)->obj_marked[(T)] == MARK_SEEN) || ((P)->dm_flags & DM_SEE_LEVEL))


/*
 * Is that monster shimmering?
 */
#define monster_shimmer(R) \
    (rf_has((R)->flags, RF_ATTR_MULTI) || rf_has((R)->flags, RF_ATTR_FLICKER))


/*
 * Should we shimmer stuff for this player?
 */
#define allow_shimmer(P) \
    (OPT_P(P, animate_flicker) && !(P)->use_graphics)


/*
 * Missiles
 */
#define magic_ammo_p(T) \
    (obj_is_ammo(NULL, T) && ((T)->sval == SV_AMMO_MAGIC))


/*
 * Determine if a "legal" grid is a "basic floor" grid
 *
 * Line 1 -- floor tiles
 * Line 2 -- other "floor" grids
 */
#define cave_floor_basic(F) \
    ((((F) >= FEAT_FLOOR) && ((F) <= FEAT_INVIS)) || \
        (((F) >= FEAT_WATER) && ((F) <= FEAT_CROP)))


/*
 * Determine if a "legal" grid is a "basic wall" grid
 */
#define cave_wall_basic(F) \
    (((F) >= FEAT_SECRET) && ((F) <= FEAT_PERM_SOLID))


/*
 * Determine if a "legal" grid is a "basic tree" grid
 */
#define cave_tree_basic(F) \
    (((F) >= FEAT_TREE) && ((F) <= FEAT_EVIL_TREE))


/*
 * Determine if a "legal" grid is a "fountain" grid
 */
#define cave_fountain_basic(F) \
    (((F) >= FEAT_FOUNTAIN) && ((F) <= FEAT_FNT_DRIED))


/*
 * Wilderness macros
 */
#define wild_radius(idx) \
    (abs(wild_info[idx].world_x) + abs(wild_info[idx].world_y))

#define monster_level(DEPTH) \
    ((DEPTH >= 0)? DEPTH: wild_info[DEPTH].monst_lev)

#define object_level(DEPTH) \
    ((DEPTH >= 0)? DEPTH: wild_radius(DEPTH))

#define town_area(DEPTH) \
    ((DEPTH <= 0)? (wild_radius(DEPTH) <= 2): FALSE)


/*
 * DM macros
 */
#define is_dm(IND) \
    ((player_get(IND)->dm_flags & DM_IS_MASTER)? TRUE: FALSE)

#define is_dm_p(P) \
    (((P)->dm_flags & DM_IS_MASTER)? TRUE: FALSE)

#define is_memorized(P, I) \
    (((I) & CAVE_MARK) || ((P)->dm_flags & DM_SEE_LEVEL))


/*
 * Prevents abuse from level 1 characters
 */
#define newbies_cannot_drop(P) \
    (((P)->lev == 1) && cfg_newbies_cannot_drop)


#define on_channel(P, I) ((P)->on_channel[(I)] & UCM_EAR)


/* can_talk(p_ptr, channel_index) test:
 * Line 1. Present on channel
 * Line 2. Not banned
 * Line 3. Not moderated
 * Line 4. Moderated, BUT
 * Line 5.  user has Voice or Op */
#define can_talk(P, I) \
    (on_channel((P),(I)) && \
    !((P)->on_channel[(I)] & UCM_BAN) && \
    (!(channels[(I)].mode & CM_MODERATE) || ( \
    (channels[(I)].mode & CM_MODERATE) && \
    ((P)->on_channel[(I)] & UCM_VOICE || (P)->on_channel[(I)] & UCM_OPER) \
    )))


#define clog(C, M) if (chan_ ## C) msg_channel(chan_ ## C, (M))
#define audit(M) clog(audit, (M))
#define debug(M) clog(debug, (M))
#define cheat(M) clog(cheat, (M))


/* Check if object is owned by player */
#define obj_own_p(P, O) (!(O)->owner || ((P)->id == (O)->owner))

/* Macro for "inscription_prevent" */
#define CGI(O, M, H) inscription_prevent((O)->note, (M), (H))

/* Macro for "check_prevent_inscription" */
#define CPI(P, M) (P)->prevents[(byte)(M)]

/* Overloaded guard-inscriptions */
#define protected_p(P, O, M, H) \
    (!is_dm_p((P)) && !obj_own_p((P), (O)) && CGI((O), (M), (H)))

/* Check guard inscription and abort (chunk of code) */
#define __trap(P, X) \
    if ((X)) \
    { \
        msg((P), "The item's inscription prevents it."); \
        return; \
    }

#define __trapR(P, X, RET) \
    if ((X)) \
    { \
        msg((P), "The item's inscription prevents it."); \
        return (RET); \
    }


#define player_undead(P) \
    ((P)->ghost && player_can_undead(P))


#define player_passwall(P) \
    ((P)->ghost || (P)->timed[TMD_WRAITH])


#define msg_misc(P, A) msg_print_near((P), MSG_PY_MISC, (A))
#define msg_spell(P, A) msg_print_near((P), MSG_PY_SPELL, (A))

#define in_store(P) ((P)->store_num != -1)


#define ROUND(x) ((((x) - floor(x)) * 10 >= 5)? floor((x) + 1): floor(x))


/* Number of entries in presets.prf */
#define MAX_PRESETS 6


/*** These don't belong here ***/


/* Randomly select one of the entries in an array */
#define ONE_OF(x) x[randint0(N_ELEMENTS(x))]


/*
 * Keyset mappings for various keys.
 */
#define ARROW_DOWN      0x80
#define ARROW_LEFT      0x81
#define ARROW_RIGHT     0x82
#define ARROW_UP        0x83
#define KC_ENTER        0x9C
#define ESCAPE          0xE000


/* Analogous to isdigit() etc in ctypes */
#define isarrow(c)  ((c >= ARROW_DOWN) && (c <= ARROW_UP))


#endif /* INCLUDED_S_DEFINES_H */
