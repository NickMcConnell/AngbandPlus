/*
 * File: z-defines.h
 * Purpose: Global constants and macro definitions
 */

#ifndef INCLUDED_Z_DEFINES_H
#define INCLUDED_Z_DEFINES_H

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
 * RLE encoding modes
 */
#define RLE_NONE 0
#define RLE_CLASSIC 1
#define RLE_LARGE 2

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
#define MASTER_DEBUG    8

/*
 * The types of special file perusal.
 */
#define SPECIAL_FILE_NONE       0
#define SPECIAL_FILE_OBJECT     1
#define SPECIAL_FILE_ARTIFACT   2
#define SPECIAL_FILE_EGO        3
#define SPECIAL_FILE_KILL       4
#define SPECIAL_FILE_FEATURE    5
#define SPECIAL_FILE_TRAP       6
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
#define SPECIAL_FILE_RUNE       17
#define SPECIAL_FILE_DUNGEONS   18

/* Is string empty? Beats calling strlen */
#define STRZERO(S) \
    ((S)[0] == '\0')

/* Constants for item hooks */
#define HOOK_CARRY      0
#define HOOK_WEAPON     1
#define HOOK_ARMOR      2
#define HOOK_IDENTIFY   3
#define HOOK_RECHARGE   4
#define HOOK_AMMO       5
#define HOOK_SEND       6
#define HOOK_POISON     7
#define HOOK_UNCURSE    8
#define HOOK_DRAIN      9
#define HOOK_STAFF      10

#define N_HOOKS         11

/* Special hooks */
#define HOOK_RECALL     253
#define HOOK_DOWN       254
#define HOOK_CONFIRM    255

/*
 * Modes for item lists in "show_inven()", "show_equip()" and "show_floor()"
 */
enum
{
    OLIST_NONE   = 0x00,        /* No options */
    OLIST_WINDOW = 0x01,        /* Display list in a sub-term (left-align) */
    OLIST_QUIVER = 0x02,        /* Display quiver lines */
    OLIST_GOLD   = 0x04,        /* Include gold in the list */
    OLIST_WEIGHT = 0x08,        /* Show item weight */
    OLIST_PRICE  = 0x10,        /* Show item price */
    OLIST_FAIL   = 0x20,        /* Show device failure */
    OLIST_SEMPTY = 0x40,        /* Show empty slots */
    OLIST_FLOOR  = 0x80,        /* Include first floor item in the list */
    OLIST_BOOK_TAGS = 0x100,    /* Use book sval for item tags */
    OLIST_RECHARGE = 0x200      /* Show failure for device recharging */
};

#endif /* INCLUDED_Z_DEFINES_H */
