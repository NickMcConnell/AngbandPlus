/*
 * File: pack.h
 * Purpose: Network packets
 */

/*
 * Packet types
 */
enum
{
    #define PKT(a, b, c, d, e) PKT_##a,
    #include "../common/list-packets.h"
    #undef PKT
    PKT_MAX
};

/*
 * Possible error codes returned
 */
#define SUCCESS         0x00
#define E_VERSION_OLD   0x01
#define E_INVAL         0x02
#define E_ACCOUNT       0x03
#define E_GAME_FULL     0x04
#define E_SOCKET        0x05
#define E_VERSION_NEW   0x06

/*
 * Connection types
 */
#define CONNTYPE_PLAYER     0x00
#define CONNTYPE_CONSOLE    0x01
#define CONNTYPE_MONITOR    0x02
#define CONNTYPE_ERROR      0xFF

/*
 * Connection states
 */
#define CONN_FREE       0x00
#define CONN_SETUP      0x01
#define CONN_PLAYING    0x02
#define CONN_QUIT       0x04
#define CONN_CONSOLE    0x08

/*  
 * PKT_STRUCT_INFO helpers
 */
#define STRUCT_INFO_UNKNOWN 0
#define STRUCT_INFO_LIMITS  1
#define STRUCT_INFO_RACE    2
#define STRUCT_INFO_CLASS   3
#define STRUCT_INFO_BODY    4
#define STRUCT_INFO_SOCIALS 5
#define STRUCT_INFO_KINDS   6
#define STRUCT_INFO_EGOS    7
#define STRUCT_INFO_RINFO   8
#define STRUCT_INFO_RBINFO  9
#define STRUCT_INFO_CURSES  10
#define STRUCT_INFO_REALM   11
#define STRUCT_INFO_FEAT    12
#define STRUCT_INFO_TRAP    13
#define STRUCT_INFO_TIMED   14
#define STRUCT_INFO_PROPS   15

/*
 * PKT_TERM helpers
 */
#define NTERM_ACTIVATE  0
#define NTERM_CLEAR     1
#define NTERM_CURSOR    2
#define NTERM_SAVE      3
#define NTERM_LOAD      4
#define NTERM_KEY       5
#define NTERM_HOLD      6
#define NTERM_FRESH     7
#define NTERM_POP       8
#define NTERM_FLUSH     9

/* NTERM_ACTIVATE */
#define NTERM_WIN_OVERHEAD  0
#define NTERM_WIN_MAP       1
#define NTERM_WIN_XXXX1     2
#define NTERM_WIN_OBJLIST   3
#define NTERM_WIN_OBJECT    4
#define NTERM_WIN_MONSTER   5
#define NTERM_WIN_MONLIST   6
#define NTERM_WIN_SPECIAL   7
