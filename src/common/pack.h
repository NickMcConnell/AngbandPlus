/*
 * File: pack.h
 * Purpose: Packet types
 */

/* Packet types 0-13 are "administrative" */
#define PKT_UNDEFINED       0
#define PKT_VERIFY          1
#define PKT_PLAY            3
#define PKT_QUIT            4
#define PKT_TEXT_SCREEN     6
#define PKT_BASIC_INFO      7
#define PKT_DEATH_CAUSE     8
#define PKT_WINNER          9
#define PKT_END             11
#define PKT_KEEPALIVE       12
#define PKT_STRUCT_INFO     13

/* Packet types 18-59 are info that is sent to the client */
#define PKT_LEV             18
#define PKT_WEIGHT          19
#define PKT_PLUSSES         20
#define PKT_AC              21
#define PKT_EXP             22
#define PKT_GOLD            23
#define PKT_HP              24
#define PKT_SP              25
#define PKT_CHAR_INFO       26
#define PKT_VARIOUS         27
#define PKT_STAT            28
#define PKT_HISTORY         29

#define PKT_INVEN           30
#define PKT_EQUIP           31
#define PKT_TITLE           32
#define PKT_TURN            33
#define PKT_DEPTH           34
#define PKT_FOOD            35
#define PKT_STATUS          36
#define PKT_RECALL          39

#define PKT_STATE           40
#define PKT_LINE_INFO       41
#define PKT_SPEED           42
#define PKT_STUDY           43
#define PKT_QUIVER_SIZE     44
#define PKT_SHOW_FLOOR      45
#define PKT_MESSAGE         46
#define PKT_CHAR            47
#define PKT_SPELL_INFO      48
#define PKT_FLOOR           49

#define PKT_SPECIAL_OTHER   50
#define PKT_STORE           51
#define PKT_STORE_INFO      52
#define PKT_TARGET_INFO     53
#define PKT_SOUND           54
#define PKT_MINI_MAP        55
#define PKT_SKILLS          57
#define PKT_PAUSE           58
#define PKT_MONSTER_HEALTH  59

/* Packet types 60-66 are sent from either the client or server */
#define PKT_ITEM            61
#define PKT_SELL            62
#define PKT_PARTY           63
#define PKT_SPECIAL_LINE    64
#define PKT_FULLMAP         65
#define PKT_SYMBOL_QUERY    66

/* Extra packets */
#define PKT_POLY            68
#define PKT_BREATH          69

/* Packet types 70-119 are sent from the client */
#define PKT_WALK            70
#define PKT_RUN             71
#define PKT_TUNNEL          72
#define PKT_AIM_WAND        73
#define PKT_DROP            74
#define PKT_FIRE            75
#define PKT_PICKUP          76
#define PKT_DESTROY         77
#define PKT_TARGET_CLOSEST  78
#define PKT_SPELL           79

#define PKT_OPEN            80
#define PKT_PRAY            81
#define PKT_QUAFF           82
#define PKT_READ            83
#define PKT_SEARCH          84
#define PKT_TAKE_OFF        85
#define PKT_USE             86
#define PKT_THROW           87
#define PKT_WIELD           88
#define PKT_ZAP             89

#define PKT_TARGET          90
#define PKT_INSCRIBE        91
#define PKT_UNINSCRIBE      92
#define PKT_ACTIVATE        93
#define PKT_BASH            94
#define PKT_DISARM          95
#define PKT_EAT             96
#define PKT_FILL            97
#define PKT_LOCATE          98
#define PKT_MAP             99

#define PKT_SEARCH_MODE     100
#define PKT_SPIKE           101
#define PKT_QUEST           102
#define PKT_CLOSE           103
#define PKT_GAIN            104
#define PKT_GO_UP           105
#define PKT_GO_DOWN         106
#define PKT_PURCHASE        107
#define PKT_STORE_LEAVE     108
#define PKT_STORE_CONFIRM   109

#define PKT_DROP_GOLD       110
#define PKT_REDRAW          111
#define PKT_REST            112
#define PKT_GHOST           113
#define PKT_SUICIDE         114
#define PKT_STEAL           115
#define PKT_OPTIONS         116
#define PKT_MASTER          118 /* dungeon master commands */
#define PKT_MIMIC           119

/* Packet types 121-123 are more administrative stuff */
#define PKT_CLEAR           123

/* Packet type 150 are hacks */
#define PKT_FLUSH           150
#define PKT_CURSOR          151

/* Extra packets */
#define PKT_OBSERVE         160
#define PKT_STORE_EXAMINE   161
#define PKT_CHANGEPASS      162
#define PKT_OBJFLAGS        163
#define PKT_ALTER           164
#define PKT_SPELL_DESC      165
#define PKT_FIRE_AT_NEAREST 166
#define PKT_DTRAP           167
#define PKT_JUMP            168
#define PKT_CHANNEL         169
#define PKT_TERM            170
#define PKT_SOCIAL          171
#define PKT_MONLIST         172
#define PKT_FEELING         173
#define PKT_INTERACTIVE     174
#define PKT_FOUNTAIN        175
#define PKT_ICKY            176
#define PKT_OBJLIST         177
#define PKT_CENTER          178
#define PKT_CHAR_DUMP       179
#define PKT_IGNORE          180
#define PKT_USE_ANY         181
#define PKT_STORE_ORDER     182
#define PKT_PLAYER          183


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
 * PKT_STRUCT_INFO helpers
 */
#define STRUCT_INFO_UNKNOWN 0
#define STRUCT_INFO_LIMITS  1
#define STRUCT_INFO_RACE    2
#define STRUCT_INFO_CLASS   3
#define STRUCT_INFO_INVEN   4
#define STRUCT_INFO_SOCIALS 5
#define STRUCT_INFO_KINDS   6
#define STRUCT_INFO_HINTS   7
#define STRUCT_INFO_RINFO   8


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
#define NTERM_WIN_OVERHEAD      0
#define NTERM_WIN_MAP           1
#define NTERM_WIN_XXXX1         2
#define NTERM_WIN_OBJLIST       3
#define NTERM_WIN_OBJECT        4
#define NTERM_WIN_MONSTER       5
#define NTERM_WIN_MONLIST       6
#define NTERM_WIN_SPECIAL       7
