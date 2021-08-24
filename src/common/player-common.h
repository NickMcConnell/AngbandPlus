/*
 * File: player-common.h
 * Purpose: Player interface
 */

#ifndef INCLUDED_PLAYER_COMMON_H
#define INCLUDED_PLAYER_COMMON_H

/* Maximum number of spells per page */
/* Note: this must be greater than the maximum number of spells in all books (currently 9) */
#define MAX_SPELLS_PER_PAGE 10

/*
 * Player constants
 */

#define PY_MAX_EXP      99999999L   /* Maximum exp */
#define PY_MAX_GOLD     999999999L  /* Maximum gold */
#define PY_MAX_LEVEL    50          /* Maximum level */

/*
 * Player food values
 */
#define PY_FOOD_MAX     17000   /* Food value (Bloated) */
#define PY_FOOD_FULL    10000   /* Food value (Normal) */
#define PY_FOOD_ALERT   2000    /* Food value (Hungry) */
#define PY_FOOD_WEAK    1000    /* Food value (Weak) */
#define PY_FOOD_FAINT   500     /* Food value (Fainting) */
#define PY_FOOD_STARVE  100     /* Food value (Starving) */

/** Sexes **/

/*
 * Maximum number of player "sex" types (see "table.c", etc)
 */
#define MAX_SEXES   3

/*
 * Player sex constants (hard-coded by save-files, arrays, etc)
 */
#define SEX_FEMALE  0
#define SEX_MALE    1
#define SEX_NEUTER  2

/*
 * Player magic realms
 */
enum
{
    #define REALM(a, b, c, d, e, f) REALM_##a,
    #include "list-magic-realms.h"
    #undef REALM
    REALM_MAX
};

/*
 * Timed effects
 */
enum
{
    #define TMD(a, b, c, d, e, f, g, h, i, j, k, l, m) TMD_##a,
    #include "list-player-timed.h"
    #undef TMD
    TMD_MAX
};

/*
 * Number of history flags
 */
#define N_HISTORY_FLAGS (1 + STAT_MAX + RES_PANELS * RES_ROWS)

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
 * The number of wilderness levels we have allocated.
 */
#define MAX_WILD    2112

/*
 * Maximum number of lines in 'special info' (*ID*, Self-Knowledge, Recalls)
 */
#define MAX_TXT_INFO    384

/* Constants for character history */
#define N_HIST_LINES    3
#define N_HIST_WRAP     73

/* Character rolling methods */
enum birth_rollers
{
    BR_QDYNA = -2,
    BR_QUICK = -1,
    BR_POINTBASED = 0,
    BR_NORMAL,
    MAX_BIRTH_ROLLERS
};

/*
 * Maximum number of characters per account
 */
#define MAX_ACCOUNT_CHARS 12

/* Necromancers can turn into an undead being */
#define player_can_undead(P) \
    (player_has((P), PF_UNDEAD_POWERS) && ((P)->state.stat_use[STAT_INT] >= 18+70))

/*
 * List of resistances and abilities to display
 */
#define RES_PANELS  4
#define RES_ROWS    9

/*
 * List of kinds of item, for pseudo-id and ego ignoring.
 */
typedef enum
{
    ITYPE_NONE,
    #define ITYPE(a, b) ITYPE_##a,
    #include "list-ignore-types.h"
    #undef ITYPE

    ITYPE_MAX
} ignore_type_t;

#define ITYPE_SIZE          FLAG_SIZE(ITYPE_MAX)

#define itype_has(f, flag)  flag_has_dbg(f, ITYPE_SIZE, flag, #f, #flag)
#define itype_on(f, flag)   flag_on_dbg(f, ITYPE_SIZE, flag, #f, #flag)
#define itype_wipe(f)       flag_wipe(f, ITYPE_SIZE)

/* Temporary hack -- "ghost" class index */
#define CLASS_GHOST 15

/* History message types */
enum
{
    #define HIST(a, b) HIST_##a,
    #include "list-history-types.h"
    #undef HIST

    HIST_MAX
};

#define HIST_SIZE                FLAG_SIZE(HIST_MAX)

#define hist_has(f, flag)        flag_has_dbg(f, HIST_SIZE, flag, #f, #flag)
#define hist_is_empty(f)         flag_is_empty(f, HIST_SIZE)
#define hist_on(f, flag)         flag_on_dbg(f, HIST_SIZE, flag, #f, #flag)
#define hist_off(f, flag)        flag_off(f, HIST_SIZE, flag)
#define hist_wipe(f)             flag_wipe(f, HIST_SIZE)
#define hist_copy(f1, f2)        flag_copy(f1, f2, HIST_SIZE)

/*
 * Player structures
 */

/*
 * Structure for the "quests"
 */
struct quest
{
    struct quest *next;
    byte index;
    char *name;
    byte level;                 /* Dungeon level */
    struct monster_race *race;  /* Monster race */
    s16b cur_num;               /* Number killed */
    s16b max_num;               /* Number required */
    s16b timer;                 /* Time left before quest is over */
};

/*
 * Player body info
 */
struct equip_slot
{
    struct equip_slot *next;

    s16b type;
    char *name;
    struct object *obj;
};

struct player_body
{
    struct player_body *next;
    char *name;
    s16b count;
    struct equip_slot *slots;
};

extern struct player_body *bodies;

/*
 * Player racial info
 */
struct player_race
{
    char *name;                 /* Name */
    unsigned int ridx;          /* Index */
    struct player_race *next;
    s16b r_adj[STAT_MAX];       /* Racial stat bonuses */
    s16b r_skills[SKILL_MAX];   /* Racial skills */
    byte r_mhp;                 /* Race hit-dice modifier */
    s16b r_exp;                 /* Race experience factor */
    int b_age;                  /* Base age */
    int m_age;                  /* Mod age */
    int m_b_ht;                 /* Base height (males) */
    int m_m_ht;                 /* Mod height (males) */
    int m_b_wt;                 /* Base weight (males) */
    int m_m_wt;                 /* Mod weight (males) */
    int f_b_ht;                 /* Base height (females) */
    int f_m_ht;                 /* Mod height (females)   */
    int f_b_wt;                 /* Base weight (females) */
    int f_m_wt;                 /* Mod weight (females) */
    byte infra;                 /* Infra-vision range */
    int body;                   /* Race body */
    struct history_chart *history;
    bitflag flags[OF_SIZE];     /* Racial (object) flags */
    bitflag pflags[PF_SIZE];    /* Racial (player) flags */
    struct element_info el_info[ELEM_MAX];  /* Racial resists */
};

extern struct player_race *races;

/*
 * Items the player starts with. Used in player_class and specified in
 * class.txt.
 */
struct start_item
{
    struct object_kind *kind;   /* Object kind */
    int min;                    /* Minimum starting amount */
    int max;                    /* Maximum starting amount */
    int flag;                   /* Flag for no_recall characters */
    struct start_item *next;
};

/*
 * Structure for magic realms
 */
struct magic_realm
{
    byte index;
    int stat;
    const char *verb;
    const char *spell_noun;
    const char *book_noun;
    const char *adjective;
};

extern struct magic_realm realms[];

/*
 * A structure to hold class-dependent information on spells.
 */
struct class_spell
{
    char *name;
    char *text;
    struct effect *effect;  /* The spell's effect */
    int sidx;               /* The index of this spell for this class */
    int bidx;               /* The index into the player's books array */
    int slevel;             /* Required level (to learn) */
    int smana;              /* Required mana (to cast) */
    int sfail;              /* Minimum chance of failure */
    int sexp;               /* Encoded experience bonus */
    int sproj;              /* Can be projected */
};

/*
 * A structure to hold class-dependent information on spell books.
 */
struct class_book
{
    byte tval;                  /* Item type of the book */
    int sval;                   /* Item sub-type for book (book number) */
    int realm;                  /* The magic realm of this book */
    int num_spells;             /* Number of spells in this book */
    struct class_spell *spells; /* Spells in the book */
};

/*
 * Information about class magic knowledge
 */
struct class_magic
{
    byte spell_first;                   /* Level of first spell */
    int spell_weight;                   /* Max armour weight to avoid mana penalties */
    struct magic_realm *spell_realm;    /* Primary spellcasting realm */
    int num_books;                      /* Number of spellbooks */
    struct class_book *books;           /* Details of spellbooks */
    byte total_spells;                  /* Number of spells for this class */
};

/*
 * Player class info
 */
struct player_class
{
    char *name;                     /* Name */
    char *title[PY_MAX_LEVEL / 5];  /* Titles */
    unsigned int cidx;              /* Index */
    struct player_class *next;
    s16b c_adj[STAT_MAX];           /* Class stat modifier */
    s16b c_skills[SKILL_MAX];       /* Class skills */
    int x_skills[SKILL_MAX];        /* Extra skills */
    byte c_mhp;                     /* Class hit-dice adjustment */
    s16b c_exp;                     /* Class experience factor */
    bitflag pflags[PF_SIZE];        /* Class (player) flags */
    int max_attacks;                /* Maximum possible attacks */
    int min_weight;                 /* Minimum weapon weight for calculations */
    int att_multiply;               /* Multiplier for attack calculations */
    int sense_base;                 /* Base pseudo-id value */
    int sense_div;                  /* Pseudo-id divisor */
    struct start_item *start_items; /* The starting inventory */
    struct class_magic magic;       /* Magic spells */
    byte attr;                      /* Class color */
};

extern struct player_class *classes;

/*  
 * Histories are a graph of charts; each chart contains a set of individual
 * entries for that chart, and each entry contains a text description and a
 * successor chart to move history generation to
 * For example:
 *   chart 1
 *   {
 *     entry
 *     {
 *       desc "You are the illegitimate and unacknowledged child";
 *       next 2;
 *     };
 *     entry
 *     {
 *       desc "You are the illegitimate but acknowledged child";
 *       next 2;
 *     };
 *     entry
 *     {
 *       desc "You are one of several children";
 *       next 3;
 *     };
 *   };
 *
 * History generation works by walking the graph from the starting chart for
 * each race, picking a random entry (with weighted probability) each time
 */
struct history_entry
{
    struct history_entry *next;
    struct history_chart *succ;
    int isucc;
    int roll;
    char *text;
};

struct history_chart
{
    struct history_chart *next;
    struct history_entry *entries;
    unsigned int idx;
};

/*
 * Some more player information
 *
 * This information is retained across player lives
 */
typedef struct
{
    char full_name[NORMAL_WID];     /* Full name */
    bool opt[OPT_MAX];              /* Options */
    byte hitpoint_warn;             /* Hitpoint warning (0 to 9) */
    byte lazymove_delay;            /* Delay in cs before moving to allow another keypress */
    byte delay_factor;              /* Delay factor (0 to 255) */
    byte ignore_lvl[ITYPE_MAX];     /* Auto-ignore level (0 to 6) */
} player_other;

/*
 * Temporary, derived, player-related variables used during play but not saved
 */
struct player_upkeep
{
    byte new_level_method;          /* Climb up stairs, down, or teleport level? */
    bool funeral;                   /* True if player is leaving */
    s16b new_spells;                /* Number of spells available */
    struct actor health_who;        /* Who's shown on the health bar */
    struct actor_race monster_race; /* Monster race trackee */
    struct object *object;          /* Object trackee */
    u32b notice;                    /* Bit flags for pending actions */
    u32b update;                    /* Bit flags for recalculations needed */
    u32b redraw;                    /* Bit flags for changes that need to be redrawn by the UI */
    s16b resting;                   /* Resting counter */
    bool running;                   /* Are we running? */
    bool running_firststep;         /* Is this our first step running? */
    struct object **quiver;         /* Quiver objects */
    struct object **inven;          /* Inventory objects */
    s16b total_weight;              /* Total weight being carried */
    s16b inven_cnt;                 /* Number of items in inventory */
    s16b equip_cnt;                 /* Number of items in equipment */
    s16b quiver_cnt;                /* Number of items in the quiver */
    bool running_update;            /* True if updating monster/object lists while running */
};

/*
 * Player history table
 */
struct history_info
{
    bitflag type[HIST_SIZE];    /* Kind of history item */
    s16b dlev;                  /* Dungeon level when this item was recorded */
    s16b clev;                  /* Character level when this item was recorded */
    byte a_idx;                 /* Artifact this item relates to */
    char name[NORMAL_WID];      /* Artifact name */
    hturn turn;                 /* Turn this item was recorded on */
    char event[NORMAL_WID];     /* The text of the item */
};

/*
 * Player sex info
 */
typedef struct player_sex
{
    const char *title;     /* Type of sex */
    const char *winner;    /* Name of winner */
} player_sex;

extern player_sex sex_info[MAX_SEXES];

/*
 * Square flags
 */
enum
{
    #define SQUARE(a, b) SQUARE_##a,
    #include "list-square-flags.h"
    #undef SQUARE
    SQUARE_MAX
};

#define SQUARE_SIZE                FLAG_SIZE(SQUARE_MAX)

#define sqinfo_has(f, flag)        flag_has_dbg(f, SQUARE_SIZE, flag, #f, #flag)
#define sqinfo_next(f, flag)       flag_next(f, SQUARE_SIZE, flag)
#define sqinfo_is_empty(f)         flag_is_empty(f, SQUARE_SIZE)
#define sqinfo_is_full(f)          flag_is_full(f, SQUARE_SIZE)
#define sqinfo_is_inter(f1, f2)    flag_is_inter(f1, f2, SQUARE_SIZE)
#define sqinfo_is_subset(f1, f2)   flag_is_subset(f1, f2, SQUARE_SIZE)
#define sqinfo_is_equal(f1, f2)    flag_is_equal(f1, f2, SQUARE_SIZE)
#define sqinfo_on(f, flag)         flag_on_dbg(f, SQUARE_SIZE, flag, #f, #flag)
#define sqinfo_off(f, flag)        flag_off(f, SQUARE_SIZE, flag)
#define sqinfo_wipe(f)             flag_wipe(f, SQUARE_SIZE)
#define sqinfo_setall(f)           flag_setall(f, SQUARE_SIZE)
#define sqinfo_negate(f)           flag_negate(f, SQUARE_SIZE)
#define sqinfo_copy(f1, f2)        flag_copy(f1, f2, SQUARE_SIZE)
#define sqinfo_union(f1, f2)       flag_union(f1, f2, SQUARE_SIZE)
#define sqinfo_comp_union(f1, f2)  flag_comp_union(f1, f2, SQUARE_SIZE)
#define sqinfo_inter(f1, f2)       flag_inter(f1, f2, SQUARE_SIZE)
#define sqinfo_diff(f1, f2)        flag_diff(f1, f2, SQUARE_SIZE)

struct player_square
{
    byte feat;
    bitflag *info;
    byte cost;
    byte when;
    struct object *obj;
    struct trap *trap;
};

struct player_cave
{
    u16b feeling_squares;   /* How many feeling squares the player has visited */
    int height;
    int width;
    struct player_square **squares;
    bool allocated;
};

/*
 * Player info recording the original (pre-ghost) cause of death
 */
struct player_death_info
{
    char title[NORMAL_WID];         /* Title */
    s16b max_lev;                   /* Max level */
    s16b lev;                       /* Level */
    s32b max_exp;                   /* Max experience */
    s32b exp;                       /* Experience */
    s32b au;                        /* Gold */
    s16b max_depth;                 /* Max depth */
    s16b depth;                     /* What depth we died on */
    char died_from[NORMAL_WID];     /* Cause of death */
    time_t time;                    /* Time of death */
    char ctime[NORMAL_WID];
};

/* The information needed to show a single "grid" */
typedef struct
{
    u16b a; /* Color attribute */
    char c; /* ASCII character */
} cave_view_type;

/* Information about a "hostility" */
typedef struct _hostile_type
{
    s32b id;                    /* ID of player we are hostile to */
    struct _hostile_type *next; /* Next in list */
} hostile_type;

/* Archer flags */
struct bow_brand
{
    bitflag type;
    bool blast;
    int dam;
};

/*
 * Most of the "player" information goes here.
 *
 * This stucture gives us a large collection of player variables.
 *
 * This entire structure is wiped when a new character is born.
 *
 * This structure is more or less laid out so that the information
 * which must be saved in the savefile precedes all the information
 * which can be recomputed as needed.
 */
struct player
{
    /*** Angband common fields ***/

    s16b py;                            /* Player location */
    s16b px;                            /* Player location */
    byte psex;                          /* Sex index */
    const struct player_sex *sex;
    const struct player_race *race;
    const struct player_class *clazz;
    byte hitdie;                        /* Hit dice (sides) */
    s16b expfact;                       /* Experience factor */
    s16b age;                           /* Characters age */
    s16b ht;                            /* Height */
    s16b wt;                            /* Weight */
    s32b au;                            /* Current Gold */
    s16b max_depth;                     /* Max depth */
    s16b depth;                         /* Cur depth */
    s16b max_lev;                       /* Max level */
    s16b lev;                           /* Level */
    s32b max_exp;                       /* Max experience */
    s32b exp;                           /* Cur experience */
    u16b exp_frac;                      /* Cur exp frac (times 2^16) */
    s16b mhp;                           /* Max hit pts */
    s16b chp;                           /* Cur hit pts */
    u16b chp_frac;                      /* Cur hit frac (times 2^16) */
    s16b msp;                           /* Max mana pts */
    s16b csp;                           /* Cur mana pts */
    u16b csp_frac;                      /* Cur mana frac (times 2^16) */
    s16b stat_max[STAT_MAX];            /* Current "maximal" stat values */
    s16b stat_cur[STAT_MAX];            /* Current "natural" stat values */
    s16b *timed;                        /* Timed effects */
    s16b word_recall;                   /* Word of recall counter */
    s16b deep_descent;                  /* Deep Descent counter */
    s32b energy;                        /* Current energy */
    s16b food;                          /* Current nutrition */
    byte confusing;                     /* Glowing hands */
    byte searching;                     /* Currently searching */
    byte unignoring;                    /* Player doesn't hide ignored items */
    byte *spell_flags;                  /* Spell flags */
    byte *spell_order;                  /* Spell order */
    s16b player_hp[PY_MAX_LEVEL];       /* HP Array */
    char died_from[NORMAL_WID];         /* Cause of death */
    char history[N_HIST_LINES][N_HIST_WRAP];
    u16b total_winner;                  /* Total winner */
    byte noscore;                       /* Cheating flags */
    bool is_dead;                       /* Player is dead */
    s16b stat_birth[STAT_MAX];          /* Birth "natural" stat values */

    /* Variable and calculatable player state */
    struct player_state state;
    struct player_state known_state;

    /* Tracking of various temporary player-related values */
    struct player_upkeep *upkeep;

    struct object *gear;
    struct player_body body;

    /*** Angband global variables (tied to the player in MAngband) ***/

    byte run_cur_dir;       /* Direction we are running */
    byte run_old_dir;       /* Direction we came from */
    bool run_open_area;     /* Looking for an open area */
    bool run_break_right;   /* Looking for a break (right) */
    bool run_break_left;    /* Looking for a break (left) */

    /*
     * The array used to store stacked monster messages
     */
    monster_race_message *mon_msg;
    monster_message_history *mon_message_hist;

    /*
     * The current size of that array
     */
    u16b size_mon_msg;
    u16b size_mon_hist;

    /*** MAngband common fields ***/

    hturn game_turn;                            /* Number of game turns */
    hturn player_turn;                          /* Number of player turns (including resting) */
    hturn active_turn;                          /* Number of active player turns */
    bool* obj_aware;                            /* Is the player aware of this obj type? */
    bool* obj_tried;                            /* Has the player tried this obj type? */
    char name[NORMAL_WID];                      /* Nickname */
    char pass[NORMAL_WID];                      /* Password */
    s32b id;                                    /* Unique ID to each player */
    s16b ghost;                                 /* Are we a ghost */
    byte lives;                                 /* Number of times we have resurrected */
    s16b world_x;                               /* The wilderness x coordinate */
    s16b world_y;                               /* The wilderness y coordinate */
    byte party;                                 /* The party he belongs to (or 0 if neutral) */
    struct player_death_info death_info;        /* Original cause of death */
    u16b retire_timer;                          /* The number of minutes this guy can play until retired. */
    byte wild_map[MAX_WILD / 8];                /* The wilderness we have explored */
    byte* art_info;                             /* Artifacts player has encountered */

    struct player_cave *cave;                   /* The player's current cave grid info */

    /*** MAngband temporary fields ***/

    int conn;                       /* Connection number */
    char hostname[NORMAL_WID];      /* His hostname */
    char addr[NORMAL_WID];          /* His IP address */
    unsigned int version;           /* His version */
    hostile_type *hostile;          /* List of players we wish to attack */
    char savefile[MSG_LEN];         /* Name of the savefile */
    bool alive;                     /* Are we alive */
    s16b recall_depth;              /* Which depth to recall to */
    cave_view_type* hist_flags[N_HISTORY_FLAGS];    /* Player's sustains/resists/flags */
    struct actor cursor_who;        /* Who's tracked by cursor */
    byte special_file_type;         /* Type of info browsed by this player */
    bitflag (*mflag)[MFLAG_SIZE];   /* Temporary monster flags */
    byte *mon_det;                  /* Were these monsters detected by this player? */
    bitflag pflag[MAX_PLAYERS][MFLAG_SIZE]; /* Temporary monster flags (players) */
    byte play_det[MAX_PLAYERS];     /* Were these players detected by this player? */
    byte *d_attr;
    char *d_char;
    byte (*f_attr)[LIGHTING_MAX];
    char (*f_char)[LIGHTING_MAX];
    byte (*t_attr)[LIGHTING_MAX];
    char (*t_char)[LIGHTING_MAX];
    byte *k_attr;
    char *k_char;
    byte *r_attr;
    char *r_char;
    byte gf_attr[GF_MAX][BOLT_MAX];
    char gf_char[GF_MAX][BOLT_MAX];
    byte use_graphics;
    byte screen_cols;
    byte screen_rows;
    byte tile_wid;
    byte tile_hgt;
    bool tile_distorted;
    s16b offset_y;
    s16b offset_x;
    s16b offset_y_old;
    s16b offset_x_old;
    cave_view_type **scr_info;
    cave_view_type **trn_info;
    char msg_log[MAX_MSG_HIST][NORMAL_WID]; /* Message history log */
    s16b msg_hist_ptr;                      /* Where will the next message be stored */
    byte last_dir;                          /* Last direction moved (used for swapping places) */
    s16b current_spell;                     /* Current values */
    s16b current_item;
    s16b current_action;
    s16b current_value;
    s16b current_selling;
    s16b current_sell_amt;
    int current_sell_price;
    int current_house;                      /* Which house is he pointing */
    int store_num;                          /* What store this guy is in */
    int player_store_num;                   /* What player store this guy is in */
    s16b delta_floor_item;                  /* Player is standing on.. */
    s16b msg_hist_dupe;                     /* Count duplicate messages for collapsing */
    u32b dm_flags;                          /* Dungeon Master Flags */
    u16b msg_last_type;                     /* Last message type sent */
    u16b main_channel;                      /* Main chat channel the player is in */
    char second_channel[NORMAL_WID];        /* Where his legacy 'privates' are sent */
    byte *on_channel;                       /* Listening to what channels */
    cave_view_type info[MAX_TXT_INFO][NORMAL_WID];
    s16b info_y;
    s16b info_x;
    s16b last_info_line;
    byte remote_term;
    bool bubble_checked;                    /* Have we been included in a time bubble check? */
    hturn bubble_change;                    /* Server turn we last changed colour */
    byte bubble_colour;                     /* Current warning colour for slow time bubbles */
    int arena_num;                          /* What arena this guy is in */
    u32b window_flag;
    bool prevents[128];                     /* Cache of "^" inscriptions */
    s16b feeling;                           /* Most recent feeling */
    s16b interactive_line;                  /* Which line is he on? */
    char *interactive_file;                 /* Which file is he reading? */
    s16b interactive_next;                  /* Which line is he on 'in the file' ? */
    s16b interactive_size;                  /* Total number of lines in file */
    char interactive_hook[26][32];          /* Sub-menu information */

    /* Targeting */
    bool target_set;                        /* Is the target set? */
    struct actor target_who;                /* Current monster (or player) being tracked */
    int target_x;                           /* Target location */
    int target_y;
    bool tt_flag;                           /* Interesting grids */
    s16b tt_m;                              /* Current index */
    s16b tt_x;                              /* Current location */
    s16b tt_y;
    struct object *tt_o;                    /* Current object */
    byte tt_step;                           /* Current step */
    bool tt_help;                           /* Display info/help */

    /*** PWMAngband common fields ***/

    player_other other;                 /* More player information */
    struct quest quest;                 /* Current quest */
    char died_flavor[160];              /* How this guy died */
    s16b tim_mimic_what;                /* Rogue flag */
    struct monster_lore *lore;          /* Monster lore */
    struct monster_race *poly_race;     /* Monster race (mimic form) */
    s16b k_idx;                         /* Object kind index (mimic form) */
    byte *randart_info;                 /* Randarts player has encountered */
    byte *randart_created;              /* Randarts player has created */
    byte *spell_power;                  /* Spell power array */
    struct history_info *history_list;  /* Character event history */
    s16b history_ctr;                   /* Index of first writable entry */
    s16b history_size;                  /* Current size of history list */
    byte *kind_ignore;                  /* Ignore this object kind */
    byte *kind_everseen;                /* Has the player seen this object kind? */
    byte **ego_ignore_types;            /* Table for ignoring by ego and type */
    byte *ego_everseen;                 /* Has the player seen this ego type? */
    hturn quit_turn;                    /* Turn this player left the game */
    struct bow_brand brand;             /* Archer flags */

    /*** PWMAngband temporary fields ***/

    s32b esp_link;              /* Mind flags */
    byte esp_link_type;
    s16b spell_cost;            /* Total cost for spells */
    byte ignore;                /* Player has auto-ignore activated */
    struct monster_lore current_lore;
    bool starving;              /* True if player is starving */
    int flow_save;              /* Monster flow */
    byte max_hgt;               /* Max client screen height */
    cave_view_type **info_icky; /* Info is icky */
    s16b last_info_line_icky;
    char *header_icky;
    s16b screen_save_depth;     /* Depth of the screen_save() stack */
    bool was_aware;             /* Is the player aware of the current obj type? */
    s16b current_sound;         /* Current sound */
    hturn object_last_wield;
    s32b charge;                /* Charging energy */
    bool has_energy;            /* Player has energy */
    bool is_idle;               /* Player is idle */
    bool full_refresh;          /* Full refresh (includes monster/object lists) */
    byte search_request;
    byte digging_request;
    byte digging_dir;
    bool shimmer;               /* Hack -- optimize multi-hued code (players) */
    bool delayed_display;       /* Hack -- delay messages after character creation */
    bool did_visuals;           /* Hack -- projection indicator (visuals) */
    bool is_afraid;             /* Player is afraid */
    s16b old_py;                /* Previous player location */
    s16b old_px;
    bool path_drawn;            /* NPP's visible targeting */
    int path_n;
    struct loc path_g[256];
    bool can_study_book;        /* Player carries a book with spells they can study */
    byte slaves;                /* Number of controlled monsters */
    char tempbuf[NORMAL_WID];
    s16b obj_feeling;           /* Object/monster feeling (for display) */
    s16b mon_feeling;
    bool ladder;                /* Hack -- add online ladder info to dump character */

    /*
     * In order to prevent the regeneration bonus from the first few turns, we have
     * to store the number of turns the player has rested. Otherwise, the first
     * few turns will have the bonus and the last few will not.
     */
    int player_turns_rested;
    bool player_rest_disturb;

    /* Shared monster/object list instances */
    void *monster_list_subwindow;
    void *object_list_subwindow;
};

#endif /* INCLUDED_PLAYER_COMMON_H */
