/*
 * File: player-types.h
 * Purpose: Global player type declarations
 */

#ifndef INCLUDED_PLAYER_TYPES_H
#define INCLUDED_PLAYER_TYPES_H

#include "spells.h"

/*
 * Number of history flags
 */
#define N_HISTORY_FLAGS (1 + A_MAX + RES_PANELS * RES_ROWS)

/* A single history event */
typedef struct
{
    u16b type;              /* Kind of history item */
    s16b dlev;              /* Dungeon level when this item was recorded */
    s16b clev;              /* Character level when this item was recorded */
    byte a_idx;             /* Artifact this item relates to */
    char name[NORMAL_WID];  /* Artifact name */
    hturn turn;             /* Turn this item was recorded on */
    char event[NORMAL_WID]; /* The text of the item */
} history_info;

/*
 * Structure for the "quests"
 */
typedef struct
{
    s16b r_idx;     /* Monster race */
    s16b cur_num;   /* Number killed */
    s16b max_num;   /* Number required */
    s16b timer;     /* Time left before quest is over */
} player_quest;

/*
 * A structure to hold class-dependent information on spells.
 */
typedef struct
{
    byte slevel;    /* Required level (to learn) */
    byte smana;     /* Required mana (to cast) */
    byte sfail;     /* Minimum chance of failure */
    byte sexp;      /* Encoded experience bonus */
} magic_type;

/*
 * Information about the player's "magic"
 *
 * Note that a player with a "spell_book" of "zero" is illiterate.
 */
typedef struct
{
    magic_type info[PY_MAX_SPELLS]; /* The available spells */
} player_magic;

/*
 * Player sex info
 */
typedef struct player_sex
{
    const char *title;     /* Type of sex */
    const char *winner;    /* Name of winner */
} player_sex;

/*
 * Player racial info
 */
struct player_race
{
    char *name;                 /* Name */
    unsigned int ridx;          /* Index */
    struct player_race *next;
    s16b r_adj[A_MAX];          /* Racial stat bonuses */
    s16b r_skills[SKILL_MAX];   /* Racial skills */
    byte r_mhp;                 /* Race hit-dice modifier */
    s16b r_exp;                 /* Race experience factor */
    byte b_age;                 /* Base age */
    byte m_age;                 /* Mod age */
    byte m_b_ht;                /* Base height (males) */
    byte m_m_ht;                /* Mod height (males) */
    byte m_b_wt;                /* Base weight (males) */
    byte m_m_wt;                /* Mod weight (males) */
    byte f_b_ht;                /* Base height (females) */
    byte f_m_ht;                /* Mod height (females)   */
    byte f_b_wt;                /* Base weight (females) */
    byte f_m_wt;                /* Mod weight (females) */
    byte infra;                 /* Infra-vision range */
    u16b choice;                /* Legal class choices */
    struct history_chart *history;
    bitflag flags[OF_SIZE];     /* Racial (object) flags */
    bitflag pflags[PF_SIZE];    /* Racial (player) flags */
};

/*  
 * Starting equipment entry
 */
struct start_item
{
    object_kind *kind;  /* Object kind */
    byte min;           /* Minimum starting amount */
    byte max;           /* Maximum starting amount */
    struct start_item *next;
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
    s16b c_adj[A_MAX];              /* Class stat modifier */
    s16b c_skills[SKILL_MAX];       /* Class skills */
    s16b x_skills[SKILL_MAX];       /* Extra skills */
    byte c_mhp;                     /* Class hit-dice adjustment */
    s16b c_exp;                     /* Class experience factor */
    bitflag pflags[PF_SIZE];        /* Class (player) flags */
    u16b max_attacks;               /* Maximum possible attacks */
    u16b min_weight;                /* Minimum weapon weight for calculations */
    u16b att_multiply;              /* Multiplier for attack calculations */
    byte spell_book;                /* Tval of spell books (if any) */
    u16b spell_stat;                /* Stat for spells (if any) */
    u16b spell_first;               /* Level of first spell */
    u16b spell_weight;              /* Weight that hurts spells */
    u32b sense_base;                /* Base pseudo-id value */
    u16b sense_div;                 /* Pseudo-id divisor */
    struct start_item *start_items; /* The starting inventory */
    player_magic spells;            /* Magic spells */
    byte attr;                      /* Class color */
};

/*
 * Some more player information
 *
 * This information is retained across player lives
 */
typedef struct
{
    char full_name[NORMAL_WID]; /* Full name */
    char base_name[NORMAL_WID]; /* Base name */
    bool opt[OPT_MAX];          /* Options */
    byte hitpoint_warn;         /* Hitpoint warning (0 to 9) */
    byte delay_factor;          /* Delay factor (0 to 255) */
    byte squelch_lvl[TYPE_MAX]; /* Auto-squelch level (0 to 6) */
} player_other;

typedef struct
{
    /*** Angband extracted fields ***/

    s16b speed;                 /* Current speed */
    s16b num_blows;             /* Number of blows x100 */
    s16b num_shots;             /* Number of shots */
    byte ammo_mult;             /* Ammo multiplier */
    byte ammo_tval;             /* Ammo variety */
    s16b stat_add[A_MAX];       /* Modifiers to stat values */
    s16b stat_ind[A_MAX];       /* Indexes into stat tables */
    s16b stat_use[A_MAX];       /* Current modified stats */
    s16b stat_top[A_MAX];       /* Maximal modified stats */
    s16b dis_ac;                /* Known base ac */
    s16b ac;                    /* Base ac */
    s16b dis_to_a;              /* Known bonus to ac */
    s16b to_a;                  /* Bonus to ac */
    s16b to_h;                  /* Bonus to hit */
    s16b dis_to_h;              /* Known bonus to hit */
    s16b to_d;                  /* Bonus to dam */
    s16b dis_to_d;              /* Known bonus to dam */
    s16b see_infra;             /* Infravision range */
    s16b skills[SKILL_MAX];     /* Skills */
    u32b noise;                 /* Derived from stealth */
    bool heavy_wield;           /* Heavy weapon */
    bool heavy_shoot;           /* Heavy shooter */
    bool icky_wield;            /* Icky weapon */
    bitflag flags[OF_SIZE];     /* Status flags from race and items */

    /*** PWMAngband extracted fields ***/

    bool cumber_shield;         /* Encumbering shield */
    s16b frac_blow;             /* Blow frac (%) */
} player_state;

struct player_cave
{
    u16b feeling_squares;   /* Keep track of how many feeling squares the player has visited */
    byte (*info)[256];
    byte (*cost)[DUNGEON_WID];
    byte (*when)[DUNGEON_WID];
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
typedef struct player
{
    /*** Angband common fields ***/

    s16b py;                /* Player location */
    s16b px;                /* Player location */
    byte psex;              /* Sex index */
    const struct player_sex *sex;
    const struct player_race *race;
    const struct player_class *clazz;
    byte hitdie;            /* Hit dice (sides) */
    s16b expfact;           /* Experience factor */
    s16b age;               /* Characters age */
    s16b ht;                /* Height */
    s16b wt;                /* Weight */
    s16b sc;                /* Social Class */
    s32b au;                /* Current Gold */
    s16b max_depth;         /* Max depth */
    s16b depth;             /* Cur depth */
    s16b max_lev;           /* Max level */
    s16b lev;               /* Level */
    s32b max_exp;           /* Max experience */
    s32b exp;               /* Cur experience */
    u16b exp_frac;          /* Cur exp frac (times 2^16) */
    s16b mhp;               /* Max hit pts */
    s16b chp;               /* Cur hit pts */
    u16b chp_frac;          /* Cur hit frac (times 2^16) */
    s16b msp;               /* Max mana pts */
    s16b csp;               /* Cur mana pts */
    u16b csp_frac;          /* Cur mana frac (times 2^16) */
    s16b stat_max[A_MAX];   /* Current "maximal" stat values */
    s16b stat_cur[A_MAX];   /* Current "natural" stat values */
    s16b timed[TMD_MAX];    /* Timed effects */
    s16b word_recall;       /* Word of recall counter */
    s16b deep_descent;      /* Deep Descent counter */
    s32b energy;            /* Current energy */
    s16b food;              /* Current nutrition */
    byte confusing;         /* Glowing hands */
    byte searching;         /* Currently searching */
    byte spell_flags[PY_MAX_SPELLS];    /* Spell flags */
    byte spell_order[PY_MAX_SPELLS];    /* Spell order */
    s16b player_hp[PY_MAX_LEVEL];       /* HP Array */
    char died_from[NORMAL_WID];         /* Cause of death */
    char history[N_HIST_LINES][N_HIST_WRAP];    /* Initial history */
    u16b total_winner;      /* Total winner */
    byte noscore;           /* Cheating flags */
    bool is_dead;           /* Player is dead */
    s16b stat_birth[A_MAX]; /* Birth "natural" stat values */
    hturn player_turn;      /* Number of player turns (including resting) */
    hturn active_turn;      /* Number of active player turns */

    /*** Angband temporary fields ***/

    bool leaving;           /* True if player is leaving */
    s16b total_weight;      /* Total weight being carried */
    s16b inven_cnt;         /* Number of items in inventory */
    s16b health_who;        /* Who's shown on the health bar */
    s16b monster_race_idx;  /* Monster race trackee */
    s16b resting;           /* Resting counter */
    bool running;           /* Are we running? */
    bool running_firststep; /* Is this our first step running? */
    byte run_cur_dir;       /* Direction we are running */
    byte run_old_dir;       /* Direction we came from */
    bool run_open_area;     /* Looking for an open area */
    bool run_break_right;   /* Looking for a break (right) */
    bool run_break_left;    /* Looking for a break (left) */
    s16b new_spells;        /* Number of spells available */
    bool cumber_armor;      /* Mana draining armor */
    bool cumber_glove;      /* Mana draining gloves */
    s16b cur_light;         /* Radius of light (if any) */

    /*
     * Bit flags for pending "special" actions to
     * carry out after the current "action",
     * such as reordering inventory, squelching, etc.
     */
    u32b notice;

    /*
     * Bit flags for recalculations needed after this "action",
     * such as HP, or visible area
     */
    u32b update;

    /*
     * Bit flags for things that have changed, and just need to be redrawn by the UI,
     * such as HP, Speed, etc.
     */
    u32b redraw;

    /* Variable and calculatable player state */
    player_state state;

    /* "Cached" quiver statistics */
    u16b quiver_size;
    u16b quiver_slots;
    u16b quiver_remainder;

    struct object *inventory;   /* Player's inventory */

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

    hturn game_turn;            /* Number of game turns */
    bool* obj_aware;            /* Is the player aware of this obj type? */
    bool* obj_tried;            /* Has the player tried this obj type? */
    char name[NORMAL_WID];      /* Nickname */
    char pass[NORMAL_WID];      /* Password */
    s32b id;                    /* Unique ID to each player */
    s16b ghost;                 /* Are we a ghost */
    byte lives;                 /* Number of times we have resurrected */
    s16b world_x;               /* The wilderness x coordinate */
    s16b world_y;               /* The wilderness y coordinate */
    byte party;                 /* The party he belongs to (or 0 if neutral) */
    struct player_death_info death_info;    /* Original cause of death */
    u16b retire_timer;          /* The number of minutes this guy can play until retired. */
    byte wild_map[MAX_WILD / 8];    /* The wilderness we have explored */
    byte* art_info;             /* Artifacts player has encountered */
    char descrip[N_HIST_LINES * N_HIST_WRAP];   /* The player's "history" in 3rd person */

    struct player_cave *cave;   /* The player's current cave grid info */

    /*** MAngband temporary fields ***/

    int conn;                   /* Connection number */
    char hostname[NORMAL_WID];  /* His hostname */
    char addr[NORMAL_WID];      /* His IP address */
    unsigned int version;       /* His version */
    hostile_type *hostile;      /* List of players we wish to attack */
    char savefile[MSG_LEN];     /* Name of the savefile */
    bool alive;                 /* Are we alive */
    s16b recall_depth;          /* Which depth to recall to */
    cave_view_type hist_flags[N_HISTORY_FLAGS][RES_COLS]; /* Player's sustains/resists/flags */
    bool new_level_flag;        /* Has this player changed depth? */
    byte new_level_method;      /* Climb up stairs, down, or teleport level? */
    s16b cursor_who;            /* Who's tracked by cursor */
    s16b view_n;                /* Array[VIEW_MAX] used by "update_view()" */
    u16b view_g[VIEW_MAX];
    u16b *temp_g;               /* Array[TEMP_MAX] used for various things */
    byte special_file_type;     /* Type of info browsed by this player */
    bool *mon_vis;              /* Can this player see these monsters? */
    bool *mon_los;
    byte *mon_det;              /* Were these monsters detected by this player? */
    byte *obj_marked;           /* Array of marked objects */
    bool play_vis[MAX_PLAYERS]; /* Can this player see these players? */
    bool play_los[MAX_PLAYERS];
    byte play_det[MAX_PLAYERS]; /* Were these players detected by this player? */
    byte *d_attr;
    char *d_char;
    byte (*f_attr)[FEAT_LIGHTING_MAX];
    char (*f_char)[FEAT_LIGHTING_MAX];
    byte *k_attr;
    char *k_char;
    byte *r_attr;
    char *r_char;
    byte gf_attr[GF_MAX][BOLT_MAX];
    char gf_char[GF_MAX][BOLT_MAX];
    byte tval_attr[128];
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
    cave_view_type scr_info[DUNGEON_HGT + ROW_MAP + 1][DUNGEON_WID + COL_MAP];
    cave_view_type trn_info[DUNGEON_HGT + ROW_MAP + 1][DUNGEON_WID + COL_MAP];
    char msg_log[MAX_MSG_HIST][NORMAL_WID];  /* Message history log */
    s16b msg_hist_ptr;          /* Where will the next message be stored */
    byte last_dir;              /* Last direction moved (used for swapping places) */
    s16b current_spell;         /* Current values */
    s16b current_item;
    s16b current_value;
    s16b current_selling;
    s16b current_sell_amt;
    int current_sell_price;
    int current_house;          /* Which house is he pointing */
    int store_num;              /* What store this guy is in */
    int player_store_num;       /* What player store this guy is in */
    s16b delta_floor_item;      /* Player is standing on.. */
    s16b msg_hist_dupe;         /* Count duplicate messages for collapsing */
    u32b dm_flags;              /* Dungeon Master Flags */
    u16b msg_last_type;         /* Last message type sent */
    u16b main_channel;          /* Main chat channel the player is in */
    char second_channel[NORMAL_WID];    /* Where his legacy 'privates' are sent */
    byte *on_channel;           /* Listening to what channels */
    cave_view_type info[MAX_TXT_INFO][NORMAL_WID];
    s16b info_y;
    s16b info_x;
    s16b last_info_line;
    byte remote_term;
    byte run_request;
    bool bubble_checked;        /* Have we been included in a time bubble check? */
    s32b bubble_speed;          /* What was our last time bubble scale factor */
    hturn bubble_change;        /* Server turn we last changed colour */
    byte bubble_colour;         /* Current warning colour for slow time bubbles */
    int arena_num;              /* What arena this guy is in */
    u32b window_flag;
    bool prevents[128];         /* Cache of "^" inscriptions */
    s16b feeling;               /* Most recent feeling */
    s16b interactive_line;      /* Which line is he on? */
    char *interactive_file;     /* Which file is he reading? */
    s16b interactive_next;      /* Which line is he on 'in the file' ? */
    s16b interactive_size;      /* Total number of lines in file */
    char interactive_hook[26][32];  /* Sub-menu information */

    /* Targeting */
    bool target_set;            /* Is the target set? */
    s16b target_who;            /* Current monster/player being tracked, or 0 */
    s16b target_x;              /* Target location */
    s16b target_y;
    bool tt_flag;               /* Interesting grids */
    s16b tt_m;                  /* Current index */
    s16b tt_x;                  /* Current location */
    s16b tt_y;
    s16b tt_o;                  /* Current object */
    byte tt_step;               /* Current step */
    bool tt_help;               /* Display info/help */

    /*** PWMAngband common fields ***/

    player_other other;             /* More player information */
    player_quest quest;             /* Current quest */
    char died_flavor[160];          /* How this guy died */
    s16b tim_mimic_what;            /* Rogue flag */
    monster_lore* lore;             /* Monster lore */
    s16b r_idx;                     /* Monster race index (mimic form) */
    s16b k_idx;                     /* Object kind index (mimic form) */
    byte* randart_info;             /* Randarts player has encountered */
    byte* randart_created;          /* Randarts player has created */
    byte spell_power[PY_MAX_SPELLS];    /* Spell power array */
    history_info *history_list;     /* Character event history */
    s16b history_ctr;               /* Index of first writable entry */
    s16b history_size;              /* Current size of history list */
    byte *kind_everseen;            /* Has the player seen this object kind? */
    byte *ego_everseen;             /* Has the player seen this ego type? */
    hturn quit_turn;                /* Turn this player left the game */

    /*** PWMAngband temporary fields ***/

    s32b esp_link;              /* Mind flags */
    byte esp_link_type;
    byte bow_brand_t;           /* Archer flags */
    s16b bow_brand_d;
    bool project_hurt;          /* Player has been hurt in project_p() */
    s16b spell_cost;            /* Total cost for spells */
    bool *mon_hurt;             /* Did this player hurt these monsters? */
    byte squelch;               /* Player has auto-squelch activated */
    byte unignoring;            /* Player doesn't hide squelchable items */
    monster_lore current_lore;
    bool starving;              /* True if player is starving */
    int flow_save;              /* Monster flow */
    byte max_hgt;               /* Max client screen height */
    cave_view_type **info_icky; /* Info is icky */
    s16b last_info_line_icky;
    char *header_icky;
    s16b screen_icky;           /* Screen is icky */
    bool was_aware;             /* Is the player aware of the current obj type? */
    s16b current_sound;         /* Current sound */
    hturn object_last_wield;
    bool is_idle;               /* Player is idle */
    bool full_refresh;          /* Full refresh (includes monster/object lists) */
    byte search_request;
    bool shimmer;               /* Hack -- Optimize multi-hued code (players) */
    bool delayed_display;       /* Hack -- Delay messages after character creation */
    bool did_visuals;           /* Hack -- Projection indicator (visuals) */
    bool is_afraid;             /* Player is afraid */
    s16b old_py;                /* Previous player location */
    s16b old_px;
    bool path_drawn;            /* NPP's visible targeting */
    int path_n;
    u16b path_g[256];
    bool can_study_book;        /* Player carries a book with spells they can study */
    byte slaves;                /* Number of controlled monsters */
} player_type;

/* Martial arts */
typedef struct
{
    const char *hit_verb;   /* A verbose attack description */
    const char *hit_extra;
    int min_level;          /* Minimum level to use */
    int chance;             /* Chance of failure (compared to player level) */
    int dd;                 /* Damage dice */
    int ds;                 /* Damage sides */
    int effect;             /* Special effects */
} martial_arts;

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
    int bonus;
    char *text;
};

struct history_chart
{
    struct history_chart *next;
    struct history_entry *entries;
    unsigned int idx;
};

#endif /* INCLUDED_PLAYER_TYPES_H */
