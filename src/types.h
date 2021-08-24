/* File: types.h */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Purpose: global type declarations */


/*
 * This file should ONLY be included by "angband.h"
 */

/*
 * Note that "char" may or may not be signed, and that "signed char"
 * may or may not work on all machines.  So always use "s16b" or "s32b"
 * for signed values.  Also, note that unsigned values cause math problems
 * in many cases, so try to only use "u16b" and "u32b" for "bit flags",
 * unless you really need the extra bit of information, or you really
 * need to restrict yourself to a single byte for storage reasons.
 *
 * Also, if possible, attempt to restrict yourself to sub-fields of
 * known size (use "s16b" or "s32b" instead of "int", and "byte" instead
 * of "bool"), and attempt to align all fields along four-byte words, to
 * optimize storage issues on 32-bit machines.  Also, avoid "bit flags"
 * since these increase the code size and slow down execution.  When
 * you need to store bit flags, use one byte per flag, or, where space
 * is an issue, use a "byte" or "u16b" or "u32b", and add special code
 * to access the various bit flags.
 *
 * Many of these structures were developed to reduce the number of global
 * variables, facilitate structured program design, allow the use of ascii
 * template files, simplify access to indexed data, or facilitate efficient
 * clearing of many variables at once.
 *
 * Certain data is saved in multiple places for efficient access, currently,
 * this includes the tval/sval/weight fields in "object_type", various fields
 * in "header_type", and the "m_idx" and "o_idx" fields in "cave_type".  All
 * of these could be removed, but this would, in general, slow down the game
 * and increase the complexity of the code.
 */

typedef struct {
    cptr name;
    cptr desc;
} name_desc_t, *name_desc_ptr;

/* Effects are used for activations on objects (e.g. DSM) as well as devices (wand, rod, staff) */
struct effect_s
{
    s16b type;         /* See effect_e in defines.h. */
    byte power;        /* Power Level (1-100): Determines damage, etc */
    byte difficulty;   /* Difficulty Level (1-100): Determines fail rate. */
                       /* Think of power as the player level and difficulty as the spell level. */
    s16b cost;         /* Timeout for activations. SP for devices */
    s16b extra;        /* Optionally override normal damage calc. See do_effect for details on a per effect basis. */
};
typedef struct effect_s effect_t;

struct counts_s
{
    s32b generated;
    s32b found;
    s32b bought;
    s32b used;
    s32b destroyed;
};
typedef struct counts_s counts_t, *counts_ptr;

typedef struct {
    s32b found;
    s32b selling;
    s32b buying;
    s32b services;
    s32b winnings;
    s32b stolen;
} gold_counts_t;

/*
 * Information about object "kinds", including player knowledge.
 *
 */

typedef struct object_kind object_kind;

struct object_kind
{
    u32b idx;
    cptr name;
    cptr text;
    cptr flavor_name;

    byte tval;            /* Object type */
    byte sval;            /* Object sub type */
    byte stack;           /* Max Stack Size (0 => use tv_info_t settings) */

    s16b pval;            /* Object extra info */

    s16b to_h;            /* Bonus to hit */
    s16b to_d;            /* Bonus to damage */
    s16b to_a;            /* Bonus to armor */

    s16b ac;            /* Base armor */

    byte dd, ds;        /* Damage dice/sides for melee weapons */
    s16b mult;          /* Damage multiplier (scaled by 100) for bows */

    s16b weight;        /* Weight */

    s32b cost;            /* Object "base cost" */

    u32b flags[OF_ARRAY_SIZE];    /* Flags */

    u32b gen_flags;        /* flags for generate */
    byte stack_chance;
    byte stack_dice, stack_sides;

    byte locale[4];        /* Allocation level(s) */
    byte chance[4];        /* Allocation chance(s) */

    byte level;            /* Level */
    byte max_level;        /* Level */

    effect_t activation;
    cptr     activation_msg;

    byte d_attr;        /* Default object attribute */
    byte d_char;        /* Default object character */


    byte x_attr;        /* Desired object attribute */
    byte x_char;        /* Desired object character */


    s16b flavor;        /* Special object flavor (or zero) */

    bool easy_know;        /* This object is always known (if aware) */


    bool aware;            /* The player is "aware" of the item's effects */
    bool tried;            /* The player has "tried" one of the items */
    counts_t counts;
};



typedef struct artifact_type artifact_type;

struct artifact_type
{
    sym_t id;
    cptr name;
    cptr text;

    byte tval;            /* Artifact type */
    byte sval;            /* Artifact sub type */

    s16b pval;            /* Artifact extra info */

    s16b to_h;            /* Bonus to hit */
    s16b to_d;            /* Bonus to damage */
    s16b to_a;            /* Bonus to armor */

    s16b ac;            /* Base armor */

    byte dd, ds;        /* Damage when hits */
    s16b mult;          /* Damage multiplier (scaled by 100) for bows */

    s16b weight;        /* Weight */

    s32b cost;            /* Artifact "cost" */

    u32b     flags[OF_ARRAY_SIZE];       /* Artifact Flags */
    u32b     known_flags[OF_ARRAY_SIZE];

    effect_t activation;
    cptr     activation_msg;

    u32b gen_flags;        /* flags for generate */

    byte level;            /* Artifact level */
    byte rarity;        /* Artifact rarity */

    bool generated; /* Artifact has been created, but possibly yet to be discovered */
    bool found;     /* Player has found and identified this artifact */

    s16b tries;     /* Debug: Number of times the game tried to roll this art (statistics runs only) */
};


/*
 * Information about "ego-items".
 */

typedef struct ego_type ego_type;

struct ego_type
{
    u32b id;
    cptr name;
    cptr text;

    u32b type;            /* Type Flags (Bow, Weapon, Gloves, Helmet, Crown, Harp, etc)
                             Note: Due to object lore, it is very useful to share the same
                             ego type across different equipment slots. For example,
                             Elemental Protection can appear on multiple armor types. */

    byte level;           /* Minimum level */
    byte rarity;          /* Object rarity */
    byte max_level;       /* Maximum level. 0 => No restriction */

    s16b max_to_h;        /* Maximum to-hit bonus */
    s16b max_to_d;        /* Maximum to-dam bonus */
    s16b max_to_a;        /* Maximum to-ac bonus */

    byte max_pval;        /* Maximum pval */

    u32b flags[OF_ARRAY_SIZE];    /* Ego-Item Flags */
    u32b known_flags[OF_ARRAY_SIZE];
    u32b xtra_flags[OF_ARRAY_SIZE];

    u32b gen_flags;        /* flags for generate */
    effect_t activation;

    counts_t counts;

    vec_ptr  art_names;
    vec_ptr  art_names_high;
};

typedef struct object_type object_type;
typedef bool (*object_p)(object_type *o_ptr);

struct obj_loc_s {
    byte where; /* INV_* code (see inv.h) */
    union {
        int slot; /* INV_PACK, INV_EQUIP, etc */
        struct { u16b dun_id, obj_id; s16b x, y; } floor;  /* INV_FLOOR */
        struct { u16b dun_id, obj_id; u32b mon_id; } mon_pack; /* INV_MON_PACK */
    } v;
};
typedef struct obj_loc_s obj_loc_t, *obj_loc_ptr;

struct object_type
{
    s16b k_idx;            /* Kind index (zero if "dead") */

    byte tval;            /* Item type (from kind) */
    byte sval;            /* Item sub-type (from kind) */

    s16b pval;            /* Item extra-parameter */

    byte discount;        /* Discount (if any) */

    byte number;        /* Number of items */

    s16b weight;        /* Item weight in decipounds */

    sym_t art_id;
    sym_t replacement_art_id;
    sym_t race_id;         /* corpses, statues, etc */ 
    s16b name2;            /* Ego-Item type, if any */

    byte xtra1;            /* Extra info: Weaponsmith */
    byte xtra2;            /* Extra info index */
    byte xtra3;            /* Extra info: Chests and Weaponsmith. Device Power. */
    s16b xtra4;            /* Extra info: Lights, Capture, Quiver Capacity, Device MaxSP. */
    s32b xtra5;            /* Extra info: Device CSP */

    s16b to_h;            /* Plusses to hit */
    s16b to_d;            /* Plusses to damage */
    s16b to_a;            /* Plusses to AC */

    s16b ac;            /* Normal AC */

    byte dd, ds;        /* Damage dice/sides */
    s16b mult;          /* Damage multiplier (scaled by 100) for bows */

    s16b timeout;        /* Timeout Counter */

    byte ident;            /* Special flags  */

    u32b marked;        /* Object is marked */

    u16b inscription;    /* Inscription index */
    u16b art_name;      /* Artifact name (random artifacts) */

    byte feeling;          /* Game generated inscription number (eg, pseudo-id) */

    u32b flags[OF_ARRAY_SIZE];        /* Extra Flags for ego and artifacts */

    u32b curse_flags;        /* Flags for curse */

    u32b known_flags[OF_ARRAY_SIZE];
    u32b known_curse_flags;
    u32b known_xtra;

    u32b rune;

    object_type *next;  /* piles (floor or monster) */

    effect_t activation;
    obj_loc_t loc;

    s16b level;         /* object level on generation for my statistical pleasures */
    int  scratch;
};
#define object_is_(O, T, S) ((O) && (O)->tval == (T) && (O)->sval == (S))

/*
 * Monster "race" information, including racial memories
 *
 * Note that "d_attr" and "d_char" are used for MORE than "visual" stuff.
 *
 * Note that "x_attr" and "x_char" are used ONLY for "visual" stuff.
 *
 * Note that "cur_num" (and "max_num") represent the number of monsters
 * of the given race currently on (and allowed on) the current level.
 * This information yields the "dead" flag for Unique monsters.
 *
 * Note that "max_num" is reset when a new player is created.
 * Note that "cur_num" is reset when a new level is created.
 *
 * Note that several of these fields, related to "recall", can be
 * scrapped if space becomes an issue, resulting in less "complete"
 * monster recall (no knowledge of spells, etc).  All of the "recall"
 * fields have a special prefix to aid in searching for them.
 */

#define SKILL_DESC_LEN 50
typedef struct {
    char dis[SKILL_DESC_LEN];
    char dev[SKILL_DESC_LEN];
    char sav[SKILL_DESC_LEN];
    char stl[SKILL_DESC_LEN];
    char srh[SKILL_DESC_LEN];
    char fos[SKILL_DESC_LEN];
    char thn[SKILL_DESC_LEN];
    char thb[SKILL_DESC_LEN];
} skills_desc_t, *skills_desc_ptr;

/*
 * Information about "skill"
 */

typedef struct skill_table skill_table;

struct skill_table
{
    s16b w_start[5][64];      /* start weapon exp */
    s16b w_max[5][64];        /* max weapon exp */
    s16b s_start[10];      /* start skill */
    s16b s_max[10];           /* max skill */
};


typedef struct monster_type monster_type;
enum {
    SM_REFLECTION = 32,
    SM_FREE_ACTION,
    SM_CLONED,
    /* XXX The following are abuses, totally unrelated to "smart learn" */
    SM_PET,
    SM_FRIENDLY,
    SM_SUMMONED,
    SM_TEMP_PET,   /* only until T_MASK_CHARM wears off */
    SM_TEMP_FRIENDLY, /* only while T_BLESS_FRIENDSHIP is audible */
    SM_MAX
};
#define SM_ARRAY_SIZE 2

struct monster_type
{
    u32b id;
    u32b parent_id;  /* XXX parent_id might 'dangle' if parent is slain or leaves level (gc) */

    mon_race_ptr race;
    mon_race_ptr apparent_race; /* generally same as race: cf Shadower and Tanuki */

    dun_ptr      dun;
    point_t      pos;

    mon_pack_ptr pack;

    s16b align;     /* same as race->align, except neutral summons align with master for ai purposes */
    s16b mspeed;    /* Monster "speed" */
    bool ml;        /* Monster is "visible" */


    /* monster ai (cf mon_ai.c) */
    u32b    target_id;  /* fighting another monster (0 => go after plr) */
    point_t target_pos; /* lured to a specific location (illusionist) */
    point_t last_enemy_pos; /* used for blinded monsters (can be plr's last known position) */

    s16b hp;        /* Current Hit points */
    s16b maxhp;        /* Max Hit points */
    s16b max_maxhp;        /* Max Max Hit points */
    s16b pain;
    s16b ac_adj;
    s16b mpower;    /* Monster power scales various things like melee skill, damage, AC, etc.
                       This field is a per mill value, just like plr->clp */

    mon_tim_ptr timers;

    s16b energy_need;    /* Monster "energy" */
    s16b cdis;        /* Current dis from player */

    u32b mflag;        /* Extra monster flags. (Note: Not saved!) */
    u32b mflag2;        /* Extra monster flags. (This one *is* saved)  */
    dun_flow_ptr flow;  /* MFLAG2_HUNTED ... Note: Wandering requires pack->flow since the hunter may become the hunted! */


    obj_ptr obj;


    u16b nickname;        /* A named pet */

    u32b exp;

    u32b smart[SM_ARRAY_SIZE]; /* smart_learn */

    byte drop_ct;
    byte stolen_ct;
    u16b turns;

    byte anti_magic_ct; /* XXX probably s/b a timer */
    byte anger;
    s16b mana;

    s16b project_amt; /* debugging: power of spell projection cf _monsters in dun_project.c */
    s16b project_dam; /* riding: actual damage taken */
    bool project_notice; /* e.g. GF_TURN_UNDEAD; no damage but still affected */
};


/*
 * An entry for the object/monster allocation functions
 *
 * Pass 1 is determined from allocation information
 * Pass 2 is determined from allocation restriction
 * Pass 3 is determined from allocation calculation
 */

typedef struct alloc_entry alloc_entry;

struct alloc_entry
{
    s16b index;        /* The actual index */

    s16b level;        /* Base dungeon level */
    s16b max_level;
    byte prob1;        /* Probability, pass 1 */
    byte prob2;        /* Probability, pass 2 */
    byte prob3;        /* Probability, pass 3 */

    u16b total;        /* Unused for now */
};



/*
 * Available "options"
 *
 *    - Address of actual option variable (or NULL)
 *
 *    - Normal Value (TRUE or FALSE)
 *
 *    - Option Page Number (or zero)
 *
 *    - Savefile Set (or zero)
 *    - Savefile Bit in that set
 *
 *    - Textual name (or NULL)
 *    - Textual description
 */

typedef struct option_type option_type;

struct option_type
{
    bool    *o_var;

    byte    o_norm;

    byte    o_page;

    byte    o_set;
    byte    o_bit;

    cptr    o_text;
    cptr    o_desc;
};


typedef struct magic_type magic_type;

struct magic_type
{
    byte slevel;        /* Required level (to learn) */
    byte smana;            /* Required mana (to cast) */
    byte sfail;            /* Minimum chance of failure */
    byte sexp;            /* Encoded experience bonus */
};

/*
 * Information about the player's "magic"
 *
 * Note that a player with a "spell_book" of "zero" is illiterate.
 */

typedef struct player_magic player_magic;

struct player_magic
{
    int spell_book;        /* Tval of spell books (if any) */
    int spell_xtra;        /* Something for later */

    int spell_stat;        /* Stat for spells (if any)  */
    int spell_type;        /* Spell type (mage/priest) */

    int spell_first;        /* Level of first spell */
    int spell_weight;        /* Weight that hurts spells */

    magic_type info[MAX_MAGIC][32];    /* The available spells */
};



/*
 * Player sex info
 */

typedef struct player_sex player_sex;

struct player_sex
{
    cptr title;            /* Type of sex */
    cptr winner;        /* Name of winner */
};

/*
 * Most of the "player" information goes here.
 *
 * This stucture gives us a large collection of player variables.
 *
 * This structure contains several "blocks" of information.
 *   (1) the "permanent" info
 *   (2) the "variable" info
 *   (3) the "transient" info
 *
 * All of the "permanent" info, and most of the "variable" info,
 * is saved in the savefile.  The "transient" info is recomputed
 * whenever anything important changes.
 */
#define MAX_HANDS 6
#define MAX_ARMS  3
#define HAND_NONE -1
#define BLOW_NONE -1

/* XXX Player melee is being refactored.  Checkout plr_attack.h */

#define NUM_BLOWS(h) (plr->attack_info[h].base_blow + plr->attack_info[h].xtra_blow)

typedef struct player_type player_type;

struct player_type
{
    s32b id;

    u16b initial_world_id; /* W_SMAUG for middle earth; W_AMBER for random world */
    u16b world_id;     /* worlds may be sequenced (W_SMAUG->W_SARUMAN->W_SAURON) */
    u16b dun_id;       /* may differ from cave->dun_id */
    point_t pos;       /* current position in dun_id */
    point_t old_pos;   /* last position in D_SURFACE for recall */
    point_t new_pos;   /* D_QUEST position: setting pos too early can break savefiles on panic_save */
    point_t last_pos;  /* D_SURFACE pursuit ai ... cf _move_hostile and process_player */
    u32b turn;

    plr_tim_ptr timers;

    byte psex;            /* Sex index */
    s16b prace;            /* Race index */
    byte pclass;        /* Class index */
    byte personality;        /* Seikaku index */
    byte realm1;        /* First magic realm */
    byte realm2;        /* Second magic realm */
    byte dragon_realm;
    byte psubclass;        /* e.g. Pacts on Warlocks. Type of Weaponmaster.*/
    byte psubrace;      /* e.g. Parentage on Demigods */
    sym_t current_r_idx;


    u16b expfact;

    s32b au;            /* Current Gold */
    s16b fame;

    s32b max_max_exp;    /* Max max experience (only to calculate score) */
    s32b max_exp;        /* Max experience */
    s32b exp;            /* Cur experience */
    u32b exp_frac;        /* Cur exp frac (times 2^16) */

    s16b lev;            /* Level */

    s32b mmhp;           /* Max Max hit pts */
    s32b mhp;            /* Max hit pts */
    s32b chp;            /* Cur hit pts */
    u32b chp_frac;       /* Cur hit frac (times 2^16) */

    s32b msp;            /* Max mana pts */
    s32b csp;            /* Cur mana pts */
    u32b csp_frac;       /* Cur mana frac (times 2^16) */

    s16b clp;            /* Cur life pts (per mill) */

    s16b max_plv;        /* Max Player Level */

    s16b stat_max[6];    /* Current "maximal" stat values */
    s16b stat_max_max[6];    /* Maximal "maximal" stat values */
    s16b stat_cur[6];    /* Current "natural" stat values */

    s16b learned_spells;
    s16b add_spells;

    u32b count;

    s16b afraid;        /* Timed -- Fear */
    s16b mimic_form;
    s16b tim_mimic;

    bool spell_turned;
    bool fasting;
    s16b free_turns;
    bool sense_artifact;
    who_t duelist_target;
    u32b duelist_target_idx; /* XXX for savefile loads ... read before monster exists */
    who_t target;
    who_t pet_target;
    who_t riding_target;

    bool return_ammo;
    bool painted_target;
    u32b painted_target_idx;
    int  painted_target_ct;
    bool easy_2weapon;
    bool speciality_equip;
    bool cleave;
    bool vorpal;
    bool whirlwind;
    s16b elaborate_defense;
    s16b cloak_of_shadows;
    bool lightning_reflexes;
    bool clear_mind;

    s16b ambush;     /* Non-zero allows ambush on sleeping monster. Pct damage multiplier */
    s16b backstab;   /* Non-zero allows backstab on fleeing monster. Pct damage multiplier */
    s16b shoot_sleeping;
    s16b shoot_fleeing;
    bool peerless_stealth;
    s16b open_terrain_ct;

    s16b entrench_x;
    s16b entrench_y;
    s16b entrench_ct;
    bool entrenched;
    bool inven_prot;
    bool quick_walk;

    /* Rune Knight: Some Rune effects might become general game mechanics, like Magic Resistance
       and Magic Absorption.  Also, let's consolidate the White Aura mutation with the Rune of
       Good Fortune into a cached Good Luck value */
    s16b magic_resistance;
    bool good_luck;

    bool rune_elem_prot;

    s16b chaos_patron;
    u32b muta[MUT_FLAG_SIZE];
    u32b muta_lock[MUT_FLAG_SIZE];   /* Mutations that can not be removed! */
    s16b demigod_power[MAX_DEMIGOD_POWERS];
    s16b draconian_power;

    s16b virtues[8];
    s16b vir_types[8];

    s16b energy_need;      /* Energy needed for next move */

    s16b food;          /* Current nutrition */

    u32b special_attack;      /* Special attack capacity -LM- */
    u32b special_defense;      /* Special block capacity -LM- */
    byte action;          /* Currently action */

    u32b spell_learned1;      /* bit mask of spells learned */
    u32b spell_learned2;      /* bit mask of spells learned */
    u32b spell_worked1;      /* bit mask of spells tried and worked */
    u32b spell_worked2;      /* bit mask of spells tried and worked */
    u32b spell_forgotten1;      /* bit mask of spells learned but forgotten */
    u32b spell_forgotten2;      /* bit mask of spells learned but forgotten */
    byte spell_order[64];      /* order spells learned/remembered/forgotten */

    s16b spell_exp[64];       /* Proficiency of spells */
    s16b weapon_exp[5][64];   /* Proficiency of weapons */
    s16b skill_exp[10];       /* Proficiency of misc. skill */
    s16b spells_per_round;    /* 175 = 1.75 spells per round, etc. Calculated in calc_bonuses(). Only works for book casters (do_cmd_cast) at the moment. */

    s32b magic_num1[MAX_MAGIC_NUM];     /* Array for non-spellbook type magic */
    byte magic_num2[MAX_MAGIC_NUM];     /* Flags for non-spellbook type magics */

    s16b concent;      /* Sniper's concentration level */

    s16b player_hp[PY_MAX_LEVEL];
    char died_from[80];         /* What killed the player */
    cptr last_message;        /* Last message on death or retirement */

    u16b total_winner;      /* Total winner */
    u16b panic_save;      /* Panic save */

    u16b noscore;          /* Cheating flags */

    bool is_dead;          /* Player is dead */

    bool wizard;          /* Player is in wizard mode */

    u32b riding;              /* Riding on a monster of this index */
    byte knowledge;           /* Knowledge about yourself */

    s16b start_race;          /* Race at birth */
    s32b old_race1;           /* Record of race changes */
    s32b old_race2;           /* Record of race changes */
    s16b old_realm;           /* Record of realm changes */

    s16b pet_follow_distance; /* Length of the imaginary "leash" for pets */
    s16b pet_extra_flags;     /* Various flags for controling pets */

    sym_t today_mon;           /* Wanted monster */

    bool autopick_autoregister; /* auto register is in-use or not */

    /*** Temporary fields ***/

    bool playing;            /* True if player is playing */
    bool leaving;            /* True if player is leaving */

    u32b health_who;    /* Health bar trackee */

    sym_t monster_race_idx;    /* Monster race trackee */

    s16b object_kind_idx;    /* Object kind trackee */

    s16b new_spells;    /* Number of spells available */
    s16b old_spells;

    s16b old_food_aux;    /* Old value of food */

    bool old_cumber_armor;
    bool old_cumber_glove;
    bool old_heavy_wield[MAX_HANDS];
    bool old_heavy_shoot;
    bool old_icky_wield[MAX_HANDS];
    bool old_riding_wield[MAX_HANDS];
    bool old_riding_ryoute;
    bool old_monlite;

    s16b old_lite;        /* Old radius of lite (if any) */

    bool cumber_armor;    /* Mana draining armor */
    int  cumber_armor_amt;
    bool cumber_glove;    /* Mana draining gloves */
    bool riding_ryoute;    /* Riding weapon */
    bool monlite;

    s16b cur_light;        /* Radius of lightsource (if any) */


    u32b notice;        /* Special Updates (bit flags) */
    u32b update;        /* Pending Updates (bit flags) */
    u32b redraw;        /* Normal Redraws (bit flags) */
    u32b window;        /* Window Redraws (bit flags) */

    s16b stat_use[6];    /* Current modified stats */
    s16b stat_top[6];    /* Maximal modified stats */

    bool sutemi;
    bool counter;

    s32b align;                /* Good/evil/neutral */
    point_t run_pos;


    /*** Extracted fields ***/
    s16b stat_add[6];    /* Modifiers to stat values */
    s16b stat_ind[6];    /* Indexes into stat tables */

    s16b resist[GF_RES_COUNT];
    s16b life;
    s16b xtra_hp;

    bool reflect;
    bool sh_fire;
    bool sh_elec;
    bool sh_cold;
    bool sh_shards;
    bool sh_retaliation;
    bool sh_fear;
    bool sh_holy;
    bool revenge;  /* Eye for an Eye inflicts immediate revenge: T_REVENGE and T_HEX_REVENGE */
    bool innocence; /* Cloak of Innocence and Chant of Peace */

    bool repel_evil;    /* T_PROT_EVIL */
    bool repel_good;
    bool repel_monsters;

    bool no_eldritch;
    bool no_cut;
    bool no_slow;
    bool no_passwall_dam;
    bool no_charge_drain;
    bool melt_armor;

    bool anti_magic;    /* Anti-magic */
    bool res_magic;
    bool vuln_magic;
    bool anti_tele;     /* Prevent teleportation */
    bool stealthy_snipe; /* MUT_PEERLESS_SNIPER and scout's Stealthy Snipe */
    bool nimble_dodge;

    /* the following plr abilities attempt to block various monster actions */
    bool block_summon;
    bool block_multiply;
    bool block_teleport;
    bool block_magic;
    bool block_steal;

    bool sustain_str;    /* Keep strength */
    bool sustain_int;    /* Keep intelligence */
    bool sustain_wis;    /* Keep wisdom */
    bool sustain_dex;    /* Keep dexterity */
    bool sustain_con;    /* Keep constitution */
    bool sustain_chr;    /* Keep charisma */

    u32b cursed;         /* Player is cursed */

    bool can_swim;       /* No damage falling */
    bool levitation;     /* No damage falling */
    bool weak_lite;      /* Permanent light (weak) */
    s16b self_lite;      /* Permanent light or dark */
    s16b free_act;       /* Resist paralysis; perhaps slowing */
    s16b see_inv;        /* Can see invisible */
    s16b regen;          /* Rate of regeneration: 100 = 100%, 200 = 200%, etc. */
    s16b hold_life;      /* Resist life draining */

    bool auto_id;
    bool auto_pseudo_id;
    int  auto_id_sp;
    bool cult_of_personality;
    bool fairy_stealth;

    bool telepathy;      /* Telepathy */
    bool esp_animal;
    bool esp_undead;
    bool esp_demon;
    bool esp_orc;
    bool esp_troll;
    bool esp_giant;
    bool esp_dragon;
    bool esp_human;
    bool esp_evil;
    bool esp_good;
    bool esp_nonliving;
    bool esp_unique;
    bool esp_magical;
    bool esp_living;
    bool wizard_sight;

    bool slow_digest;    /* Slower digestion */
    bool pass_wall;     /* Permanent wraithform */
    bool kill_wall;
    bool pass_web;
    bool clear_web;
    bool pass_tree;     /* No energy penalty when moving thru the forest */
    bool centaur_armor; /* Centaurs|Driders can only use the breast plate portion of body armor */
    s16b dec_mana;
    s16b spell_power;
    s16b device_power;
    s16b spell_cap;
    s16b easy_spell;
    bool heavy_spell;
    bool warning;
    bool mighty_throw;
    byte see_nocto;        /* Noctovision */
    bool easy_capture;

    byte easy_realm1;   /* Magic Stones give realm specific boosts */

    bool move_random;   /* Cyberdemons and Possessors ... */

    s16b monk_lvl;
    cptr monk_tbl;

    vec_ptr           innate_blows; /* vec<mon_blow_ptr> (cf PU_INNATE) */

    int               weapon_ct;
    plr_attack_info_t attack_info[MAX_HANDS];
    plr_attack_info_t innate_attack_info;
    plr_shoot_info_t  shooter_info;

    s16b dis_to_a;        /* Known bonus to ac */

    s16b dis_ac;        /* Known base ac */

    s16b to_h_m;            /* Bonus to hit (misc) */
    s16b to_d_m;            /* Bonus to dam (misc) */
    s16b to_a;            /* Bonus to ac */
    s16b bonus_to_a;   /* bless < stone skin < ult. (plr_bonus_ac) */
    s16b bonus_speed;  /* quicken vs !Speed vs psion */

    s16b to_d_spell;

    s16b to_m_chance;        /* Minusses to cast chance */

    bool ryoute;

    int birth_mutation;

    s16b ac;            /* Base ac */
    s16b ac_adj;        /* XXX ac is forced non-negative, but duelist needs to know about this XXX */

    s16b see_infra;        /* Infravision range */

    skills_t skills;

    s16b skill_tht;        /* Skill: To hit (throwing) */
    s16b skill_dig;        /* Skill: Digging */

    s16b pspeed;        /* Current speed */
    u32b pflag;
    bool cave_no_regen; /* process_world_aux_hp_and_sp */
    s16b project_dam;   /* riding: amount of damage taken by a spell projection */
};


/*
 * A structure to hold "rolled" information
 */
typedef struct birther birther;

struct birther
{
    s16b initial_world_id;
    byte game_mode;
    byte psex;         /* Sex index */
    s16b prace;        /* Race index */
    byte psubrace;
    byte pclass;       /* Class index */
    byte psubclass;       /* Subclass index */
    byte personality;     /* Seikaku index */
    byte realm1;       /* First magic realm */
    byte realm2;       /* Second magic realm */
    byte dragon_realm;

    s32b au;

    s16b stat_max[6];        /* Current "maximal" stat values */
    bool quick_ok;
};


typedef struct kamae kamae;

struct kamae
{
    cptr    desc;       /* A verbose kamae description */
    int     min_level;  /* Minimum level to use */
    cptr    info;
};

typedef bool (*monster_hook_type)(int r_idx);


/*
 * This seems like a pretty standard "typedef"
 */
typedef int (*inven_func)(object_type *);

/*
 *  A structure type for entry of auto-picker/destroyer
 */
typedef struct {
    cptr name;          /* Items which have 'name' as part of its name match */
    cptr insc;          /* Items will be auto-inscribed as 'insc' */
    u32b flag[2];       /* Misc. keyword to be matched */
    byte action;        /* Auto-pickup or Destroy or Leave items */
    byte dice;          /* Weapons which have more than 'dice' dice match */
    byte bonus;         /* Items which have more than 'bonus' magical bonus match */
    byte level;
    byte weight;
    int  value;
} autopick_type;

/*
 *  A structure type for travel command
 */
#define TRAVEL_MODE_NORMAL   0
#define TRAVEL_MODE_AMMO     1
#define TRAVEL_MODE_AUTOPICK 2
typedef struct {
    point_t pos;
    int mode;
    int run;
    /* travel either by a direct path (This is an optimization for "auto_get") */
    point_vec_ptr path;
    int path_idx;
    /* or by a more expensive "flow" (Calculating this on D_SURFACE can be expensive) */
    dun_flow_ptr flow;
    int dir;
    point_t last_pos, temp_pos;
} travel_type;

typedef struct {
    int  id;
    cptr name;
    byte color;
    cptr desc;
    cptr parse;
    int  xtra;
} parse_tbl_t, *parse_tbl_ptr;

/*
 * A new spell system, and some half baked ideas for refactoring
 * Below here, is under construction, to be cleaned up later!
 */

#define SPELL_FLAG_LEARNED   0x0001
#define SPELL_FLAG_FORGOTTEN 0x0002
#define SPELL_FLAG_NOTICED   0x0004
#define SPELL_FLAG_CONFIRM   0x0008
#define SPELL_FLAG_HIDE      0x0010

struct spell_stats_s
{
    int flags;
    int ct_cast;
    int ct_fail;
    int skill;      /* CASTER_GAIN_SKILL */
    int max_skill;
    char alias;
};

typedef struct spell_stats_s  spell_stats_t;
typedef spell_stats_t        *spell_stats_ptr;

typedef void (*ang_spell)(int cmd, var_ptr res);

typedef struct {
    int level;
    int cost;
    int fail;
    ang_spell fn;
    spell_stats_ptr stats;  /* temp field for UI (choose_spell) */
    char alias;             /* temp field for UI (choose_spell) */
} spell_info;

typedef void (*ang_spell_action)(const spell_info *spell);
typedef int (*calc_fail_fn)(int fail);

typedef struct {
int            stat;
spell_info    spell;
} power_info;


/* TODO: This needs some work ... I just hacked this together for now.
   I'm shooting for a single unified interface for choosing, browsing
   and casting spells */

#define CASTER_ALLOW_DEC_MANA       0x0001 /* Wizardstaff and Mage Egos/Artifacts. cf equip.c for more details */
#define CASTER_GLOVE_ENCUMBRANCE    0x0002
#define CASTER_NO_SPELL_FAIL        0x0008
#define CASTER_USE_HP               0x0010
#define CASTER_GAIN_SKILL           0x0020
#define CASTER_USE_AU               0x0040 /* Leprechauns */
#define CASTER_SUPERCHARGE_MANA     0x0080
#define CASTER_USE_CONCENTRATION    0x0100 /* Sniper */

typedef struct {
    int max_wgt;    /* max weight before encumbrance */
    int weapon_pct; /* how much do melee weapons matter for encumbrance? */
    int enc_wgt;    /* how much weight over the max before 0sp */
} encumbrance_info;

typedef struct caster_info_s caster_info; /* XXX */
struct caster_info_s
{
    cptr magic_desc;    /* spell, mindcraft, brutal power, ninjitsu, etc */
    int  min_fail;
    encumbrance_info encumbrance;
    int  which_stat;
    int  min_level;
    u32b options;
    ang_spell_action on_fail;    /* Hallucinate, Temporal Inversion, etc. */
    ang_spell_action on_cast;    /* Blood Knights take cuts, etc. */
    u32b realm1_choices;         /* XXX replace this after realm re-write */
    u32b realm2_choices;
};

/************************************************************************
 * Player Race/Class Hooks. Because of Monster Mode, the plr_race_t needs
 * to be just as informative as plr_class_t.
 * Note: Hooks are typically named after their system point of origin.
 * For example, prt_effects() prints the player's current effect status,
 * so the hook is called prt_effects. Ditto with process_world, process_player,
 * move_player, etc. At some point, I will try for consistent system-wide
 * naming ...
 ************************************************************************/
typedef void (*birth_f)(void);
typedef int  (*birth_ui_f)(doc_ptr doc);
typedef void (*register_timers_f)(void);
typedef void (*load_f)(savefile_ptr file);
typedef void (*save_f)(savefile_ptr file);

typedef void (*process_world_f)(void);
typedef void (*process_player_f)(void);
typedef void (*player_action_f)(void);
typedef void (*move_player_f)(void);
typedef void (*move_monster_f)(mon_ptr mon);
typedef void (*kill_monster_f)(mon_ptr mon);
typedef int  (*obj_alloc_f)(obj_kind_ptr kind, int prob);
typedef bool (*auto_id_f)(obj_ptr obj);
typedef bool (*auto_detect_f)(void);
typedef void (*update_light_f)(void);

typedef void (*timer_on_f)(plr_tim_ptr timer);
typedef void (*timer_off_f)(plr_tim_ptr timer);

typedef void (*calc_bonuses_f)(void);
typedef void (*stats_f)(s16b stats[MAX_STATS]);
typedef void (*calc_weapon_bonuses_f)(obj_ptr obj, plr_attack_info_ptr info);
typedef void (*calc_shooter_bonuses_f)(obj_ptr obj, plr_shoot_info_ptr info_ptr);
typedef void (*flags_f)(u32b flgs[OF_ARRAY_SIZE]);
typedef status_display_t (*status_display_f)(void);

typedef caster_info* (*caster_info_f)(void);
typedef int  (*get_spells_f)(spell_info* spells, int max);

typedef void (*gain_level_f)(int new_level);
typedef void (*change_level_f)(int old_level, int new_level);
typedef void (*doc_f)(doc_ptr doc);

typedef void (*calc_innate_bonuses_f)(mon_blow_ptr blow);
typedef void (*calc_innate_attacks_f)(void);

typedef struct plr_hooks_s plr_hooks_t, *plr_hooks_ptr;
struct plr_hooks_s
{
    /* startup and savefiles */
    birth_f                 birth;          /* After py_birth() ... grant starting gear, etc */
    birth_ui_f              birth_ui;       /* Used during py_birth() ... choose a subclass */ 
    register_timers_f       register_timers;
    load_f                  load_player;
    save_f                  save_player;

    /* game processing */
    process_world_f         process_world;  /* Called every 10 game turns */
    process_player_f        process_player; /* Called from process_player ... but player take 0 or more actions per call */
    player_action_f         player_action;  /* Called once per player action, so long as the action consumes energy */
    move_player_f           move_player;    /* Called every time the player actually moves */
    move_monster_f          move_monster;   /* Called whenever a monster moves */
    kill_monster_f          kill_monster;
    obj_p                   destroy_object;
    obj_f                   get_object;
    obj_alloc_f             obj_alloc;
    auto_id_f               auto_id;
    auto_detect_f           auto_detect;
    update_light_f          update_light;

    /* timers */
    timer_on_f              timer_on;
    timer_off_f             timer_off;

    /* bonuses and status display */
    calc_bonuses_f          calc_bonuses;    /* Do flag related bonuses here ... */
    stats_f                 calc_stats;      /* ... and stat related stuff here */
    calc_weapon_bonuses_f   calc_weapon_bonuses;
    calc_shooter_bonuses_f  calc_shooter_bonuses;
    flags_f                 get_flags;
    status_display_f        status_display;
    doc_f                   prt_effects;

    /* spellcasting */
    caster_info_f           caster_info;
    get_spells_f            get_spells;
    get_spells_f            get_powers;

    /* character sheet and levelling up */
    gain_level_f            gain_level;
    change_level_f          change_level;
    doc_f                   character_dump;

    /* melee attacks */
    plr_attack_init_f       attack_init;
    mon_attack_init_f       mon_attack_init;
    calc_innate_attacks_f   calc_innate_attacks;   /* build the attacks: PU_INNATE */
    calc_innate_bonuses_f   calc_innate_bonuses;   /* tweak attacks: PU_BONUS (e.g. calculate blow->blows) */

    /* archery */
    plr_shoot_init_f        shoot_init;
};

typedef struct class_s class_t, *class_ptr;
struct class_s
{
    int         id;
    int         subid;
    cptr        name;
    cptr        subname;
    cptr        desc;
    cptr        subdesc;
    s16b        stats[MAX_STATS];
    skills_t    skills;
    skills_t    extra_skills;
    s16b        life;
    s16b        base_hp;
    s16b        exp;
    byte        pets;
    u32b        flags;

    plr_hooks_t hooks;
};


typedef struct race_s race_t, *race_ptr;
struct race_s
{
    int         id;
    int         subid;
    cptr        name;
    cptr        subname;
    cptr        desc;
    cptr        subdesc;
    s16b        stats[MAX_STATS];
    skills_t    skills;
    skills_t    extra_skills;
    s16b        life;
    s16b        base_hp;
    s16b        exp;
    s16b        infra;
    u32b        flags;
    bool        mimic;
    equip_template_ptr
                equip_template;
    int         boss_r_idx;
    s16b        pseudo_class_id; /* For the "Monster" class ... */
    s16b        shop_adjust;

    plr_hooks_t hooks;
};

typedef struct {
    int                     id;
    cptr                    name;
    cptr                    desc;
    s16b                    stats[MAX_STATS];
    skills_t                skills;
    s16b                    life;
    s16b                    exp;
    s16b                    attack;
    s16b                    breath;
    s16b                    spell_stat;
    s16b                    mana_adj;
} dragon_realm_t, *dragon_realm_ptr;

struct device_effect_info_s
{
    int      type;
    int      level;
    int      cost;
    int      rarity;
    int      max_depth;
    int      difficulty_base;
    int      difficulty_xtra;
    int      flags;
    counts_t counts;
    int      prob;
};

typedef struct device_effect_info_s  device_effect_info_t;
typedef struct device_effect_info_s *device_effect_info_ptr;

struct personality_s
{
    int            id;
    cptr           name;
    cptr           desc;
    s16b           stats[MAX_STATS];
    skills_t       skills;
    s16b           life;
    s16b           exp;
    int            flags;
    birth_f        birth;
    calc_bonuses_f calc_bonuses;
    flags_f        get_flags;
};

typedef struct personality_s personality_t, *personality_ptr;
